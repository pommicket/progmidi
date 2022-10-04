extern crate cpal;

use cpal::traits::{DeviceTrait, HostTrait, StreamTrait};
use std::io::Write;
use std::sync::Mutex;

mod midi_input;
mod soundfont;

struct Note {
	key: u8,
	req: soundfont::SamplesRequest,
	down: bool,
	kill: bool, // only used briefly
}

struct NoteInfo {
	pitch_bend: i16, // in cents
	pedal_down: bool,
	notes: Vec<Note>,
}

static NOTE_INFO: Mutex<NoteInfo> = Mutex::new(NoteInfo {
	pitch_bend: 0,
	notes: vec![],
	pedal_down: false,
});
static SOUNDFONT: Mutex<Option<soundfont::SoundFont>> = Mutex::new(None);

fn playmidi_main() -> Result<(), String> {
	let mut device_mgr = midi_input::DeviceManager::new()?;
	device_mgr.set_quiet(true);
	let devices = device_mgr.list()?;
	for (index, device) in (&devices).into_iter().enumerate() {
		print!("{:3} | ", index + 1);
		let mut first = true;
		for line in device.name.lines() {
			if !first {
				print!("    | ");
			}
			println!("{}", line);
			first = false;
		}
		println!("    -----------------");
	}
	print!("Select a device (default {}): ", devices.default + 1);
	if std::io::stdout().flush().is_err() {
		//who cares
	}

	let device_id;
	{
		let mut buf = String::new();
		std::io::stdin()
			.read_line(&mut buf)
			.expect("error reading stdin");
		let s = buf.trim();
		if s.is_empty() {
			device_id = &devices[devices.default].id;
		} else {
			match s.parse::<usize>() {
				Ok(idx) if idx >= 1 && idx <= devices.len() => {
					device_id = &devices[idx - 1].id;
				}
				_ => {
					return Err(format!("Bad device ID: {}", s));
				}
			}
		}
	}
	let mut midi_device = device_mgr
		.open(device_id)
		.expect("error opening MIDI device");

	let mut sf = soundfont::SoundFont::open("/etc/alternatives/default-GM.sf3")?;

	for i in 0..sf.preset_count() {
		println!("{}. {}", i, sf.preset_name(i).unwrap());
	}

	//sf._debug_preset_zones(125);
	//sf._debug_instrument_zones(148);

	// 	let result = playmidi_main();
	// 	if let Err(s) = result {
	// 		eprintln!("Error: {}", s);
	// 	}
	let host = cpal::default_host();
	let audio_device = host
		.default_output_device()
		.expect("no output device available");
	let supported_configs = audio_device
		.supported_output_configs()
		.expect("error while querying configs");
	let mut chosen_config = None;
	for config in supported_configs {
		if config.channels() != 2 || config.sample_format() != cpal::SampleFormat::I16 {
			continue;
		}
		chosen_config = Some(config);
	}
	let chosen_config = match chosen_config {
		None => {
			return Err("Couldn't configure audio device to have 2 16-bit channels.".to_string())
		}
		Some(x) => x,
	};
	let supp_config: cpal::SupportedStreamConfig = chosen_config.with_max_sample_rate();
	if supp_config.channels() != 2 {}
	let config = supp_config.into();
	let preset = 299;

	{
		use std::time::Instant;
		let now = Instant::now();
		sf.load_samples_for_preset(preset).expect("oh no");
		println!("Loaded in {:?}", now.elapsed());
	}

	{
		let mut sflock = SOUNDFONT.lock().expect("couldn't lock soundfont.");
		*sflock = Some(sf);
	}

	let stream = audio_device
		.build_output_stream(
			&config,
			move |data: &mut [i16], _: &cpal::OutputCallbackInfo| {
				let mut note_info = NOTE_INFO.lock().expect("couldn't lock notes.");
				let mut maybe_sf = SOUNDFONT.lock().expect("couldn't lock soundfont.");
				if let Some(sf) = maybe_sf.as_mut() {
					let sample_rate = config.sample_rate.0 as f64;
					for x in data.iter_mut() {
						*x = 0;
					}
					let pitch_bend = note_info.pitch_bend;
					for note in note_info.notes.iter_mut() {
						note.req.set_tune(pitch_bend as i32);
						match sf.add_samples_interlaced(&mut note.req, data, sample_rate) {
							Ok(true) => {}
							Ok(false) => note.kill = true,
							Err(e) => eprintln!("{}", e),
						}
					}
					note_info.notes.retain(|note| !note.kill);
				}
			},
			move |err| {
				println!("audio stream error: {}", err);
			},
		)
		.expect("couldn't build output stream");
	stream.play().expect("couldn't play stream");

	let note_falloff = 0.1; // falloff when note is released
	while midi_device.is_connected() {
		while let Some(event) = midi_device.read_event() {
			let mut note_info = NOTE_INFO
				.lock()
				.map_err(|_| "couldn't lock notes".to_string())?;
			use midi_input::Event::*;
			match event {
				NoteOn { note, vel, .. } => {
					let mut maybe_sf = SOUNDFONT.lock().expect("couldn't lock soundfont.");
					note_info.notes.retain(|n| n.key != note);
					if let Some(sf) = maybe_sf.as_mut() {
						match sf.request(preset, note, vel) {
							Ok(mut req) => {
								req.set_volume(0.1);
								note_info.notes.push(Note {
									key: note,
									req,
									down: true,
									kill: false,
								});
							}
							Err(e) => eprintln!("get samples error: {}", e),
						}
					}
				}
				NoteOff { note, .. } => {
					let pedal_down = note_info.pedal_down;
					if let Some(n) = note_info.notes.iter_mut().find(|n| n.key == note) {
						n.down = false;
						if !pedal_down {
							n.req.set_falloff(note_falloff);
						}
					}
				}
				PitchBend { amount, .. } => {
					note_info.pitch_bend = amount / 128;
				}
				ControlChange {
					controller, value, ..
				} => {
					if controller == 64 {
						// oddly, a value of 0 means "down"
						note_info.pedal_down = value < 127;
						if note_info.pedal_down {
							// disable falloff for all notes
							for note in note_info.notes.iter_mut() {
								note.req.set_falloff(1.0);
							}
						} else {
							// start falloff for all non-down notes
							for note in note_info.notes.iter_mut() {
								if !note.down {
									note.req.set_falloff(note_falloff);
								}
							}
						}
					}
				}
				_ => {}
			}
		}
		if let Some(err) = midi_device.get_error() {
			eprintln!("Error: {}", err);
			midi_device.clear_error();
		}
		std::thread::sleep(std::time::Duration::from_millis(5));
	}

	Ok(())
}

fn main() {
	if let Err(e) = playmidi_main() {
		eprintln!("Error: {:?}", e);
	}
}
