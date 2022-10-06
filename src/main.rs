extern crate cpal;
extern crate rhai;

use cpal::traits::{DeviceTrait, HostTrait, StreamTrait};
use std::io::Write;
use std::sync::Mutex;

mod midi_input;
mod soundfont;

const NOTE_FALLOFF: f32 = 0.1; // falloff when note is released
const CHANNEL_COUNT: usize = 16;

struct Note {
	key: u8,
	req: soundfont::SamplesRequest,
	down: bool,
	kill: bool, // only used briefly
}

struct NoteInfo {
	pitch_bend: i32, // in cents
	pedal_down: bool,
	presets: [usize; CHANNEL_COUNT],
	notes: [Vec<Note>; CHANNEL_COUNT],
	channel_volumes: [f32; CHANNEL_COUNT],
	master_volume: f32,
}

static NOTE_INFO: Mutex<NoteInfo> = Mutex::new(NoteInfo {
	pitch_bend: 0,
	// this is ridiculous.
	notes: [vec![], vec![], vec![], vec![], vec![], vec![], vec![], vec![], vec![], vec![], vec![], vec![], vec![], vec![], vec![], vec![]],
	pedal_down: false,
	presets: [0; CHANNEL_COUNT],
	channel_volumes: [1.0; CHANNEL_COUNT],
	master_volume: 1.0,
});
static SOUNDFONT: Mutex<Option<soundfont::SoundFont>> = Mutex::new(None);

fn get_midi_device(idx: i32) -> Result<midi_input::Device, String> {
	let mut device_mgr = midi_input::DeviceManager::new()?;
	device_mgr.set_quiet(true);
	let devices = device_mgr.list()?;
	
	let device_idx = match idx {
		0 => devices.default,
		i if i >= 1 => {
			(i - 1) as usize
		},
		_ => {
			// user selects device
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
		
			let mut buf = String::new();
			std::io::stdin()
				.read_line(&mut buf)
				.expect("error reading stdin");
			let s = buf.trim();
			if s.is_empty() {
				devices.default
			} else {
				match s.parse::<usize>() {
					Ok(idx) if idx >= 1 && idx <= devices.len() => {
						idx - 1
					}
					_ => {
						return Err(format!("Bad device ID: {}", s));
					}
				}
			}
		}
	};
	let device_id = &devices[device_idx].id;
	Ok(device_mgr.open(device_id)?)
}

fn get_audio_stream() -> Result<cpal::Stream, String> {
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
					for channel in 0..CHANNEL_COUNT {
						let volume = 0.1 * note_info.master_volume * note_info.channel_volumes[channel];
						let notes = &mut note_info.notes[channel];
						for note in notes.iter_mut() {
							note.req.set_tune(pitch_bend as i32);
							note.req.set_volume(volume);
							match sf.add_samples_interlaced(&mut note.req, data, sample_rate) {
								Ok(true) => {}
								Ok(false) => note.kill = true,
								Err(e) => eprintln!("{}", e),
							}
						}
						notes.retain(|note| !note.kill);
					}
				}
			},
			move |err| {
				eprintln!("audio stream error: {}", err);
			},
		).map_err(|e| format!("{}", e))?;
	Ok(stream)
}

#[must_use]
fn check_channel(channel: usize) -> bool {
	if channel >= CHANNEL_COUNT {
		eprintln!("channel {} out of range", channel);
		false
	} else {
		true
	}
}

fn play_note(channel: i64, note: i64, vel: i64) {
	let channel = channel as usize;
	let note = note as u8;
	let vel = vel as u8;
	
	if !check_channel(channel) {
		return;
	}
	
	let mut note_info = NOTE_INFO
		.lock()
		.expect("couldn't lock notes");
	let mut maybe_sf = SOUNDFONT.lock().expect("couldn't lock soundfont.");
	note_info.notes[channel].retain(|n| n.key != note);
	let preset = note_info.presets[channel];
	if let Some(sf) = maybe_sf.as_mut() {
		match sf.request(preset, note, vel) {
			Ok(req) => {
				note_info.notes[channel].push(Note {
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

fn release_note(channel: i64, note: i64) {
	let channel = channel as usize;
	if !check_channel(channel) {
		return;
	}
	let note = note as u8;
	
	let mut note_info = NOTE_INFO
		.lock()
		.expect("couldn't lock notes");
	let pedal_down = note_info.pedal_down;
	let notes  = &mut note_info.notes[channel];
	if let Some(n) = notes.iter_mut().find(|n| n.key == note && n.down) {
		n.down = false;
		if !pedal_down {
			n.req.set_falloff(NOTE_FALLOFF);
		}
	}
}

fn set_pedal_down(down: bool) {
	let mut note_info = NOTE_INFO
		.lock()
		.expect("couldn't lock notes");
	note_info.pedal_down = down;
	for channel in 0..CHANNEL_COUNT {
		let notes  = &mut note_info.notes[channel];
		if down {
			// disable falloff for all notes
			for note in notes.iter_mut() {
				note.req.set_falloff(1.0);
			}
		} else {
			// start falloff for all non-down notes
			for note in notes.iter_mut() {
				if !note.down {
					note.req.set_falloff(NOTE_FALLOFF);
				}
			}
		}
	}
}

fn set_pitch_bend(amount: f64) {
	let amount = amount as i32;
	let mut note_info = NOTE_INFO
		.lock()
		.expect("couldn't lock notes");
	note_info.pitch_bend = amount;
}

fn load_soundfont(filename: &str) {
	if let Ok(sf) = soundfont::SoundFont::open(filename) {
		for i in 0..sf.preset_count() {
			println!("{}. {}", i, sf.preset_name(i).unwrap());
		}
		let mut sflock = SOUNDFONT.lock().expect("couldn't lock soundfont.");
		*sflock = Some(sf);
	} else {
		eprintln!("Couldn't open soundfont: {}", filename);
	}
}

fn load_preset(channel: i64, preset: i64) {
	let preset = preset as usize;
	let channel = channel as usize;
	if !check_channel(channel) {
		return;
	}
	let mut note_info = NOTE_INFO.lock().expect("couldn't lock notes");
	let mut soundfont = SOUNDFONT.lock().expect("couldn't lock soundfont.");
	if let Some(sf) = soundfont.as_mut() {
		if preset >= sf.preset_count() {
			eprintln!("preset {} out of range", preset);
		} else {
			note_info.presets[channel] = preset;
			if let Err(e) = sf.load_samples_for_preset(preset) {
				eprintln!("error loading preset {}: {}", preset, e);
			}
		}
	}
	
}

fn set_volume(channel: i64, volume: f64) {
	let volume = if volume.is_nan() { 0.0 } else { volume as f32 };
	let mut note_info = NOTE_INFO.lock().expect("couldn't lock notes");
	if channel == -1 {
		note_info.master_volume = volume as f32;
		return;
	}
	let channel = channel as usize;
	if !check_channel(channel) {
		return;
	}
	note_info.channel_volumes[channel] = volume as f32;
}

fn call_fn_if_exists(engine: &rhai::Engine, ast: &rhai::AST, name: &str, args: impl rhai::FuncArgs) {
	let mut scope = rhai::Scope::new();
	match engine.call_fn::<()>(&mut scope, &ast, name, args).map_err(|e| *e) {
		Ok(_) => {},
		Err(rhai::EvalAltResult::ErrorFunctionNotFound(s, _)) if s == name =>
			{/* function not found */},
		Err(e) => eprintln!("Warning: rhai error: {}", e),
	}
}

// @TODO change this to -> () and handle errors
fn playmidi_main() -> Result<(), String> {
	let mut engine = rhai::Engine::new();
	engine.register_fn("pm_load_soundfont", load_soundfont);
	engine.register_fn("pm_load_preset", load_preset);
	engine.register_fn("pm_play_note", play_note);
	engine.register_fn("pm_release_note", release_note);
	engine.register_fn("pm_set_pedal", set_pedal_down);
	engine.register_fn("pm_bend_pitch", set_pitch_bend);
	engine.register_fn("pm_set_volume", set_volume);
	
	let engine = engine; // de-multablify
	let mut ast = engine.compile_file("config.rhai".into()).map_err(|e| format!("{}", e))?;
	engine.run_ast(&ast).map_err(|e| format!("{}", e))?;
	
	
	let mut midi_device;
	{
		let mut idx = -1;
		for (name, _, value) in ast.iter_literal_variables(true, true) {
			if name == "PM_DEVICE_ID" {
				match value.as_int() {
					Ok(i) => match i.try_into() {
							Ok(i) => idx = i,
							Err(_) => eprintln!("PM_DEVICE_ID {} too large.", i),
						},
					Err(t) => eprintln!("Warning: PM_DEVICE_ID should be integer, not {}.", t),
				}
			}
		}
		midi_device = get_midi_device(idx)?;
	}
	
	// without this, top-level statements will be executed each time a function is called
	ast.clear_statements();
	
	let ast = ast; // de-mutablify
	
//	load_soundfont("/etc/alternatives/default-GM.sf3");
//	{
//		use std::time::Instant;
//		let now = Instant::now();
//		sf.load_samples_for_preset(preset).expect("oh no");
//		println!("Loaded in {:?}", now.elapsed());
//	}

	let stream = get_audio_stream()?;
	stream.play().map_err(|e| format!("{}", e))?;
	
	while midi_device.is_connected() {
		while let Some(event) = midi_device.read_event() {
			use midi_input::Event::*;
			match event {
				NoteOn { channel, note, vel } =>
					call_fn_if_exists(&engine, &ast, "pm_note_played", (channel as i64, note as i64, vel as i64)),
				NoteOff { channel, note, vel } =>
					call_fn_if_exists(&engine, &ast, "pm_note_released", (channel as i64, note as i64, vel as i64)),
				PitchBend { channel, amount } => {
					let amount = (amount as f64 - 8192.0) * (1.0 / 8192.0);
					call_fn_if_exists(&engine, &ast, "pm_pitch_bent", (channel as i64, amount));
				}
				ControlChange {
					channel, controller, value
				} => {
					call_fn_if_exists(&engine, &ast, "pm_control_changed", (channel as i64, controller as i64, value as i64));
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
