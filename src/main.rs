extern crate chrono;
extern crate cpal;
extern crate rhai;

mod midi_input;
mod soundfont;

use cpal::traits::{DeviceTrait, HostTrait, StreamTrait};
use soundfont::SoundFont;
use std::ffi::c_int;
use std::fs::File;
use std::io::Write;
use std::sync::{Mutex, MutexGuard};
use std::time::{Duration, Instant};

const CHANNEL_COUNT: usize = 17; // 16 MIDI channels + metronome
const METRONOME_CHANNEL: i64 = 16;

struct Note {
	key: u8,
	req: soundfont::SamplesRequest,
	down: bool,
	// if user plays A4, then A4 again, the first A4 will get this (it will be cut out quickly,
	// but not instantaneously to avoid clicking)
	cut: bool,
	kill: bool, // only used briefly
}

#[derive(Clone)]
struct Metronome {
	bpm: f32,
	key: i64,
}

struct MidiRecording {
	data: Vec<u8>,
	last_event_time: Instant,
}

struct WavRecording {
	data: Vec<i16>,
}

struct NoteInfo {
	output_sample_rate: u32,
	midi_recording: Option<MidiRecording>,
	wav_recording: Option<WavRecording>,
	pitch_bend: i32, // in cents
	pedal_down: bool,
	presets: [usize; CHANNEL_COUNT],
	notes: [Vec<Note>; CHANNEL_COUNT],
	channel_volumes: [f32; CHANNEL_COUNT],
	master_volume: f32,
	metronome: Metronome,
	release_falloff: f32, // falloff when a note is released
}

static NOTE_INFO: Mutex<NoteInfo> = Mutex::new(NoteInfo {
	midi_recording: None,
	wav_recording: None,
	output_sample_rate: 0,
	pitch_bend: 0,
	// this is ridiculous.
	notes: [
		vec![],
		vec![],
		vec![],
		vec![],
		vec![],
		vec![],
		vec![],
		vec![],
		vec![],
		vec![],
		vec![],
		vec![],
		vec![],
		vec![],
		vec![],
		vec![],
		vec![],
	],
	pedal_down: false,
	presets: [0; CHANNEL_COUNT],
	channel_volumes: [1.0; CHANNEL_COUNT],
	master_volume: 1.0,
	release_falloff: 0.1,
	metronome: Metronome { bpm: 0.0, key: 0 },
});

// unfortunately, this needs to be a separate mutex because otherwise
// we would get double mutable borrows. sometimes we only need one of
// the note_info / soundfont, so it might actually be slightly better
// this way.
static SOUNDFONT: Mutex<Option<SoundFont>> = Mutex::new(None);

impl MidiRecording {
	fn new() -> Self {
		MidiRecording {
			last_event_time: Instant::now(),
			data: Vec::with_capacity(1000),
		}
	}

	fn write(&mut self, byte: u8) {
		self.data.push(byte);
	}
	fn write_vlq(&mut self, value: u32) {
		if value < 0x80 {
			self.write(value as u8);
		} else if value < 0x4000 {
			self.write((0x80 | value >> 7) as u8);
			self.write((value & 0x7f) as u8);
		} else if value < 0x200000 {
			self.write((0x80 | value >> 14) as u8);
			self.write((0x80 | value >> 7) as u8);
			self.write((value & 0x7f) as u8);
		} else {
			self.write((0x80 | value >> 21) as u8);
			self.write((0x80 | value >> 14) as u8);
			self.write((0x80 | value >> 7) as u8);
			self.write((value & 0x7f) as u8);
		}
	}

	fn write_event(&mut self, status: u8, data1: Option<u8>, data2: Option<u8>) {
		// check data
		if let Some(b) = data1 {
			if b >= 128 {
				return;
			}
		}
		if let Some(b) = data2 {
			if b >= 128 {
				return;
			}
		}

		let now = Instant::now();
		let ms_elapsed = now.duration_since(self.last_event_time).as_millis();
		self.write_vlq(ms_elapsed as u32);
		self.write(status);
		if let Some(b) = data1 {
			self.write(b);
			if let Some(b2) = data2 {
				self.write(b2);
			}
		}
		self.last_event_time = now;
	}

	fn save(&self, filename: &str) -> std::io::Result<()> {
		print!("Saving MIDI recording...");
		flush_stdout();
		let mut file = std::fs::OpenOptions::new()
			.write(true)
			.create_new(true)
			.open(filename)?;
		file.write_all("MThd".as_bytes())?;
		write_u32_be(&mut file, 6)?; // header size
		write_u16_be(&mut file, 1)?; // <format> = 1
		write_u16_be(&mut file, 1)?; // <ntrks> = 1
							 // tick = 1ms, quarter note = 600 ticks => 100bpm
		write_u16_be(&mut file, 600)?;
		file.write_all("MTrk".as_bytes())?;

		// MIDI "track end"
		// (people complain if we don't include this)
		let track_end = [0, 0xff, 0x2f, 0];

		write_u32_be(&mut file, (self.data.len() + track_end.len()) as u32)?; // track size
		file.write_all(&self.data)?;
		file.write_all(&track_end)?;

		file.sync_all()?;
		println!("\rMIDI recording saved to {}", filename);
		Ok(())
	}
}

impl WavRecording {
	fn new() -> Self {
		WavRecording {
			data: Vec::with_capacity(441000),
		}
	}

	fn write(&mut self, samples: &[i16]) {
		if samples.len() + self.data.len() > (u32::MAX / 2 - 100) as usize {
			// too much data for wav file
			return;
		}
		for x in samples {
			self.data.push(*x);
		}
	}

	fn save(&self, filename: &str, sample_rate: u32) -> std::io::Result<()> {
		print!("Saving WAV recording...");
		// rust doesn't seem to be able to optimize out
		// a i16::to_le_bytes loop. annoying.
		let data8 = unsafe {
			std::slice::from_raw_parts(self.data.as_ptr() as *const u8, 2 * self.data.len())
		};
		flush_stdout();
		let mut file = std::fs::OpenOptions::new()
			.write(true)
			.create_new(true)
			.open(filename)?;
		file.write_all("RIFF".as_bytes())?;
		write_u32_le(&mut file, 36 + data8.len() as u32)?; // RIFF chunk size
		file.write_all("WAVEfmt ".as_bytes())?;
		write_u32_le(&mut file, 16)?; // fmt  chunk size
		write_u16_le(&mut file, 1)?; // PCM
		write_u16_le(&mut file, 2)?; // 2 channels
		write_u32_le(&mut file, sample_rate)?;
		write_u32_le(&mut file, sample_rate * 4)?; // "byte rate"
		write_u16_le(&mut file, 4)?; // block align
		write_u16_le(&mut file, 16)?; // bits per sample
		file.write_all("data".as_bytes())?;
		write_u32_le(&mut file, data8.len() as u32)?; // data chunk size
		file.write_all(data8)?;
		file.sync_all()?;
		println!("\rWAV recording saved to {}", filename);
		Ok(())
	}
}

fn flush_stdout() {
	if std::io::stdout().flush().is_err() {
		//who cares
	}
}

fn write_u16_be(file: &mut File, n: u16) -> std::io::Result<()> {
	file.write_all(&u16::to_be_bytes(n))
}
fn write_u32_be(file: &mut File, n: u32) -> std::io::Result<()> {
	file.write_all(&u32::to_be_bytes(n))
}

fn write_u16_le(file: &mut File, n: u16) -> std::io::Result<()> {
	file.write_all(&u16::to_le_bytes(n))
}
fn write_u32_le(file: &mut File, n: u32) -> std::io::Result<()> {
	file.write_all(&u32::to_le_bytes(n))
}

fn lock_note_info<'a>() -> MutexGuard<'a, NoteInfo> {
	NOTE_INFO.lock().expect("couldn't lock notes")
}

fn lock_soundfont<'a>() -> MutexGuard<'a, Option<SoundFont>> {
	SOUNDFONT.lock().expect("couldn't lock soundfont")
}

fn lock_note_info_and_soundfont<'a>(
) -> (MutexGuard<'a, NoteInfo>, MutexGuard<'a, Option<SoundFont>>) {
	let note_info = lock_note_info();
	let soundfont = lock_soundfont();
	(note_info, soundfont)
}

fn get_midi_device(idx: i32) -> Result<midi_input::Device, String> {
	let mut device_mgr = midi_input::DeviceManager::new()?;
	device_mgr.set_quiet(true);
	let devices = device_mgr.list()?;

	let device_idx = match idx {
		0 => devices.default,
		i if i >= 1 => (i - 1) as usize,
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
			flush_stdout();

			let mut buf = String::new();
			std::io::stdin()
				.read_line(&mut buf)
				.expect("error reading stdin");
			let s = buf.trim();
			if s.is_empty() {
				devices.default
			} else {
				match s.parse::<usize>() {
					Ok(idx) if idx >= 1 && idx <= devices.len() => idx - 1,
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

fn get_audio_stream() -> Result<(cpal::Stream, u32), String> {
	let host = cpal::default_host();
	let audio_device = host
		.default_output_device()
		.expect("no output device available");
	let supported_configs = audio_device
		.supported_output_configs()
		.expect("error while querying configs");
	let mut chosen_config = None;
	let srate = 44100;
	for config in supported_configs {
		if config.channels() != 2
			|| config.sample_format() != cpal::SampleFormat::I16
			|| config.min_sample_rate().0 > srate
			|| config.max_sample_rate().0 < srate
		{
			continue;
		}
		chosen_config = Some(config);
	}
	let chosen_config = match chosen_config {
		None => return Err("Couldn't get desired audio properties.".to_string()),
		Some(x) => x,
	};

	let supp_config: cpal::SupportedStreamConfig =
		chosen_config.with_sample_rate(cpal::SampleRate(srate));
	let config = supp_config.into();

	let stream = audio_device
		.build_output_stream(
			&config,
			move |data: &mut [i16], _: &cpal::OutputCallbackInfo| {
				let (mut note_info, mut maybe_sf) = lock_note_info_and_soundfont();
				if let Some(sf) = maybe_sf.as_mut() {
					let sample_rate = config.sample_rate.0 as f64;
					for x in data.iter_mut() {
						*x = 0;
					}
					let pitch_bend = note_info.pitch_bend;
					for channel in 0..CHANNEL_COUNT {
						let volume =
							0.1 * note_info.master_volume * note_info.channel_volumes[channel];
						let notes = &mut note_info.notes[channel];
						for note in notes.iter_mut() {
							note.req.set_tune(pitch_bend as i32);
							note.req.set_volume(volume);
							if note.cut {
								note.req.set_falloff(0.01);
							}
							match sf.add_samples_interlaced(&mut note.req, data, sample_rate) {
								Ok(true) => {}
								Ok(false) => note.kill = true,
								Err(e) => eprintln!("{}", e),
							}
						}
						notes.retain(|note| !note.kill);
					}
				}
				drop(maybe_sf);
				if let Some(recording) = note_info.wav_recording.as_mut() {
					recording.write(data);
				}
			},
			move |err| {
				eprintln!("audio stream error: {}", err);
			},
		)
		.map_err(|e| format!("{}", e))?;
	Ok((stream, config.sample_rate.0))
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

	let (mut note_info, mut maybe_sf) = lock_note_info_and_soundfont();

	if channel < 16 {
		if let Some(recording) = note_info.midi_recording.as_mut() {
			// note on event
			recording.write_event(0b1001_0000 | (channel as u8), Some(note), Some(vel));
		}
	}
	for n in note_info.notes[channel].iter_mut() {
		if n.key == note {
			n.cut = true;
		}
	}

	let preset = note_info.presets[channel];
	if let Some(sf) = maybe_sf.as_mut() {
		match sf.request(preset, note, vel) {
			Ok(req) => {
				note_info.notes[channel].push(Note {
					key: note,
					req,
					down: true,
					cut: false,
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

	let mut note_info = lock_note_info();

	if channel < 16 {
		if let Some(recording) = note_info.midi_recording.as_mut() {
			// note off event
			recording.write_event(0b1000_0000 | (channel as u8), Some(note), Some(0));
		}
	}

	let pedal_down = note_info.pedal_down;
	let release_falloff = note_info.release_falloff;
	let notes = &mut note_info.notes[channel];
	for n in notes.iter_mut() {
		if n.key == note {
			n.down = false;
			if !pedal_down {
				n.req.set_falloff(release_falloff);
			}
		}
	}
}

fn set_pedal_down(down: bool) {
	let mut note_info = lock_note_info();
	note_info.pedal_down = down;
	for channel in 0..CHANNEL_COUNT {
		let release_falloff = note_info.release_falloff;
		let notes = &mut note_info.notes[channel];
		if down {
			// disable falloff for all notes
			for note in notes.iter_mut() {
				note.req.set_falloff(1.0);
			}
		} else {
			// start falloff for all non-down notes
			for note in notes.iter_mut() {
				if !note.down {
					note.req.set_falloff(release_falloff);
				}
			}
		}
	}
}

fn set_pitch_bend(amount: f64) {
	let amount = amount as i32;
	let mut note_info = lock_note_info();
	note_info.pitch_bend = amount;
}

fn set_release_falloff(falloff: f64) {
	let mut note_info = lock_note_info();
	note_info.release_falloff = falloff as f32;
}

fn load_soundfont(filename: &str) {
	if let Ok(sf) = SoundFont::open(filename) {
		let mut sflock = lock_soundfont();
		*sflock = Some(sf);
	} else {
		eprintln!("Couldn't open soundfont: {}", filename);
	}
}

fn print_presets() {
	if let Some(sf) = lock_soundfont().as_ref() {
		for i in 0..sf.preset_count() {
			println!("{}. {}", i, sf.preset_name(i).unwrap());
		}
	}
}

fn load_preset(channel: i64, preset: i64) {
	let preset = preset as usize;

	let (mut note_info, mut soundfont) = lock_note_info_and_soundfont();

	if let Some(sf) = soundfont.as_mut() {
		if preset >= sf.preset_count() {
			eprintln!("preset {} out of range", preset);
		} else {
			if let Err(e) = sf.load_samples_for_preset(preset) {
				eprintln!("error loading preset {}: {}", preset, e);
			}

			if channel == -1 {
				for p in note_info.presets.iter_mut() {
					*p = preset;
				}
			} else {
				let channel = channel as usize;
				if !check_channel(channel) {
					return;
				}
				note_info.presets[channel] = preset;
			}
		}
	} else {
		eprintln!("Can't load preset {} since no soundfont is loaded", preset);
	}
}

fn load_preset_by_name(channel: i64, name: &str) {
	let mut preset_names;
	{
		if let Some(soundfont) = lock_soundfont().as_ref() {
			preset_names = Vec::with_capacity(soundfont.preset_count());
			for i in 0..soundfont.preset_count() {
				let pname = soundfont.preset_name(i).unwrap().to_lowercase();
				if pname.contains(name) {
					preset_names.push((i, pname));
				}
			}
		} else {
			eprintln!(
				"Can't load preset \"{}\", since no soundfont is loaded.",
				name
			);
			return;
		}
	}
	preset_names.sort_by(|(_, a), (_, b)| {
		use std::cmp::Ordering::*;
		if a.len() < b.len() {
			Less
		} else if a.len() > b.len() {
			Greater
		} else if a < b {
			Less
		} else if a > b {
			Greater
		} else {
			Equal
		}
	});
	if !preset_names.is_empty() {
		load_preset(channel, preset_names[0].0 as i64);
	}
}

fn set_volume(channel: i64, volume: f64) {
	let volume = if volume.is_nan() { 0.0 } else { volume as f32 };
	let mut note_info = lock_note_info();
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

fn set_metronome(key: i64, bpm: f64) {
	let mut note_info = lock_note_info();
	note_info.metronome = Metronome {
		key,
		bpm: bpm as f32,
	}
}

fn start_midi_recording() {
	let recording = MidiRecording::new();
	let mut note_info = lock_note_info();
	note_info.midi_recording = Some(recording);
}

fn stop_midi_recording_err() -> std::io::Result<()> {
	let mut note_info = lock_note_info();
	if let Some(recording) = note_info.midi_recording.take() {
		drop(note_info);
		let name = chrono::Local::now()
			.format("%Y-%m-%d-%H-%M-%S.mid")
			.to_string();
		recording.save(&name)?;
	}
	Ok(())
}

fn stop_midi_recording() {
	if let Err(e) = stop_midi_recording_err() {
		eprintln!("Error stopping MIDI recording: {}", e);
	}
}

fn start_wav_recording() {
	let recording = WavRecording::new();
	let mut note_info = lock_note_info();
	note_info.wav_recording = Some(recording);
}

fn stop_wav_recording_err() -> std::io::Result<()> {
	let mut note_info = lock_note_info();
	if let Some(recording) = note_info.wav_recording.take() {
		let sample_rate = note_info.output_sample_rate;
		drop(note_info);
		let name = chrono::Local::now()
			.format("%Y-%m-%d-%H-%M-%S.wav")
			.to_string();
		recording.save(&name, sample_rate)?;
	}
	Ok(())
}

fn stop_wav_recording() {
	if let Err(e) = stop_wav_recording_err() {
		eprintln!("Error stopping WAV recording: {}", e);
	}
}

fn call_fn_if_exists(
	engine: &rhai::Engine,
	ast: &rhai::AST,
	this: &mut rhai::Dynamic,
	name: &str,
	args: impl rhai::FuncArgs,
) {
	let mut arg_vec = vec![];
	args.parse(&mut arg_vec);
	let mut scope = rhai::Scope::new();
	match engine
		.call_fn_raw(&mut scope, ast, true, false, name, Some(this), &mut arg_vec)
		.map_err(|e| *e)
	{
		Ok(_) => {}
		Err(rhai::EvalAltResult::ErrorFunctionNotFound(s, _)) if s == name => { /* function not found */
		}
		Err(e) => eprintln!("Warning: rhai error: {}", e),
	}
}

const SIGINT: c_int = 2;

type SigHandler = extern "C" fn(c_int);

extern "C" {
	fn signal(signum: c_int, handler: SigHandler) -> SigHandler;
}

extern "C" fn sig_handler(_signum: c_int) {
	// do this in a separate thread in case note_info is locked in this thread
	std::thread::spawn(|| {
		stop_midi_recording();
		stop_wav_recording();
		std::process::exit(0);
	});
}

fn main() {
	unsafe { signal(SIGINT, sig_handler) };
	
	let application_start = Instant::now();

	let mut engine = rhai::Engine::new();
	engine.set_max_expr_depths(0, 0);
	engine.register_fn("pm_load_soundfont", load_soundfont);
	engine.register_fn("pm_print_presets", print_presets);
	engine.register_fn("pm_load_preset", load_preset);
	engine.register_fn("pm_load_preset", load_preset_by_name);
	engine.register_fn("pm_play_note", play_note);
	engine.register_fn("pm_release_note", release_note);
	engine.register_fn("pm_set_pedal", set_pedal_down);
	engine.register_fn("pm_bend_pitch", set_pitch_bend);
	engine.register_fn("pm_bend_pitch", |bend: i64| {
		set_pitch_bend(bend as f64);
	});
	engine.register_fn("pm_set_volume", set_volume);
	engine.register_fn("pm_set_volume", |channel: i64, volume: i64| {
		set_volume(channel, volume as f64);
	});
	engine.register_fn("pm_set_metronome", set_metronome);
	// allow integer bpm as well
	engine.register_fn("pm_set_metronome", |key: i64, bpm: i64| {
		set_metronome(key, bpm as f64);
	});
	engine.register_fn("pm_set_release_falloff", set_release_falloff);
	engine.register_fn("pm_set_release_falloff", |f: i64| {
		set_release_falloff(f as f64);
	});
	engine.register_fn("pm_start_midi_recording", start_midi_recording);
	engine.register_fn("pm_stop_midi_recording", stop_midi_recording);
	engine.register_fn("pm_start_wav_recording", start_wav_recording);
	engine.register_fn("pm_stop_wav_recording", stop_wav_recording);
	engine.register_fn("pm_print", |s: &str| {
		print!("{s}");
		if std::io::stdout().flush().is_err() {
			// whatever
		}
	});
	engine.register_fn("pm_get_time", move || -> i64 {
		Instant::now().duration_since(application_start)
			.as_millis()
			.try_into()
			.expect("i'm gonna stop you right there. you've been running this program for HOW LONG?")
	});
	
	let engine = engine; // de-multablify
	let args: Vec<String> = std::env::args().collect();
	let config_filename = match args.len() {
		0 | 1 => "config.rhai",
		2 => &args[1],
		_ => {
			eprintln!("Usage: {} [config file]", args[0]);
			return;
		}
	};
	let ast = match engine.compile_file(config_filename.into()) {
		Ok(x) => x,
		Err(e) => {
			eprintln!("Config error: {}", e);
			return;
		}
	};

	if let Err(e) = engine.run_ast(&ast) {
		eprintln!(
			"Error running top-level statements in {}: {}",
			config_filename, e
		);
		return;
	}

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
		midi_device = match get_midi_device(idx) {
			Ok(dev) => dev,
			Err(e) => {
				eprintln!("Error loading MIDI device: {}", e);
				return;
			}
		};
	}
	
	let mut this = rhai::Dynamic::from(rhai::Map::new());
	call_fn_if_exists(&engine, &ast, &mut this, "pm_start", ());

	let (stream, sample_rate) = match get_audio_stream() {
		Ok(s) => s,
		Err(e) => {
			eprintln!("Error loading audio stream: {}", e);
			return;
		}
	};

	{
		let mut note_info = lock_note_info();
		note_info.output_sample_rate = sample_rate;
	}

	if let Err(e) = stream.play() {
		eprintln!("Error starting audio stream: {}", e);
		return;
	}

	let mut last_metronome_tick = Instant::now();

	while midi_device.is_connected() {
		let metronome_time = last_metronome_tick.elapsed().as_secs_f32();

		let metronome;
		{
			let note_info = lock_note_info();
			metronome = note_info.metronome.clone();
		}
		if metronome.bpm > 0.0 && metronome_time >= 60.0 / metronome.bpm {
			play_note(METRONOME_CHANNEL, metronome.key, 127);
			last_metronome_tick = Instant::now();
		}

		while let Some(event) = midi_device.read_event() {
			use midi_input::Event::*;
			match event {
				NoteOn { channel, note, vel } => call_fn_if_exists(
					&engine,
					&ast,
					&mut this,
					"pm_note_played",
					(channel as i64, note as i64, vel as i64),
				),
				NoteOff { channel, note, vel } => call_fn_if_exists(
					&engine,
					&ast,
					&mut this,
					"pm_note_released",
					(channel as i64, note as i64, vel as i64),
				),
				PitchBend { channel, amount } => {
					let amount = (amount as f64 - 8192.0) * (1.0 / 8192.0);
					call_fn_if_exists(&engine, &ast, &mut this, "pm_pitch_bent", (channel as i64, amount));
				}
				ControlChange {
					channel,
					controller,
					value,
				} => {
					call_fn_if_exists(
						&engine,
						&ast,
						&mut this,
						"pm_control_changed",
						(channel as i64, controller as i64, value as i64),
					);
				}
				_ => {}
			}
		}
		if let Some(err) = midi_device.get_error() {
			eprintln!("Error: {}", err);
			midi_device.clear_error();
		}
		std::thread::sleep(Duration::from_millis(1));
	}
}
