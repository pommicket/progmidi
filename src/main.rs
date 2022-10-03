// @TODO: sort presets alphabetically
extern crate cpal;

use std::io::Write;

use cpal::traits::{DeviceTrait, HostTrait, StreamTrait};

mod midi_input;
mod soundfont;

#[allow(unused)]
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
	let mut device = device_mgr
		.open(device_id)
		.expect("error opening MIDI device");

	while device.is_connected() {
		let maybe_event = device.read_event();
		if let Some(event) = maybe_event {
			println!("{:?}", event);
		} else {
			std::thread::sleep(std::time::Duration::from_millis(10));
		}
		if let Some(err) = device.get_error() {
			eprintln!("Error: {}", err);
			device.clear_error();
		}
	}
	Ok(())
}

fn main() {
	let mut sf = match soundfont::SoundFont::open("/usr/share/sounds/sf2/FluidR3_GM.sf2") {
		Err(x) => {
			eprintln!("Error: {}", String::from(x));
			return;
		}
		Ok(s) => s,
	};

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
	let device = host
		.default_output_device()
		.expect("no output device available");
	let supported_configs = device
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
			eprintln!("Couldn't configure audio device to have 2 16-bit channels.");
			return;
		}
		Some(x) => x,
	};
	let supp_config: cpal::SupportedStreamConfig = chosen_config.with_max_sample_rate().into();
	if supp_config.channels() != 2 {}
	let config = supp_config.into();
	let mut time = 0.0;
	let mut key = 60;
	sf.load_samples_for_preset(125).expect("oh no");

	let stream = device
		.build_output_stream(
			&config,
			move |data: &mut [i16], _: &cpal::OutputCallbackInfo| {
				for x in data.iter_mut() {
					*x = 0;
				}
				let sample_rate = config.sample_rate.0 as f64;
				match sf.add_samples_interlaced(125, 1.0, key, 60, time, data, sample_rate) {
					Ok(false) => {} //{println!("stop")},
					Err(e) => eprintln!("{}", e),
					_ => {}
				}
				time += (data.len() as f64) / (2.0 * sample_rate);
				if time >= 1.0 {
					println!("{}", sf.cache_size());
					time = 0.0;
					key += 1;
				}
			},
			move |err| {
				println!("audio stream error: {}", err);
			},
		)
		.expect("couldn't build output stream");
	stream.play().expect("couldn't play stream");

	loop {
		std::thread::sleep(std::time::Duration::from_millis(100));
	}
}
