extern crate cpal;

use std::io::Write;

use cpal::traits::{DeviceTrait, HostTrait, StreamTrait};

mod midi_input;
mod soundfont;

#[allow(unused)]
fn midi_in_main() -> Result<(), String> {
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
		while let Some(event) = device.read_event() {
			println!("{:?}", event);
		}
		std::thread::sleep(std::time::Duration::from_millis(140));
		if let Some(err) = device.get_error() {
			eprintln!("Error: {}", err);
			device.clear_error();
		}
	}
	Ok(())
}

#[allow(unused)]
fn soundfont_main() {
	let mut sf = match soundfont::SoundFont::open("/etc/alternatives/default-GM.sf3") {
		///usr/share/sounds/sf2/FluidR3_GM.sf2") {
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
	let supp_config: cpal::SupportedStreamConfig = chosen_config.with_max_sample_rate();
	if supp_config.channels() != 2 {}
	let config = supp_config.into();
	let mut time = 0.0;
	let mut key = 60;
	let preset = 299;
	
	{
		use std::time::Instant;
		let now = Instant::now();
		sf.load_samples_for_preset(preset).expect("oh no");
		println!("Loaded in {:?}", now.elapsed());
	}
	let stream = device
		.build_output_stream(
			&config,
			move |data: &mut [i16], _: &cpal::OutputCallbackInfo| {
				for x in data.iter_mut() {
					*x = 0;
				}
				let sample_rate = config.sample_rate.0 as f64;
				for k in key..key + 1 {
					let mut request = sf.request(preset, k, 60, 0.0).expect("ah");
					request.set_hold_time(time);
					request.set_volume(0.5);
					request.set_tune(((key % 2) * 50) as _);
					request.set_falloff(0.0, 0.01);
					match sf.add_samples_interlaced(&request, data, sample_rate) {
						Ok(false) => {} //{println!("stop")},
						Err(e) => eprintln!("{}", e),
						_ => {}
					}
				}
				time += (data.len() as f64) / (2.0 * sample_rate);
				if time >= 0.3 {
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

fn main() {
	// 	match midi_in_main() {
	// 		Err(e) => println!("{}", e),
	// 		_ => {}
	// 	}
	soundfont_main();
}
