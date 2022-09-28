extern crate cpal;

use std::io::Write;

//use cpal::traits::{HostTrait, DeviceTrait, StreamTrait};

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
	let sf = match soundfont::SoundFont::open("/etc/alternatives/default-GM.sf2") {
		Err(x) => {
			eprintln!("Error: {}", String::from(x));
			return;
		},
		Ok(s) => s,
	};
	
	println!("{}",sf.name);
	
	// 	let result = playmidi_main();
	// 	if let Err(s) = result {
	// 		eprintln!("Error: {}", s);
	// 	}
	/*
	let host = cpal::default_host();
	let device = host.default_output_device().expect("no output device available");
	let mut supported_configs_range = device.supported_output_configs()
		.expect("error while querying configs");
	let config = supported_configs_range.next()
		.expect("no supported config?!")
		.with_max_sample_rate()
		.into();
	let stream = device.build_output_stream(
		&config,
		move |data: &mut [i16], _: &cpal::OutputCallbackInfo| {
			for sample in data.iter_mut() {
				*sample = 0;
			}
		},
		move |err| {
			println!("audio stream error: {}", err);
		},
	).expect("couldn't build output stream");
	stream.play().expect("couldn't play stream");

	loop {
		std::thread::sleep(std::time::Duration::from_millis(100));
	}
	*/
}
