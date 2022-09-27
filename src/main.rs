extern crate cpal;

use std::io::Write;

//use cpal::traits::{HostTrait, DeviceTrait, StreamTrait};

mod midi_input;

fn main() {
	let mut device_mgr = midi_input::DeviceManager::new().expect("Couldn't create device manager");
	device_mgr.set_quiet(true);
	let devices = device_mgr.list().expect("couldn't list MIDI devices");
	let mut index = 0;
	for device in &devices {
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
		index += 1;
	}
	print!("Select a device (default {}): ", devices.default + 1);
	match std::io::stdout().flush() {
		Err(_) => {} // whatever
		_ => {}
	}

	let device_id;
	{
		let mut buf = String::new();
		std::io::stdin()
			.read_line(&mut buf)
			.expect("error reading stdin");
		let s = buf.trim();
		if s.len() == 0 {
			device_id = &devices[devices.default].id;
		} else {
			match s.parse::<usize>() {
				Ok(idx) if idx >= 1 && idx <= devices.len() => {
					device_id = &devices[idx - 1].id;
				}
				_ => {
					eprintln!("Bad device ID: {}", s);
					return;
				}
			}
		}
	}
	let mut device = device_mgr
		.open(&device_id)
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
		}
	}

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
