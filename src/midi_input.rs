use std::ffi::{c_char, c_int, c_void, CStr, CString};
/// Query and read from MIDI input devices.
/// Don't assume these functions are thread-safe.
/// You should only need to call them in the main thread anyways.
/// Basic usage:
/// ```
/// let mut manager = DeviceManager::new().unwrap();
/// let mut devices = manager.list();
/// while devices.len() == 0 {
///     std::thread::sleep(std::time::Duration::from_millis(100));
///     devices = manager.list();
/// }
/// let mut i = devices.default;
/// for device in devices {
///      println!("{}",device.name);
/// }
/// let device = manager.open(&devices[i].id);
/// while device.is_connected() {
///     if let Some(event) = device.read_event() {
///         ...
///     } else {
///         std::thread::sleep(std::time::Duration::from_millis(100));
///     }
/// }
/// ```
use std::sync::Mutex;
// so much stuff depends on libc
// it's not such a big deal to add it as a dependency
// (we only really need it for size_t, ssize_t)
extern crate libc;
use libc::{free, size_t, ssize_t};

// (snd_rawmidi_t is an opaque struct)
type SndRawMidiT = c_void;

/// Opaque device manager type.
pub struct DeviceManager {}

/// Opaque type for device ID.
#[derive(Debug, Clone)]
pub struct DeviceID {
	name: String,
}

/// Information about a device.
pub struct DeviceInfo {
	/// A human-readable name for the device.
	/// This may contain newlines. You can just use the first line
	/// if you want a quick description.
	pub name: String,
	/// The id, to be used with `open_device`.
	pub id: DeviceID,
}

/// Opaque type for MIDI input device.
#[derive(Debug)]
pub struct Device {
	rawmidi: *mut SndRawMidiT,
	error: i32,
	// used to hold first data byte of two-data-byte MIDI events.
	buffered_byte: Option<u8>,
	last_status: u8,
	connected: bool,
}

/// An error produced by opening a device.
#[derive(Debug)]
pub enum DeviceOpenError {
	/// the device was not found.
	/// this can hapen if it was disconnected between calling `list_devices`
	/// and `open_device`.
	NotFound(String),
	/// other error
	Other(String),
}

pub struct DeviceList {
	pub devices: Vec<DeviceInfo>,
	pub default: usize,
}

/// A MIDI event.
/// All notes and velocities are from 0 to 127.
/// A note of 60 +/- n indicates n semitones above/below middle C.
#[derive(Debug, Clone, Copy)]
pub enum Event {
	/// a note was pressed
	/// This may also indicate a note off, if `vel = 0`.
	NoteOn { channel: u8, note: u8, vel: u8 },
	/// a note was released
	NoteOff { channel: u8, note: u8, vel: u8 },
	/// a change in pressure to a note
	NoteAftertouch { channel: u8, note: u8, pressure: u8 },
	/// a controller was changed
	ControlChange {
		channel: u8,
		controller: u8,
		value: u8,
	},
	/// the current program should be changed.
	ProgramChange { channel: u8, program: u8 },
	/// like `NoteAftertouch`, but affects all notes on this channel
	ChannelAftertouch { channel: u8, pressure: u8 },
	/// the pitch bend was altered.
	/// amount ranges from `-0x2000` to `0x1fff`.
	PitchBend { channel: u8, amount: i16 },
	/// another event. probably doesn't matter to you.
	/// note that if there are multiple bytes of data,
	/// you will get multiple `Other` events: e.g.
	/// ```
	/// Other { status: 0xf0, data: None }
	/// Other { status: 0xf0, data: Some(0x33) }
	/// Other { status: 0xf0, data: Some(0x45) }
	/// Other { status: 0xf0, data: Some(0x1a) }
	/// ```
	/// corresponds to the raw MIDI data `f0 33 45 1a`.
	Other { status: u8, data: Option<u8> },
}

impl IntoIterator for DeviceList {
	type Item = DeviceInfo;
	type IntoIter = std::vec::IntoIter<Self::Item>;

	fn into_iter(self) -> Self::IntoIter {
		self.devices.into_iter()
	}
}

impl<'a> IntoIterator for &'a DeviceList {
	type Item = &'a DeviceInfo;
	type IntoIter = std::slice::Iter<'a, DeviceInfo>;

	fn into_iter(self) -> Self::IntoIter {
		self.devices.iter()
	}
}

impl DeviceList {
	pub fn len(&self) -> usize {
		self.devices.len()
	}
}

impl std::ops::Index<usize> for DeviceList {
	type Output = DeviceInfo;

	fn index(&self, i: usize) -> &Self::Output {
		&self.devices[i]
	}
}

#[link(name = "asound", kind = "dylib")]
extern "C" {
	fn snd_device_name_hint(
		card: c_int,
		devname: *const c_char,
		hints: *mut *mut *mut c_void,
	) -> c_int;
	fn snd_device_name_get_hint(hint: *const c_void, id: *const c_char) -> *mut c_char;
	fn snd_device_name_free_hint(hints: *mut *mut c_void) -> c_int;
	fn snd_rawmidi_open(
		inputp: *mut *mut SndRawMidiT,
		outputp: *mut *mut SndRawMidiT,
		name: *const c_char,
		mode: c_int,
	) -> c_int;
	fn snd_rawmidi_nonblock(rawmidi: *mut SndRawMidiT, nonblock: c_int) -> c_int;
	fn snd_rawmidi_read(rawmidi: *mut SndRawMidiT, buffer: *mut c_void, size: size_t) -> ssize_t;
	fn snd_rawmidi_close(rawmidi: *mut SndRawMidiT) -> c_int;
	fn snd_lib_error_set_handler(handler: *mut c_void) -> c_int;
}

impl From<&DeviceOpenError> for String {
	fn from(e: &DeviceOpenError) -> String {
		use DeviceOpenError::*;
		match e {
			NotFound(s) => format!("device not found: {}", s),
			Other(s) => s.clone(),
		}
	}
}

impl From<DeviceOpenError> for String {
	fn from(e: DeviceOpenError) -> String {
		String::from(&e)
	}
}

// technically there should be varargs here but oh well
pub unsafe extern "C" fn snd_lib_error_handler_quiet(
	_file: *const c_char,
	_line: i32,
	_function: *const c_char,
	_err: i32,
) {
}

// convert cstr to Option<String>. you'll get back None if
// either cstr is null, or invalid UTF-8.
unsafe fn cstr_to_option_string(cstr: *const c_char) -> Option<String> {
	if cstr == 0 as _ {
		return None;
	}

	let result = CStr::from_ptr(cstr).to_str();
	match result {
		Ok(s) => Some(s.to_string()),
		Err(_) => None,
	}
}

static DEVICE_MANAGER_EXISTS: Mutex<bool> = Mutex::new(false);

impl DeviceManager {
	/// Create a new manager for MIDI input devices.
	/// Only ONE device manager can exist at any point in time.
	pub fn new() -> Result<DeviceManager, String> {
		let mut r = DEVICE_MANAGER_EXISTS
			.lock()
			.map_err(|_| "Failed to lock mutex".to_string())?;
		if *r {
			return Err("A device manager already exists.".to_string());
		}
		*r = true;
		Ok(DeviceManager {})
	}

	/// Pass `true` to stop ALSA from outputting errors to `stderr`
	/// (no effect on Windows).
	pub fn set_quiet(&mut self, quiet: bool) {
		let mut callback: *mut c_void = 0 as _;

		if quiet {
			callback = snd_lib_error_handler_quiet as _;
		}
		unsafe {
			snd_lib_error_set_handler(callback);
		}
	}

	/// Returns a `DeviceList` containing descriptions for all MIDI input devices.
	pub fn list(&self) -> Result<DeviceList, String> {
		let mut hints: *mut *mut c_void = 0 as _;
		let err: c_int;
		unsafe {
			err =
				snd_device_name_hint(-1, "rawmidi\0".as_ptr() as *const c_char, (&mut hints) as _);
		}

		// in theory hints should never be null if err != 0.
		// but you can never be sure.
		if err != 0 || hints == 0 as _ {
			return Err(format!("failed to get device hints (error code {})", err));
		}

		let mut idx: usize = 0;
		let mut devices = vec![];
		let mut default = None;

		loop {
			let hint;
			unsafe {
				hint = *hints.add(idx);
			}
			if hint.is_null() {
				break;
			}

			let name;
			let desc;

			unsafe {
				let name_cstr = snd_device_name_get_hint(hint, "NAME\0".as_ptr() as _);
				let desc_cstr = snd_device_name_get_hint(hint, "DESC\0".as_ptr() as _);
				//let ioid_cstr = snd_device_name_get_hint(hint, "IOID\0".as_ptr() as _);

				name = cstr_to_option_string(name_cstr);
				desc = cstr_to_option_string(desc_cstr);

				free(name_cstr as _);
				free(desc_cstr as _);
			}

			// we need the name to be able to do anything with the device
			if let Some(name_unwrapped) = name {
				let has_desc = desc.is_some();
				let desc_unwrapped = desc.unwrap_or_else(|| "(no description)".to_string());
				let desc_str = format!("{}\n{}", name_unwrapped, desc_unwrapped);
				let info = DeviceInfo {
					id: DeviceID {
						name: name_unwrapped,
					},
					name: desc_str,
				};

				if has_desc && default.is_none() {
					default = Some(idx);
				}

				devices.push(info);
			}

			idx += 1;
		}

		unsafe {
			snd_device_name_free_hint(hints);
		}

		Ok(DeviceList {
			devices,
			default: default.unwrap_or(0),
		})
	}

	/// Open a device.
	pub fn open(&self, id: &DeviceID) -> Result<Device, DeviceOpenError> {
		let name_cstr = CString::new(&id.name[..]).map_err(|_| {
			// (name has null bytes)
			// this should never happen since `name` should
			//   have been constructed from a cstr.
			// but it could happen if someone uses unsafe or something.
			DeviceOpenError::Other("invalid device ID".to_string())
		})?;
		let mut input: *mut SndRawMidiT = 0 as _;
		let mut err;
		unsafe {
			err = snd_rawmidi_open((&mut input) as _, 0 as _, name_cstr.as_ptr(), 0);
			if err == 0 && !input.is_null() {
				err = snd_rawmidi_nonblock(input, 1);
			}
		}
		if err != 0 || input.is_null() {
			if err == -2 {
				return Err(DeviceOpenError::NotFound(id.name.clone()));
			}
			return Err(DeviceOpenError::Other(format!(
				"other error (code {})",
				err
			)));
		}
		Ok(Device {
			rawmidi: input,
			buffered_byte: None,
			error: 0,
			last_status: 0,
			connected: true,
		})
	}
}

impl Device {
	fn read_raw(&mut self, buffer: &mut [u8]) -> usize {
		let n;
		unsafe {
			let pbuffer = &mut buffer[0] as *mut u8 as *mut c_void;
			n = snd_rawmidi_read(self.rawmidi, pbuffer, buffer.len() as _);
		}
		if n == -11 {
			// EAGAIN (indicates no bytes were read)
			0
		} else if n == -19 {
			// ENODEV (device was disconnected)
			self.connected = false;
			0
		} else if n < 0 {
			self.error = n as _;
			0
		} else if (n as usize) <= buffer.len() {
			n as usize
		} else {
			// this shouldn't happen
			// something messed up
			self.error = -1000;
			0
		}
	}

	// Read a single byte of MIDI input, if there is any.
	// (not exported since you can really screw things up
	//  by mixing this with `read_event`)
	fn read_byte(&mut self) -> Option<u8> {
		let mut buffer = [0u8; 1];
		let n = self.read_raw(&mut buffer);
		if n == 1 {
			Some(buffer[0])
		} else {
			None
		}
	}

	/// returns false if the device was disconnected.
	/// if a device is disconnected and reconnected, you need to reopen it.
	/// (its ID should remain the same)
	pub fn is_connected(&self) -> bool {
		self.connected
	}

	/// get the device error if there is one
	#[allow(dead_code)]
	pub fn get_error(&self) -> Option<String> {
		if self.error == 0 {
			return None;
		}
		Some(format!("ALSA error code {}", self.error))
	}

	pub fn clear_error(&mut self) {
		self.error = 0;
	}

	/// read a MIDI event.
	/// things may get screwed up if you aren't careful about
	/// mixing `.read_bytes()` with this.
	/// For simplicity, if there's a read error, this just returns None.
	/// Check `get_error()` to find out more.
	pub fn read_event(&mut self) -> Option<Event> {
		let mut byte = self.read_byte()?;
		if (byte & 0x80) != 0 {
			let status = byte;
			// new status
			self.last_status = status;
			self.buffered_byte = None;

			if (status & 0b11110000) > 0b11000000 {
				return Some(Event::Other { status, data: None });
			}

			byte = self.read_byte()?;
			if (byte & 0x80) != 0 {
				// no data provided for MIDI event which should have data.
				// what can we do...
				return None;
			}
		}

		let channel = self.last_status & 0xf;

		// at this point we have a data byte
		assert!((byte & 0x80) == 0);

		match self.last_status & 0xf0 {
			0x80 | 0x90 | 0xA0 | 0xB0 | 0xE0 => {
				// event with two bytes of data.
				if let Some(data1) = self.buffered_byte {
					// we have both data bytes!
					self.buffered_byte = None;
					let data2 = byte;
					return Some(match self.last_status & 0xf0 {
						0x80 => Event::NoteOff {
							channel,
							note: data1,
							vel: data2,
						},
						0x90 => Event::NoteOn {
							channel,
							note: data1,
							vel: data2,
						},
						0xA0 => Event::NoteAftertouch {
							channel,
							note: data1,
							pressure: data2,
						},
						0xB0 => Event::ControlChange {
							channel,
							controller: data1,
							value: data2,
						},
						0xE0 => {
							let amount = ((data1 as i16) << 7 | (data2 as i16)) - 0x2000;
							Event::PitchBend { channel, amount }
						}
						_ => panic!("what"),
					});
				} else {
					self.buffered_byte = Some(byte);
				}
			}
			0xC0 => {
				return Some(Event::ProgramChange {
					channel,
					program: byte,
				});
			}
			0xD0 => {
				return Some(Event::ChannelAftertouch {
					channel,
					pressure: byte,
				});
			}
			_ => {
				return Some(Event::Other {
					status: self.last_status,
					data: Some(byte),
				});
			}
		}
		None
	}
}

impl Drop for DeviceManager {
	fn drop(&mut self) {
		*DEVICE_MANAGER_EXISTS.lock().unwrap() = false;
	}
}

impl Drop for Device {
	fn drop(&mut self) {
		unsafe {
			// there's not much we can do if this fails.
			snd_rawmidi_close(self.rawmidi);
		}
	}
}
