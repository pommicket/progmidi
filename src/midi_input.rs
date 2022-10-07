#[cfg(unix)]
use std::ffi::{c_char, c_int, c_void, CStr, CString};
/// Query and read from MIDI input devices.
/// Basic usage:
/// ```
/// let mut manager = DeviceManager::new().unwrap();
/// let mut devices = manager.list().unwrap();
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
// so much stuff depends on libc so it's not such
// a big deal to add it as a dependency
// (we only really need it for size_t, ssize_t)
extern crate libc;
#[cfg(unix)]
use libc::{free, size_t, ssize_t};

// (snd_rawmidi_t is an opaque struct)
#[cfg(unix)]
type SndRawMidiT = c_void;

/// Opaque device manager type.
pub struct DeviceManager {}

/// Opaque type for device ID.
#[derive(Debug, Clone)]
#[cfg(unix)]
pub struct DeviceID {
	name: String,
}

#[derive(Debug, Clone)]
#[cfg(windows)]
pub struct DeviceID {
	id: u32,
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
	#[cfg(unix)]
	rawmidi: *mut SndRawMidiT,
	#[cfg(windows)]
	hmi: usize,
	#[cfg(windows)]
	name: String, // needed to detect if device is disconnected
	error: i32,
	// used to hold first data byte of two-data-byte MIDI events.
	buffered_byte: Option<u8>,
	last_status: u8,
	#[cfg(unix)]
	connected: bool,
}

/// An error produced by opening a device.
#[derive(Debug)]
pub enum DeviceOpenError {
	/// the device was not found.
	/// this can hapen if it was disconnected between calling `list`
	/// and `open`.
	NotFound(String),
	/// other error
	Other(String),
}

/// A list of available MIDI input devices.
pub struct DeviceList {
	pub devices: Vec<DeviceInfo>,
	pub default: usize,
}

/// Error produced by `DeviceManager::list`
#[derive(Debug, PartialEq, Eq)]
pub enum DeviceListError {
	/// There are no MIDI input devices available
	NoDevices,
	/// Other error
	Other(String),
}

/// A MIDI event.
/// All notes and velocities are from 0 to 127.
/// A note of 60 +/- n indicates n semitones above/below middle C.
#[derive(Debug, Clone, Copy)]
pub enum Event {
	/// a note was pressed (if vel = 0, this is automatically converted to a note off event)
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
	/// sysex messages (& some other system common messages) won't work on windows.
	///  (too much of a pain to implement)
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

#[cfg(unix)]
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

#[cfg(windows)]
mod windows {
	pub use std::collections::VecDeque;
	use std::sync::Mutex;

	#[repr(C)]
	#[derive(Default)]
	pub struct MidiInCapsW {
		pub mid: u16,
		pub pid: u16,
		pub driver_version: u32,
		pub pname: [u16; 32],
		pub support: u32,
	}

	#[link(name = "winmm", kind = "dylib")]
	extern "C" {
		pub fn midiInGetNumDevs() -> u32;
		pub fn midiInGetDevCapsW(device_id: u32, pmic: *mut MidiInCapsW, cbmic: u32) -> u32;
		pub fn midiInOpen(
			phmi: *mut HMidiIn,
			device_id: u32,
			callback: MidiInProc,
			instance: usize,
			flags: u32,
		) -> u32;
		// According to Microsoft:
		// "This function is supported for backward compatibility.
		//  New applications can cast a handle of the device rather than retrieving the device identifier."
		//   ^ what?  this isn't true?? hmi is some random pointer and deviceID is a small integer...
		pub fn midiInGetID(hmi: HMidiIn, pdevice_id: *mut u32) -> u32;
		pub fn midiInClose(hmi: HMidiIn) -> u32;
		pub fn midiInStart(hmi: HMidiIn) -> u32;
		//pub fn midiInStop(hmi: HMidiIn) -> u32;
	}

	pub type MidiInProc =
		extern "C" fn(midi_in: HMidiIn, msg: u32, instance: usize, param1: usize, param2: usize);
	pub type HMidiIn = usize;

	pub const CALLBACK_FUNCTION: u32 = 0x00030000;
	pub const MMSYSERR_BADDEVICEID: u32 = 2;
	pub const MMSYSERR_ALLOCATED: u32 = 4;
	pub const MMSYSERR_NOMEM: u32 = 7;
	pub const MMSYSERR_INVALFLAG: u32 = 10;
	pub const MMSYSERR_INVALPARAM: u32 = 11;

	pub const MM_MIM_DATA: u32 = 0x3C3;

	pub fn midi_in_dev_get_caps(device_id: u32) -> Option<MidiInCapsW> {
		let mut mic = MidiInCapsW {
			..Default::default()
		};
		let result = unsafe {
			midiInGetDevCapsW(
				device_id,
				&mut mic as _,
				std::mem::size_of::<MidiInCapsW>() as u32,
			)
		};
		if result == 0 {
			Some(mic)
		} else {
			None
		}
	}

	// returns u32::MAX on error.
	pub fn midi_in_get_id(hmi: HMidiIn) -> u32 {
		let mut device_id = u32::MAX;
		unsafe { midiInGetID(hmi, (&mut device_id) as *mut u32) };
		device_id
	}

	// returns empty string on error.
	pub fn get_device_name(id: u32) -> String {
		if let Some(mic) = midi_in_dev_get_caps(id) {
			let mut name_len = 0;
			while name_len < mic.pname.len() && mic.pname[name_len] != 0 {
				name_len += 1;
			}
			String::from_utf16_lossy(&mic.pname[..name_len])
		} else {
			String::new()
		}
	}

	pub type MidiQueue = VecDeque<u8>;
	pub static MIDI_QUEUES: Mutex<Vec<(HMidiIn, MidiQueue)>> = Mutex::new(vec![]);
	pub extern "C" fn midi_callback(
		midi_in: HMidiIn,
		msg: u32,
		_instance: usize,
		param1: usize,
		_param2: usize,
	) {
		if msg != MM_MIM_DATA {
			// don't care
			return;
		}

		if let Ok(mut queues) = MIDI_QUEUES.lock() {
			for (dev, queue) in queues.iter_mut() {
				if *dev == midi_in {
					// windows has "helpfully" semi-parsed the MIDI for us.
					// unparse it so we can use the same parsing code across platforms.
					let status = param1 as u8;
					// (clearing top bit of data1,data2 just in case)
					let data1 = ((param1 >> 8) & 0x7f) as u8;
					let data2 = ((param1 >> 16) & 0x7f) as u8;
					queue.push_back(status);
					match status >> 4 {
						0b1000 | 0b1001 | 0b1010 | 0b1011 | 0b1110 => {
							// events with 2 data bytes
							queue.push_back(data1);
							queue.push_back(data2);
						}
						0b1100 | 0b1101 => {
							// events with 1 data byte
							queue.push_back(data1);
						}
						_ => {}
					}
				}
			}
		}
	}
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

impl From<&DeviceListError> for String {
	fn from(e: &DeviceListError) -> String {
		use DeviceListError::*;
		match e {
			NoDevices => "no devices found".to_string(),
			Other(s) => s.clone(),
		}
	}
}

impl From<DeviceListError> for String {
	fn from(e: DeviceListError) -> String {
		String::from(&e)
	}
}

// technically there should be varargs here but oh well
#[cfg(unix)]
pub unsafe extern "C" fn snd_lib_error_handler_quiet(
	_file: *const c_char,
	_line: i32,
	_function: *const c_char,
	_err: i32,
) {
}

// convert cstr to Option<String>. you'll get back None if
// cstr is null.
#[cfg(unix)]
unsafe fn cstr_to_option_string(cstr: *const c_char) -> Option<String> {
	if cstr == 0 as _ {
		return None;
	}

	Some(String::from_utf8_lossy(CStr::from_ptr(cstr).to_bytes()).to_string())
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
	#[allow(unused_variables)] // quiet unused on windows
	pub fn set_quiet(&mut self, quiet: bool) {
		#[cfg(unix)]
		{
			let mut callback: *mut c_void = 0 as _;
			if quiet {
				callback = snd_lib_error_handler_quiet as _;
			}
			unsafe {
				snd_lib_error_set_handler(callback);
			}
		}
	}

	/// Returns a `DeviceList` containing descriptions for all MIDI input devices.
	pub fn list(&mut self) -> Result<DeviceList, DeviceListError> {
		#[cfg(unix)]
		{
			let mut hints: *mut *mut c_void = 0 as _;
			let err: c_int;
			unsafe {
				err = snd_device_name_hint(
					-1,
					"rawmidi\0".as_ptr() as *const c_char,
					(&mut hints) as _,
				);
			}

			// in theory hints should never be null if err != 0.
			// but you can never be sure.
			if err != 0 || hints == 0 as _ {
				return Err(DeviceListError::Other(format!(
					"failed to get device hints (error code {})",
					err
				)));
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

			if devices.is_empty() {
				return Err(DeviceListError::NoDevices);
			}
			Ok(DeviceList {
				devices,
				default: default.unwrap_or(0),
			})
		}
		#[cfg(windows)]
		{
			use windows::*;
			let num_devs = unsafe { midiInGetNumDevs() };
			let mut default = None;
			let mut devices = Vec::with_capacity(num_devs as usize);
			for i in 0..num_devs {
				let name = get_device_name(i);
				if !name.is_empty() {
					// idk what MIDIIN devices are.
					if default.is_none() && !name.starts_with("MIDIIN") {
						default = Some(i);
					}
					devices.push(DeviceInfo {
						name,
						// we'll just use the index for the ID
						// this isn't perfect since someone might
						// disconnect a device between list() and open(),
						// and then this could (maybe?) refer to a different device.
						// we could also use the name as the ID, but that isn't guaranteed
						// to be unique & the user could still disconnect between midiInGetDevCaps and midiInOpen.
						id: DeviceID { id: i },
					});
				}
			}

			if devices.is_empty() {
				return Err(DeviceListError::NoDevices);
			}
			Ok(DeviceList {
				devices,
				default: default.unwrap_or(0) as usize,
			})
		}
	}

	/// Open a device.
	pub fn open(&mut self, id: &DeviceID) -> Result<Device, DeviceOpenError> {
		#[cfg(unix)]
		{
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
			Ok(Device::new(input))
		}
		#[cfg(windows)]
		{
			use windows::*;
			let mut hmi = 0 as HMidiIn;
			let result =
				unsafe { midiInOpen((&mut hmi) as _, id.id, midi_callback, 0, CALLBACK_FUNCTION) };

			if result != 0 {
				let result_str = match result {
					MMSYSERR_BADDEVICEID => {
						return Err(DeviceOpenError::NotFound(format!("{}", id.id)))
					}
					MMSYSERR_ALLOCATED => "MMSYSERR_ALLOCATED".to_string(),
					MMSYSERR_INVALFLAG => "MMSYSERR_INVALFLAG".to_string(),
					MMSYSERR_INVALPARAM => "MMSYSERR_INVALPARAM".to_string(),
					MMSYSERR_NOMEM => "MMSYSERR_NOMEM".to_string(),
					x => format!("{}", x),
				};
				return Err(DeviceOpenError::Other(format!(
					"windows error {}",
					result_str
				)));
			}

			let mut queues = MIDI_QUEUES
				.lock()
				.map_err(|_| DeviceOpenError::Other("failed to lock mutex".to_string()))?;
			queues.push((hmi, VecDeque::new()));
			drop(queues);

			unsafe {
				midiInStart(hmi);
			}

			Ok(Device::new(hmi))
		}
	}
}

impl Device {
	#[cfg(unix)]
	fn new(rawmidi: *mut SndRawMidiT) -> Self {
		Self {
			rawmidi,
			buffered_byte: None,
			error: 0,
			last_status: 0,
			connected: true,
		}
	}

	#[cfg(windows)]
	fn new(hmi: windows::HMidiIn) -> Self {
		let id = windows::midi_in_get_id(hmi);
		Self {
			hmi,
			name: windows::get_device_name(id),
			buffered_byte: None,
			error: 0,
			last_status: 0,
		}
	}

	fn read_raw(&mut self, buffer: &mut [u8]) -> usize {
		#[cfg(unix)]
		{
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

		#[cfg(windows)]
		{
			if let Ok(mut queues) = windows::MIDI_QUEUES.lock() {
				// in theory, this find() should always succeed
				let result = queues.iter_mut().find(|(hmi, _)| *hmi == self.hmi);
				if let Some((_, vec)) = result {
					for (i, out) in buffer.iter_mut().enumerate() {
						match vec.pop_front() {
							Some(x) => *out = x,
							None => return i,
						}
					}
					return buffer.len();
				}
			}
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
	/// (its ID may not remain the same.)
	pub fn is_connected(&self) -> bool {
		#[cfg(unix)]
		{
			self.connected
		}
		#[cfg(windows)]
		{
			// there's no way of telling when a device is connected, it seems.
			// this is the best we can do.
			let name_expected = windows::get_device_name(windows::midi_in_get_id(self.hmi));
			!name_expected.is_empty() && name_expected == self.name
		}
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
	/// mixing `.read_bytes()` with this (i don't know why you would mix them).
	/// For simplicity, if there's a read error, this just returns `None`.
	/// Check `get_error()` to find out more.
	pub fn read_event(&mut self) -> Option<Event> {
		loop {
			let mut byte = self.read_byte()?;
			if (byte & 0x80) != 0 {
				let status = byte;
				// new status
				self.last_status = status;
				self.buffered_byte = None;

				if (status >> 4) == 0b1111 {
					return Some(Event::Other { status, data: None });
				}

				byte = self.read_byte()?;
				if (byte & 0x80) != 0 {
					// no data provided for MIDI event which should have data.
					// what can we do...
					// returning None is bad because it stops the stream.
					return Some(Event::Other {
						status,
						data: Some(byte & 0x7f),
					});
				}
			}

			let channel = self.last_status & 0xf;

			// at this point we have a data byte
			assert!((byte & 0x80) == 0);

			match self.last_status >> 4 {
				0b1000 | 0b1001 | 0b1010 | 0b1011 | 0b1110 => {
					// event with two bytes of data.
					if let Some(data1) = self.buffered_byte {
						// we have both data bytes!
						self.buffered_byte = None;
						let data2 = byte;
						return Some(match self.last_status >> 4 {
							0b1000 => Event::NoteOff {
								channel,
								note: data1,
								vel: data2,
							},
							0b1001 => {
								if data2 == 0 {
									Event::NoteOff {
										channel,
										note: data1,
										vel: data2,
									}
								} else {
									Event::NoteOn {
										channel,
										note: data1,
										vel: data2,
									}
								}
							}
							0b1010 => Event::NoteAftertouch {
								channel,
								note: data1,
								pressure: data2,
							},
							0b1011 => Event::ControlChange {
								channel,
								controller: data1,
								value: data2,
							},
							0b1110 => {
								let amount = (data1 as i16 | (data2 as i16) << 7) - 0x2000;
								Event::PitchBend { channel, amount }
							}
							_ => panic!("what"),
						});
					} else {
						self.buffered_byte = Some(byte);
						continue;
					}
				}
				0b1100 => {
					return Some(Event::ProgramChange {
						channel,
						program: byte,
					});
				}
				0b1101 => {
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
		}
	}
}

impl Drop for DeviceManager {
	fn drop(&mut self) {
		*DEVICE_MANAGER_EXISTS.lock().unwrap() = false;
	}
}

impl Drop for Device {
	fn drop(&mut self) {
		#[cfg(unix)]
		{
			unsafe {
				// there's not much we can do if this fails.
				snd_rawmidi_close(self.rawmidi);
			}
		}
		#[cfg(windows)]
		{
			use windows::*;
			let mut queues = MIDI_QUEUES.lock().unwrap();
			queues.retain(|(hmi, _)| *hmi != self.hmi);
			unsafe {
				midiInClose(self.hmi);
			}
		}
	}
}
