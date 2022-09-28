#![allow(unused)] // @TODO: delete me
use std::fs::File;
use std::io::{Read, Seek};

pub struct SoundFont {
	file: Option<File>,
	pub name: String
}

pub enum Error {
	IO(std::io::Error),
	NotASoundFont,
	BadSoundFont(String),
}

impl From<&Error> for String {
	fn from(err: &Error) -> String {
		use Error::*;
		match err {
			IO(e) => format!("IO error: {}", e),
			NotASoundFont => "not a sound font".to_string(),
			BadSoundFont(s) => format!("bad sound font file: {}", s),
		}
	}
}

impl From<Error> for String {
	fn from(err: Error) -> String {
		String::from(&err)
	}
}

impl From<std::io::Error> for Error {
	fn from(err: std::io::Error) -> Error {
		Error::IO(err)
	}
}

#[derive(PartialEq, Eq)]
struct FourCC(u8, u8, u8, u8);

impl std::fmt::Debug for FourCC {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
		let bytes = [self.0, self.1, self.2, self.3];
		let string = String::from_utf8(Vec::from(bytes)).unwrap();
		write!(f, "FourCC({})", string)
	}
}

impl FourCC {
	const fn new(a: u8, b: u8, c: u8, d: u8) -> Option<Self> {
		if a == 0 || b == 0 || c == 0 || d == 0 {
			return None;
		}
		if a >= 128 || b >= 128 || c >= 128 || d >= 128 {
			return None;
		}
		Some(FourCC(a, b, c, d))
	}
}

const fn fourcc(s: &str) -> FourCC {
	let bytes = s.as_bytes();
	match FourCC::new(bytes[0], bytes[1], bytes[2], bytes[3]) {
		Some(x) => x,
		None => panic!("bad fourcc")
	}
}

fn read_fourcc(f: &mut File) -> Result<FourCC, Error> {
	let mut bytes = [0; 4];
	f.read_exact(&mut bytes);
	FourCC::new(bytes[0], bytes[1], bytes[2], bytes[3]).ok_or(Error::NotASoundFont)
}

fn read_u32(f: &mut File) -> u32 {
	let mut bytes = [0; 4];
	f.read_exact(&mut bytes);
	u32::from_le_bytes(bytes)
}

fn read_u16(f: &mut File) -> u16 {
	let mut bytes = [0; 2];
	f.read_exact(&mut bytes);
	u16::from_le_bytes(bytes)
}

fn bad_sound_font(s: &str) -> Error {
	Error::BadSoundFont(s.to_string())
}

impl SoundFont {
	/// Open a soundfont.
	/// Note: SoundFont keeps a handle to the file.
	///       when you have loaded all the instruments you need,
	///       you can call `close_file()` if you want to close it.
	pub fn open(filename: &str) -> Result<Self, Error> {
		const RIFF: FourCC = fourcc("RIFF");
		const SFBK: FourCC = fourcc("sfbk");
		const LIST: FourCC = fourcc("LIST");
		const INFO: FourCC = fourcc("INFO");
		const INAM: FourCC = fourcc("INAM");
		const SDTA: FourCC = fourcc("sdta");
		const PDTA: FourCC = fourcc("pdta");
		const INST: FourCC = fourcc("inst");
		
		let mut file = File::open(filename)?;

		let riff = read_fourcc(&mut file)?;
		if riff != RIFF {
			// definitely not a soundfont
			return Err(Error::NotASoundFont);
		}

		let _sfbk_size = read_u32(&mut file);

		let sfbk = read_fourcc(&mut file)?;
		if sfbk != SFBK {
			// could be a WAV file, for example.
			return Err(Error::NotASoundFont);
		}

		// at this point, the file *should* be a soundfont.

		let list = read_fourcc(&mut file)?;
		let info_size = read_u32(&mut file);
		let info_end = file.stream_position()? + info_size as u64;
		let info = read_fourcc(&mut file)?;
		if list != LIST || info != INFO {
			return Err(bad_sound_font("no INFO chunk"));
		}

		let mut name = None;

		// read INFO data
		while file.stream_position()? < info_end {
			let chunk_type = read_fourcc(&mut file)?;
			let chunk_size = read_u32(&mut file);
			let chunk_end = file.stream_position()? + chunk_size as u64;

			if chunk_type == INAM {
				if chunk_size < 256 {
					let mut data = vec![0; chunk_size as usize];
					file.read(&mut data);
					data.pop(); // null terminator
					if let Ok(n) = String::from_utf8(data) {
						name = Some(n);
					}
				}
				if name.is_none() {
					return Err(bad_sound_font("bad INAM"));
				}
			}

			file.seek(std::io::SeekFrom::Start(chunk_end));
		}
		
		let name_unwrapped = match name {
			None => return Err(bad_sound_font("no INAM")),
			Some(n) => n,
		};
		
		let list = read_fourcc(&mut file)?;
		let sdta_size = read_u32(&mut file);
		let sdta_end = file.stream_position()? + sdta_size as u64;
		let sdta = read_fourcc(&mut file)?;
		
		
		if list != LIST || sdta != SDTA {
			return Err(bad_sound_font("no sdta chunk"));
		}
		
		file.seek(std::io::SeekFrom::Start(sdta_end));
		
		let list = read_fourcc(&mut file)?;
		let pdta_size = read_u32(&mut file);
		let pdta_end = file.stream_position()? + pdta_size as u64;
		let pdta = read_fourcc(&mut file)?;		
		if list != LIST || pdta != PDTA {
			return Err(bad_sound_font("no pdta chunk"));
		}
		

		// read pdta data
		while file.stream_position()? < pdta_end {
			let chunk_type = read_fourcc(&mut file)?;
			let chunk_size = read_u32(&mut file);
			let chunk_end = file.stream_position()? + chunk_size as u64;
			
			if chunk_type == INST {
				while file.stream_position()? < chunk_end {
					let mut name_buf = [0; 20];
					file.read_exact(&mut name_buf);
					let mut name_vec = Vec::from(name_buf);
					while !name_vec.is_empty() && name_vec[name_vec.len()-1] == 0 {
						name_vec.pop();
					}
					let name = match String::from_utf8(name_vec) {
						Ok(x) => x,
						Err(_) => return Err(bad_sound_font("invalid UTF-8 in inst name")),
					};
					if name.is_empty() {
						return Err(bad_sound_font("instrument with no name."));
					}
					
					let bag_idx = read_u16(&mut file);
					println!("{:30} ----   {}",name, bag_idx);
				}
			}
			
			file.seek(std::io::SeekFrom::Start(chunk_end));
		}
		
		Ok(SoundFont {
			file: Some(file),
			name: name_unwrapped,
		})
	}

	pub fn close_file(&mut self) {
		self.file = None;
	}
}
