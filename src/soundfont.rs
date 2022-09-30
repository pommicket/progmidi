#![allow(unused)] // @TODO: delete me
use std::fs::File;
use std::io::{Read, Seek};
use std::collections::HashMap;

#[derive(Clone, Debug)]
enum ZoneReference {
	None,
	SampleID(u16), // for instrument zones
	Instrument(u16) // for preset zones
}

impl ZoneReference {
	fn is_none(&self) -> bool {
		match &self {
			ZoneReference::None => true,
			_ => false,
		}
	}
}

#[derive(Clone, Debug)]
struct Zone {
	key_range: (u8, u8),
	vel_range: (u8, u8),
	inst: u32,
	start_offset: i64,
	end_offset: i64,
	startloop_offset: i64,
	endloop_offset: i64,
	pan: i16, // -1000 = full pan left, 1000 = full pan right
	is_global: bool,
	force_key: i8, // -1 for no forced key, otherwise input MIDI key is replaced with this
	force_vel: i8, // -1 for no forced velocity
	initial_attenuation: u16, // in centibels
	tune: i32, // in cents
	reference: ZoneReference,
	loops: bool,
	scale_tuning: u16, // 100 = normal tuning, 50 = each MIDI key is a *quarter* tone, etc.
	force_root_key: i8, // -1 for no forced root key
}

// Instrument and Preset implement this (and nothing else does)
trait SFObject {
	fn add_zone(&mut self, zone: Zone);
}

#[derive(Default)]
struct Instrument {
	name: String,
	zones: Vec<Zone>
}

impl SFObject for Instrument {
	fn add_zone(&mut self, zone: Zone) {
		self.zones.push(zone);
	}
}

#[derive(Default)]
pub struct Preset {
	name: String,
	zones: Vec<Zone>
}

impl SFObject for Preset {
	fn add_zone(&mut self, zone: Zone) {
		self.zones.push(zone);
	}
}


#[derive(Debug)]
struct Sample {
	offset: u32,
	len: u32,
	startloop: u32,
	endloop: u32,
	sample_rate: u32,
	pitch: u8,
	pitch_correction: i8,
	data: Vec<i16>,
}

const FILE_CACHE_CHUNK_SIZE: usize = 4096;

pub struct SoundFont {
	file: Option<File>,
	presets: Vec<Preset>,
	instruments: Vec<Instrument>,
	samples: Vec<Sample>,
	// maps (offset, len) to sample data
	file_cache: HashMap<(u32, u32), Vec<i16>>,
	pub name: String
}

pub enum OpenError {
	IO(std::io::Error),
	NotASoundFont,
	BadSoundFont(String),
}

pub enum PresetError {
	BadPreset,
	IO(std::io::Error),
}

impl From<&OpenError> for String {
	fn from(err: &OpenError) -> String {
		use OpenError::*;
		match err {
			IO(e) => format!("IO error: {}", e),
			NotASoundFont => "not a sound font".to_string(),
			BadSoundFont(s) => format!("bad sound font file: {}", s),
		}
	}
}

impl From<OpenError> for String {
	fn from(err: OpenError) -> String {
		String::from(&err)
	}
}

impl From<&PresetError> for String {
	fn from(err: &PresetError) -> String {
		use PresetError::*;
		match err {
			IO(e) => format!("IO error: {}", e),
			BadPreset => format!("bad preset index"),
		}
	}
}

impl From<PresetError> for String {
	fn from(err: PresetError) -> String {
		String::from(&err)
	}
}

impl From<std::io::Error> for OpenError {
	fn from(err: std::io::Error) -> OpenError {
		OpenError::IO(err)
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

fn read_fourcc(f: &mut File) -> Result<FourCC, OpenError> {
	let mut bytes = [0; 4];
	f.read_exact(&mut bytes);
	FourCC::new(bytes[0], bytes[1], bytes[2], bytes[3]).ok_or(OpenError::NotASoundFont)
}

fn read_u8(f: &mut File) -> u8 {
	let mut bytes = [0; 1];
	f.read_exact(&mut bytes);
	bytes[0]
}

fn read_u16(f: &mut File) -> u16 {
	let mut bytes = [0; 2];
	f.read_exact(&mut bytes);
	u16::from_le_bytes(bytes)
}

fn read_u32(f: &mut File) -> u32 {
	let mut bytes = [0; 4];
	f.read_exact(&mut bytes);
	u32::from_le_bytes(bytes)
}

fn read_i8(f: &mut File) -> i8 {
	read_u8(f) as i8
}

fn bad_sound_font(s: &str) -> OpenError {
	OpenError::BadSoundFont(s.to_string())
}

fn read_utf8_fixed_len(file: &mut File, len: usize, what: &str) -> Result<String, OpenError> {
	let mut name_vec = vec![0; len];
	file.read_exact(&mut name_vec);
	while !name_vec.is_empty() && name_vec[name_vec.len()-1] == 0 {
		name_vec.pop();
	}
	String::from_utf8(name_vec).map_err(
		|_| OpenError::BadSoundFont(format!("invalid UTF-8 in {}", what))
	)
}


impl Zone {
	fn new(inst: u32) -> Self {
		Self {
			key_range: (0, 127),
			vel_range: (0, 127),
			inst,
			start_offset: 0,
			end_offset: 0,
			startloop_offset: 0,
			endloop_offset: 0,
			pan: 0,
			is_global: false,
			force_key: -1,
			force_vel: -1,
			initial_attenuation: 0,
			tune: 0,
			reference: ZoneReference::None,
			loops: false,
			scale_tuning: 100,
			force_root_key: -1,
		}
	}
}

fn read_gen_zone(file: &mut File, zone: &mut Zone, gen_count: u16) {
	for _gen_ndx in 0..gen_count {
		let gen_type = read_u16(file);
		let amount_u16 = read_u16(file);
		let amount_i16 = amount_u16 as i16;
		let amount_range = ((amount_u16 >> 8) as u8, amount_u16 as u8);
		
		mod gen {
			// generators
			// these aren't all of the ones soundfont defines,
			// just the ones I think are relevant+easy enough to implement.
			pub const START_ADDRS_OFFSET: u16 = 0;
			pub const END_ADDRS_OFFSET: u16 = 1;
			pub const STARTLOOP_ADDRS_OFFSET: u16 = 2;
			pub const ENDLOOP_ADDRS_OFFSET: u16 = 3;
			pub const START_ADDRS_COARSE_OFFSET: u16 = 4;
			pub const END_ADDRS_COARSE_OFFSET: u16 = 12;
			pub const PAN: u16 = 17;
			pub const INSTRUMENT: u16 = 41;
			pub const KEY_RANGE: u16 = 43;
			pub const VEL_RANGE: u16 = 44;
			pub const STARTLOOP_ADDRS_COARSE_OFFSET: u16 = 45;
			pub const KEYNUM: u16 = 46;
			pub const VELOCITY: u16 = 47;
			pub const INITIAL_ATTENUATION: u16 = 48;
			pub const ENDLOOP_ADDRS_COARSE_OFFSET: u16 = 50;
			pub const COARSE_TUNE: u16 = 51;
			pub const FINE_TUNE: u16 = 52;
			pub const SAMPLE_ID: u16 = 53;
			pub const SAMPLE_MODES: u16 = 54;
			pub const SCALE_TUNING: u16 = 56;
			pub const OVERRIDING_ROOT_KEY: u16 = 58;
		}
		use gen::*;
		
		match gen_type {
			START_ADDRS_OFFSET => zone.start_offset += amount_i16 as i64,
			START_ADDRS_COARSE_OFFSET => zone.start_offset += (amount_i16 as i64) * 32768,
			END_ADDRS_OFFSET => zone.end_offset += amount_i16 as i64,
			END_ADDRS_COARSE_OFFSET => zone.end_offset += (amount_i16 as i64) * 32768,
			STARTLOOP_ADDRS_OFFSET => zone.startloop_offset += amount_i16 as i64,
			STARTLOOP_ADDRS_COARSE_OFFSET => zone.startloop_offset += (amount_i16 as i64) * 32768,
			ENDLOOP_ADDRS_OFFSET => zone.endloop_offset += amount_i16 as i64,
			ENDLOOP_ADDRS_COARSE_OFFSET => zone.endloop_offset += (amount_i16 as i64) * 32768,
			PAN => zone.pan = amount_i16.clamp(-1000, 1000),
			KEY_RANGE => zone.key_range = amount_range,
			VEL_RANGE => zone.vel_range = amount_range,
			KEYNUM => zone.force_key = amount_i16.clamp(-1, 127) as i8,
			VELOCITY => zone.force_vel = amount_i16.clamp(-1, 127) as i8,
			INITIAL_ATTENUATION => zone.initial_attenuation = amount_u16,
			COARSE_TUNE => zone.tune += (amount_i16 as i32) * 100,
			FINE_TUNE => zone.tune += amount_i16 as i32,
			SAMPLE_ID => zone.reference = ZoneReference::SampleID(amount_u16),
			INSTRUMENT => zone.reference = ZoneReference::Instrument(amount_u16),
			SAMPLE_MODES => zone.loops = (amount_u16 & 1) != 0,
			SCALE_TUNING => zone.scale_tuning = amount_u16,
			OVERRIDING_ROOT_KEY => zone.force_root_key = amount_i16.clamp(-1, 127) as i8,
			//other => println!("OTHER: {}",other),
			_ => {},
		}
		
	}
}

// reads the ibag or pbag chunk of a soundfont
fn read_bag_chunk(file: &mut File, bag_indices: Vec<u16>) -> (Vec<(u32, u16)>, Vec<(u32, u16)>) {
	let mut gen_indices = vec![];
	let mut mod_indices = vec![];
	for inst_ndx in 0..bag_indices.len() - 1 {
		let start_ndx = bag_indices[inst_ndx];
		let end_ndx = bag_indices[inst_ndx+1];
		for i in start_ndx..end_ndx {
			let gen_ndx = read_u16(file);
			let mod_ndx = read_u16(file);
			gen_indices.push((inst_ndx as u32, gen_ndx));
			mod_indices.push((inst_ndx as u32, mod_ndx));
		}
	}
	
	{
		// terminal zone
		let item_ndx = bag_indices.len() as u32;
		let gen_ndx = read_u16(file);
		let mod_ndx = read_u16(file);
		gen_indices.push((item_ndx, gen_ndx));
		mod_indices.push((item_ndx, mod_ndx));
	}
	
	(gen_indices, mod_indices)
}

// read pgen or igen chunk
fn read_gen_zones<Item: SFObject>(file: &mut File, items: &mut Vec<Item>, gen_indices: Vec<(u32, u16)>, mod_indices: Vec<(u32, u16)>) {
	let mut prev_inst_ndx = u32::MAX;
	let mut global_zone: Option<Zone> = None;
	for zone_ndx in 0..gen_indices.len()-1 {
		let (inst_ndx, start_ndx) = gen_indices[zone_ndx];
		let (_, end_ndx) = gen_indices[zone_ndx+1];
		let mut zone = Zone::new(inst_ndx);
		if inst_ndx == prev_inst_ndx {
			if let Some(z) = &global_zone {
				zone = z.clone();
			}
		} else {
			global_zone = None;
		}
		prev_inst_ndx = inst_ndx;
		
		read_gen_zone(file, &mut zone, end_ndx - start_ndx);
		
		if zone.reference.is_none() {
			// this is a global zone
			zone.is_global = true;
			global_zone = Some(zone.clone());
		} else {
			items[inst_ndx as usize].add_zone(zone);
		}
	}
}

impl SoundFont {
	/// Open a soundfont.
	/// Note: SoundFont keeps a handle to the file.
	///       When you've loaded all the presets you need, you can call `close_file()` to close it.
	pub fn open(filename: &str) -> Result<Self, OpenError> {
		const RIFF: FourCC = fourcc("RIFF");
		const SFBK: FourCC = fourcc("sfbk");
		const LIST: FourCC = fourcc("LIST");
		const INFO: FourCC = fourcc("INFO");
		const INAM: FourCC = fourcc("INAM");
		const SDTA: FourCC = fourcc("sdta");
		const PDTA: FourCC = fourcc("pdta");
		const INST: FourCC = fourcc("inst");
		const IBAG: FourCC = fourcc("ibag");
		const IGEN: FourCC = fourcc("igen");
		const IMOD: FourCC = fourcc("imod");
		const SHDR: FourCC = fourcc("shdr");
		const PHDR: FourCC = fourcc("phdr");
		const PGEN: FourCC = fourcc("pgen");
		const PBAG: FourCC = fourcc("pbag");
		
		let mut file = File::open(filename)?;

		let riff = read_fourcc(&mut file)?;
		if riff != RIFF {
			// definitely not a soundfont
			return Err(OpenError::NotASoundFont);
		}

		let _sfbk_size = read_u32(&mut file);

		let sfbk = read_fourcc(&mut file)?;
		if sfbk != SFBK {
			// could be a WAV file, for example.
			return Err(OpenError::NotASoundFont);
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
		let sdta_offset = file.stream_position()?;
		let sdta_end = sdta_offset + sdta_size as u64;
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
		
		struct Chunk {
			offset: u64,
			size: u32
		}
		impl Chunk {
			fn new() -> Self {
				Chunk { offset: 0, size: 0 }
			}
			fn end(&self) -> u64 {
				return self.offset + self.size as u64;
			}
		}
		let mut inst = Chunk::new();
		let mut ibag = Chunk::new();
		let mut igen = Chunk::new();
		let mut imod = Chunk::new();
		let mut shdr = Chunk::new();
		let mut phdr = Chunk::new();
		let mut pbag = Chunk::new();
		let mut pgen = Chunk::new();
		
		// read pdta data
		while file.stream_position()? < pdta_end {
			let chunk_type = read_fourcc(&mut file)?;
			let chunk_size = read_u32(&mut file);
			let chunk_end = file.stream_position()? + chunk_size as u64;
			
			let chunk = Chunk {
				offset: file.stream_position()?,
				size: chunk_size,
			};
			
			match chunk_type {
				INST => inst = chunk,
				IBAG => ibag = chunk,
				IGEN => igen = chunk,
				IMOD => imod = chunk,
				SHDR => shdr = chunk,
				PHDR => phdr = chunk,
				PBAG => pbag = chunk,
				PGEN => pgen = chunk,
				_ => {},
			}
			
			file.seek(std::io::SeekFrom::Start(chunk_end));
		}
		
		if inst.offset == 0 {
			return Err(bad_sound_font("no inst chunk."));
		}
		if ibag.offset == 0 {
			return Err(bad_sound_font("no ibag chunk."));
		}
		if igen.offset == 0 {
			return Err(bad_sound_font("no igen chunk."));
		}
		if imod.offset == 0 {
			return Err(bad_sound_font("no imod chunk."));
		}
		if shdr.offset == 0 {
			return Err(bad_sound_font("no shdr chunk."));
		}
		if phdr.offset == 0 {
			return Err(bad_sound_font("no phdr chunk."));
		}
		
		
		let mut instruments: Vec<Instrument> = vec![];
		let mut instrument_bag_indices: Vec<u16> = vec![];
		
		// --- read inst chunk ---
		{
			file.seek(std::io::SeekFrom::Start(inst.offset));
			loop {
				let name = read_utf8_fixed_len(&mut file, 20, "instrument name")?;
				if name.is_empty() {
					return Err(bad_sound_font("instrument with no name."));
				}
				
				let bag_ndx = read_u16(&mut file);
				
				let is_eoi = name == "EOI";
				
				instruments.push(Instrument { name, ..Default::default() });
				instrument_bag_indices.push(bag_ndx);
				
				if is_eoi {
					// terminal instrument.
					break;
				}
				
				if file.stream_position()? >= inst.end() {
					return Err(bad_sound_font("No terminal instrument."));
				}
			}
		}
		

		// --- read ibag chunk ---
		file.seek(std::io::SeekFrom::Start(ibag.offset));
		// these are vecs of (instrument idx, gen idx)
		//               and (instrument idx, mod idx)
		let (instrument_gen_indices, instrument_mod_indices) = read_bag_chunk(&mut file, instrument_bag_indices);
		
		// --- read igen chunk ---
		// annoyingly, the igen chunk appears after the imod chunk, even though you need it first
		// to figure out which modifiers are global.
		file.seek(std::io::SeekFrom::Start(igen.offset));
		read_gen_zones(&mut file, &mut instruments, instrument_gen_indices, instrument_mod_indices);
		
		// --- read phdr chunk ---
		let mut presets = vec![];
		let mut preset_bag_indices = vec![];
		file.seek(std::io::SeekFrom::Start(phdr.offset));
		if phdr.size < 38 * 2 || phdr.size % 38 != 0 {
			return Err(OpenError::BadSoundFont(format!("Bad PHDR size: {}", phdr.size)));
		}
		for i in 0..phdr.size / 38 {
			let name = read_utf8_fixed_len(&mut file, 20, "preset name")?;
			let preset = read_u16(&mut file);
			let bank = read_u16(&mut file);
			let bag_ndx = read_u16(&mut file);
			let library = read_u32(&mut file);
			let genre = read_u32(&mut file);
			let morphology = read_u32(&mut file);
			presets.push(Preset { name, ..Default::default() });
			preset_bag_indices.push(bag_ndx);
			
		}
		
		
		// --- read pbag chunk ---
		file.seek(std::io::SeekFrom::Start(pbag.offset));
		// these are vecs of (preset idx, gen idx)
		//               and (preset idx, mod idx)
		let (preset_gen_indices, preset_mod_indices) = read_bag_chunk(&mut file, preset_bag_indices);
		
		// --- read pgen chunk ---
		file.seek(std::io::SeekFrom::Start(pgen.offset));
		read_gen_zones(&mut file, &mut presets, preset_gen_indices, preset_mod_indices);
		
		
		// --- read shdr chunk ---
		file.seek(std::io::SeekFrom::Start(shdr.offset));
		let samples_count = shdr.size / 46;
		let mut samples = Vec::with_capacity(samples_count as usize);
		for i in 0..shdr.size / 46 {
			// a sample
			let sample_name = read_utf8_fixed_len(&mut file, 20, "sample name")?;
			if sample_name == "EOS" { break; }
			let start = read_u32(&mut file);
			let end = read_u32(&mut file);
			let startloop = read_u32(&mut file);
			let endloop = read_u32(&mut file);
			/*
			for sample rates:
			 "If an illegal or impractical value is encountered,
			 the nearest practical value should be used"
			*/
			let sample_rate = read_u32(&mut file).clamp(400, 100000);
			let mut original_pitch = read_u8(&mut file);
			if original_pitch == 255 {
				// unpitched instrument
				original_pitch = 60;
			}
			let pitch_correction = read_i8(&mut file);
			let _sample_link = read_u16(&mut file);
			let _sample_type = read_u16(&mut file);
			
			// file offset
			let offset = sdta_offset as u32 + 2 * start;
			
			let sample = Sample {
				offset,
				len: end - start,
				startloop: startloop - start,
				endloop: endloop - start,
				sample_rate,
				pitch: original_pitch,
				pitch_correction,
				data: vec![]
			};
			samples.push(sample);
			
		}
		
		
		instruments.pop(); // remove EOI
		presets.pop(); // remove EOP
		
		
		Ok(SoundFont {
			file: Some(file),
			name: name_unwrapped,
			instruments,
			samples,
			presets,
			file_cache: HashMap::new(),
		})
	}
	
	pub fn preset_count(&self) -> usize {
		self.presets.len()
	}
	
	pub fn preset_name(&self, idx: usize) -> Option<&str> {
		if idx >= self.presets.len() {
			None
		} else {
			Some(&self.presets[idx].name)
		}
	}
	
	pub fn load_preset(&mut self, preset_idx: usize) -> Result<Preset, PresetError> {
		if preset_idx >= self.presets.len() {
			return Err(PresetError::BadPreset);
		}
		let preset = &self.presets[preset_idx];
		// @TODO
	}
	
	pub fn close_file(&mut self) {
		self.file = None;
	}
}

impl Preset {
	/// adds own sample data to `samples`.
	pub fn add_samples(&self, key: u8, vel: u8, hold_time: f64, samples: &mut [i16]) {
	}
}
