/*
IMPORTANT SOUNDFONT TERMINOLOGY:
 a PRESET is a source you can play from, e.g. "Piano", "Harpsichord", "Choir"
 an INSTRUMENT is an internal group of samples which presets refer to
 (the idea here is that you could, e.g., have presets "Piano 1" and "Piano 2" which use the
  same underlying "Piano" instrument, but different settings like pitch correction, etc.)
 an OBJECT is a preset or instrument
 a ZONE is a specially designated interval of keys and velocities.
 the zone controls various settings about how a sample should be played.
 both presets and instruments have zones.
	 a preset zone refers to an instrument
	 an object zone refers to a sample
 a SAMPLE is a block of audio data with some properties of how it should be played
*/

#![allow(unused)] // @TODO: delete me
use std::collections::HashMap;
use std::fs::File;
use std::io::{Read, Seek};

#[derive(Clone, Debug)]
enum ZoneReference {
	None,
	SampleID(u16),   // for instrument zones
	Instrument(u16), // for preset zones
}

impl ZoneReference {
	fn is_none(&self) -> bool {
		matches!(self, ZoneReference::None)
	}
}

#[derive(Clone, Debug)]
struct Zone {
	key_range: (u8, u8),
	vel_range: (u8, u8),
	start_offset: i32,
	end_offset: i32,
	startloop_offset: i32,
	endloop_offset: i32,
	pan: i16, // -1000 = full pan left, 1000 = full pan right
	is_global: bool,
	force_key: i8, // -1 for no forced key, otherwise input MIDI key is replaced with this
	force_vel: i8, // -1 for no forced velocity
	initial_attenuation: u16, // in centibels
	tune: i32,     // in cents
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
	zones: Vec<Zone>,
}

impl SFObject for Instrument {
	fn add_zone(&mut self, zone: Zone) {
		self.zones.push(zone);
	}
}

#[derive(Default)]
pub struct Preset {
	name: String,
	zones: Vec<Zone>,
}

impl SFObject for Preset {
	fn add_zone(&mut self, zone: Zone) {
		self.zones.push(zone);
	}
}

#[derive(Debug, Clone)]
struct Sample {
	start: u32,
	len: u32,
	startloop: u32,
	endloop: u32,
	sample_rate: u32,
	root_key: u8,
	pitch_correction: i8,
}

const FILE_CACHE_CHUNK_SIZE: usize = 4096;

pub struct SoundFont {
	file: File,
	sdta_offset: u64,
	presets: Vec<Preset>,
	instruments: Vec<Instrument>,
	samples: Vec<Sample>,
	// maps (offset, len) to sample data
	file_cache: HashMap<(u32, u32), Vec<i16>>,
	pub name: String,
}

pub enum OpenError {
	IO(std::io::Error),
	NotASoundFont,
	BadSoundFont(String),
}

pub enum SampleError {
	BadPreset,
	NoSamples,
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

impl From<std::io::Error> for OpenError {
	fn from(err: std::io::Error) -> OpenError {
		OpenError::IO(err)
	}
}

impl std::fmt::Debug for OpenError {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
		write!(f, "OpenError({})", String::from(self))
	}
}

impl From<&SampleError> for String {
	fn from(err: &SampleError) -> String {
		use SampleError::*;
		match err {
			IO(e) => format!("IO error: {}", e),
			BadPreset => "bad preset index".to_string(),
			NoSamples => "no samples".to_string(),
		}
	}
}

impl From<SampleError> for String {
	fn from(err: SampleError) -> String {
		String::from(&err)
	}
}

impl std::fmt::Display for SampleError {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
		write!(f, "{}", String::from(self))
	}
}

impl std::fmt::Debug for SampleError {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
		write!(f, "SampleError({})", String::from(self))
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
		None => panic!("bad fourcc"),
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
	while !name_vec.is_empty() && name_vec[name_vec.len() - 1] == 0 {
		name_vec.pop();
	}
	String::from_utf8(name_vec)
		.map_err(|_| OpenError::BadSoundFont(format!("invalid UTF-8 in {}", what)))
}

impl Zone {
	fn new() -> Self {
		Self {
			key_range: (0, 127),
			vel_range: (0, 127),
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

	fn contains(&self, key: u8, vel: u8) -> bool {
		if self.is_global {
			return false;
		}

		key >= self.key_range.0
			&& key <= self.key_range.1
			&& vel >= self.vel_range.0
			&& vel <= self.vel_range.1
	}
	
	// the zone for a note is generated by adding 
	fn add(zone1: &Self, zone2: &Self) -> Self {
		fn add_forced(a: i8, b: i8) -> i8 {
			// the standard isn't really clear about this
			//   but whatever probably doesn't matter
			if a == -1 {
				b
			} else if b == -1 {
				a
			} else {
				a + b
			}
		}
		
		let mut reference = ZoneReference::None;
		if let ZoneReference::SampleID(id) = zone1.reference {
			reference = ZoneReference::SampleID(id);
		}
		if let ZoneReference::SampleID(id) = zone2.reference {
			reference = ZoneReference::SampleID(id);
		}
		
		Self {
			key_range: (0,0), // not relevant
			vel_range: (0,0), // not relevant
			is_global: false, // not relevant
			start_offset: zone1.start_offset + zone2.start_offset,
			end_offset: zone1.end_offset + zone2.end_offset,
			startloop_offset: zone1.startloop_offset + zone2.startloop_offset,
			endloop_offset: zone1.endloop_offset + zone2.endloop_offset,
			pan: zone1.pan + zone2.pan,
			force_key: add_forced(zone1.force_key, zone2.force_key),
			force_vel: add_forced(zone1.force_vel, zone2.force_vel),
			initial_attenuation: zone1.initial_attenuation + zone2.initial_attenuation,
			tune: zone1.tune + zone2.tune,
			reference,
			loops: zone1.loops ^ zone2.loops,
			force_root_key: add_forced(zone1.force_root_key, zone2.force_root_key),
			scale_tuning: zone1.scale_tuning, // it doesn't really make sense to add scale tunings
		}
	}
}

fn read_gen_zone(file: &mut File, zone: &mut Zone, gen_count: u16) {
	for _gen_ndx in 0..gen_count {
		let gen_type = read_u16(file);
		let amount_u16 = read_u16(file);
		let amount_i16 = amount_u16 as i16;
		let mut amount_range = ((amount_u16 >> 8) as u8, amount_u16 as u8);
		// "LS byte indicates the highest and the MS byte the lowest valid key." (soundfont § 8.1.2)
		// but "TimGM6mb.sf2" seems to disagree. maybe something something endianness.
		amount_range = (u8::min(amount_range.0, amount_range.1), u8::max(amount_range.0, amount_range.1));

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
			START_ADDRS_OFFSET => zone.start_offset += amount_i16 as i32,
			START_ADDRS_COARSE_OFFSET => zone.start_offset += (amount_i16 as i32) * 32768,
			END_ADDRS_OFFSET => zone.end_offset += amount_i16 as i32,
			END_ADDRS_COARSE_OFFSET => zone.end_offset += (amount_i16 as i32) * 32768,
			STARTLOOP_ADDRS_OFFSET => zone.startloop_offset += amount_i16 as i32,
			STARTLOOP_ADDRS_COARSE_OFFSET => zone.startloop_offset += (amount_i16 as i32) * 32768,
			ENDLOOP_ADDRS_OFFSET => zone.endloop_offset += amount_i16 as i32,
			ENDLOOP_ADDRS_COARSE_OFFSET => zone.endloop_offset += (amount_i16 as i32) * 32768,
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
			_ => {}
		}
	}
}

// reads the ibag or pbag chunk of a soundfont
struct GenIndex {
	obj: u16,
	gen: u16,
}

struct ModIndex {
	obj: u16,
	r#mod: u16,
}

// read pbag or ibag
fn read_bag_chunk(file: &mut File, bag_indices: Vec<u16>) -> (Vec<GenIndex>, Vec<ModIndex>) {
	let mut gen_indices = vec![];
	let mut mod_indices = vec![];
	for obj_ndx in 0..bag_indices.len() - 1 {
		let start_ndx = bag_indices[obj_ndx];
		let end_ndx = bag_indices[obj_ndx + 1];
		for i in start_ndx..end_ndx {
			let gen_ndx = read_u16(file);
			let mod_ndx = read_u16(file);
			gen_indices.push(GenIndex {
				obj: obj_ndx as u16,
				gen: gen_ndx,
			});
			mod_indices.push(ModIndex {
				obj: obj_ndx as u16,
				r#mod: mod_ndx,
			});
		}
	}

	{
		// terminal zone
		let obj_ndx = bag_indices.len();
		let gen_ndx = read_u16(file);
		let mod_ndx = read_u16(file);
		gen_indices.push(GenIndex {
			obj: obj_ndx as u16,
			gen: gen_ndx,
		});
		mod_indices.push(ModIndex {
			obj: obj_ndx as u16,
			r#mod: mod_ndx,
		});
	}

	(gen_indices, mod_indices)
}

// read pgen or igen chunk
fn read_gen_zones<Item: SFObject>(
	file: &mut File,
	items: &mut [Item],
	gen_indices: Vec<GenIndex>,
	mod_indices: Vec<ModIndex>,
) {
	let mut prev_inst_ndx = u16::MAX;
	let mut global_zone: Option<Zone> = None;
	for zone_ndx in 0..gen_indices.len() - 1 {
		let inst_ndx = gen_indices[zone_ndx].obj;
		let start_gen = gen_indices[zone_ndx].gen;
		let end_gen = gen_indices[zone_ndx + 1].gen;
		let mut zone = Zone::new();
		if inst_ndx == prev_inst_ndx {
			if let Some(z) = &global_zone {
				zone = z.clone();
			}
		} else {
			global_zone = None;
		}
		prev_inst_ndx = inst_ndx;

		read_gen_zone(file, &mut zone, end_gen - start_gen);

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
			size: u32,
		}
		impl Chunk {
			fn new() -> Self {
				Chunk { offset: 0, size: 0 }
			}
			fn end(&self) -> u64 {
				self.offset + self.size as u64
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
				_ => {}
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

				instruments.push(Instrument {
					name,
					..Default::default()
				});
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
		let (instrument_gen_indices, instrument_mod_indices) =
			read_bag_chunk(&mut file, instrument_bag_indices);

		// --- read igen chunk ---
		// annoyingly, the igen chunk appears after the imod chunk, even though you need it first
		// to figure out which modifiers are global.
		file.seek(std::io::SeekFrom::Start(igen.offset));
		read_gen_zones(
			&mut file,
			&mut instruments,
			instrument_gen_indices,
			instrument_mod_indices,
		);

		// --- read phdr chunk ---
		let mut presets = vec![];
		let mut preset_bag_indices = vec![];
		file.seek(std::io::SeekFrom::Start(phdr.offset));
		if phdr.size < 38 * 2 || phdr.size % 38 != 0 {
			return Err(OpenError::BadSoundFont(format!(
				"Bad PHDR size: {}",
				phdr.size
			)));
		}
		for i in 0..phdr.size / 38 {
			let name = read_utf8_fixed_len(&mut file, 20, "preset name")?;
			let preset = read_u16(&mut file);
			let bank = read_u16(&mut file);
			let bag_ndx = read_u16(&mut file);
			let library = read_u32(&mut file);
			let genre = read_u32(&mut file);
			let morphology = read_u32(&mut file);
			presets.push(Preset {
				name,
				..Default::default()
			});
			preset_bag_indices.push(bag_ndx);
		}

		// --- read pbag chunk ---
		file.seek(std::io::SeekFrom::Start(pbag.offset));
		// these are vecs of (preset idx, gen idx)
		//               and (preset idx, mod idx)
		let (preset_gen_indices, preset_mod_indices) =
			read_bag_chunk(&mut file, preset_bag_indices);

		// --- read pgen chunk ---
		file.seek(std::io::SeekFrom::Start(pgen.offset));
		read_gen_zones(
			&mut file,
			&mut presets,
			preset_gen_indices,
			preset_mod_indices,
		);

		// --- read shdr chunk ---
		file.seek(std::io::SeekFrom::Start(shdr.offset));
		let samples_count = shdr.size / 46;
		let mut samples = Vec::with_capacity(samples_count as usize);
		for i in 0..shdr.size / 46 {
			// a sample
			let sample_name = read_utf8_fixed_len(&mut file, 20, "sample name")?;
			if sample_name == "EOS" {
				break;
			}
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


			let sample = Sample {
				start,
				len: end - start,
				startloop: startloop - start,
				endloop: endloop - start,
				sample_rate,
				root_key: original_pitch,
				pitch_correction,
			};
			samples.push(sample);
		}

		instruments.pop(); // remove EOI
		presets.pop(); // remove EOP

		Ok(SoundFont {
			file,
			sdta_offset,
			name: name_unwrapped,
			instruments,
			samples,
			presets,
			file_cache: HashMap::new(),
		})
	}

	pub fn clear_cache(&mut self) {
		self.file_cache.clear();
	}

	fn get_samples(&mut self, start: u32, len: u32) -> &[i16] {
		if self.file_cache.get(&(start, len)).is_none() {
			self.file.seek(std::io::SeekFrom::Start(self.sdta_offset + 2 * start as u64));
			let mut data8 = vec![0u8; 2 * (len as usize)];
			self.file.read_exact(&mut data8);

			let mut data16 = vec![0i16; len as usize];
			for i in 0..len as usize {
				data16[i] = i16::from_le_bytes([data8[2 * i], data8[2 * i + 1]]);
			}

			self.file_cache.insert((start, len), data16);
		}
		self.file_cache.get(&(start, len)).unwrap()
	}

	fn get_zones(&self, preset: &Preset, key: u8, vel: u8) -> Vec<Zone> {
		let mut zones = vec![];
		for pzone in preset.zones.iter() {
			if pzone.contains(key, vel) {
				if let ZoneReference::Instrument(i) = pzone.reference {
					let inst = &self.instruments[i as usize];
					for izone in inst.zones.iter() {
						if izone.contains(key, vel) {
							zones.push(Zone::add(pzone, izone));
						}
					}
				}
			}
		}
		zones
	}

	/// adds sample data to `samples` which is an i16 slice containing samples LRLRLRLR...
	///   (`samples` should have even length)
	/// volume in (0,1) = volume of max velocity note.
	/// returns Ok(true) if the note should still be held
	pub fn add_samples_interlaced(
		&mut self,
		preset_idx: usize,
		volume: f32,
	//	falloff: f32, @TODO
		key: u8,
		vel: u8,
		hold_time: f64,
		samples: &mut [i16],
		sample_rate: f64,
	) -> Result<bool, SampleError> {
		if preset_idx >= self.presets.len() {
			return Err(SampleError::BadPreset);
		}

		let zones = self
			.get_zones(&self.presets[preset_idx], key, vel);
		if zones.len() == 0 {
			return Err(SampleError::NoSamples);
		}
		let mut held = false;
		
		for zone in zones.iter() {
			let sample = match zone.reference {
				ZoneReference::SampleID(id) => self.samples[id as usize].clone(),
				_ => return Err(SampleError::NoSamples),
			};
			
			let mut tune = zone.tune as i32;
			let root_key = if zone.force_root_key != -1 {
					zone.force_root_key as u8
				} else {
					sample.root_key
				};
			let keynum = if zone.force_key != -1 {
					zone.force_key as u8
				} else {
					key
				};
			tune += (keynum as i32 - root_key as i32) * zone.scale_tuning as i32;
			tune += sample.pitch_correction as i32;
			let freq_modulation = f64::powf(1.0005777895065548 /* 2 ^ (1/1200) */, tune as f64);
			
			let data = self.get_samples(sample.start, sample.len);
			
			let mut this_held = true;
			
			let pan = zone.pan as f32 * 0.001 + 0.5;
			
			let velnum = if zone.force_vel != -1 {
					zone.force_vel as u8
				} else {
					vel
				};
			let mut amplitude = volume * (velnum as f32) * (1.0 / 127.0);
			/*
			i've taken initial attenuation out because i dont want it (better to just control the volume).
			if you want it back in:
			//0.9885530946569389 = 0.1^0.005
			// initial attenuation is in cB, so why 0.005 instead of 0.1?
			//  i have no clue. if you do a -10dB gain on audacity,
			//   it decreases all the samples by a factor of sqrt(10), not 10.
			//      some bullshit audio thing or something.
			amplitude *= f32::powf(0.9885530946569389, zone.initial_attenuation as f32);
			*/
			
			let mut t = hold_time;
			for i in 0..samples.len() / 2 {
				let mut s = (t * freq_modulation * sample.sample_rate as f64) as usize;
				if zone.loops {
					// @TODO alter s
				}
				if s >= data.len() {
					this_held = false;
					break;
				}
				let sample = data[s] as f32;
				
				samples[2*i]   += (amplitude * sample * (1.0 - pan)) as i16;
				samples[2*i+1] += (amplitude * sample * pan) as i16;
				t += 1.0 / sample_rate;
			}
			held |= this_held;
		}
		Ok(held)
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
}
