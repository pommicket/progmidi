/// SOUNDFONT PARSER.
/// supports .sf2 and .sf3 files.
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

modulators are not currently supported
*/
use std::fs::File;
use std::io::{Read, Seek, Write};

#[derive(Clone, Copy, Debug)]
/// type of soundfont
pub enum FileType {
	/// .sf2 files, as detailed in the SoundFont 2 spec: <http://www.synthfont.com/sfspec24.pdf>
	SF2,
	/// .sf3 files (musescore's vorbis-encoded soundfont format)
	SF3,
}

#[derive(Clone, Debug)]
enum ZoneReference {
	None,
	SampleID(u16),   // for instrument zones
	Instrument(u16), // for preset zones
}

#[derive(Clone, Debug)]
struct Zone {
	key_range: (u8, u8),
	vel_range: (u8, u8),
	start_offset: i32,
	end_offset: i32,
	startloop_offset: i32,
	endloop_offset: i32,
	pan: i16,                 // -1000 = full pan left, 1000 = full pan right
	force_key: i8,            // -1 for no forced key, otherwise input MIDI key is replaced with this
	force_vel: i8,            // -1 for no forced velocity
	initial_attenuation: i16, // in centibels. the spec seems to think this is unsigned, but musescore disagrees.
	tune: i32,                // in cents
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
	#[allow(unused)]
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

#[derive(Debug)]
struct Sample {
	start: u32,
	file_len: u32, // NOT equal to data.len() for vorbis-encoded samples
	startloop: u32,
	endloop: u32,
	sample_rate: u32,
	root_key: u8,
	pitch_correction: i8,
	data: Vec<i16>,
}

/// basic usage:
/// ```
/// let sf = SoundFont::open("soundfont.sf2");
/// for i in 0..sf.preset_count() {
///     println!("{}: {}", i, sf.preset_name(i).unwrap())
/// }
/// sf.load_samples_for_preset(106).expect("oh no");
/// let mut request = sf.request(106, 60, 127);
/// loop {
///     let samples = [0i16; 4096];
///     match sf.add_samples_interlaced(&mut request, &mut samples, 44100.0) {
///         Ok(true) => {},
///         Ok(false) => break,
///         Err(e) => eprintln!("{}", e),
///     }
///     (play samples)
/// }
/// ...
/// ```
pub struct SoundFont {
	file: Option<File>,
	file_type: FileType,
	smpl_offset: u64,
	presets: Vec<Preset>,
	instruments: Vec<Instrument>,
	samples: Vec<Sample>,
	pub name: String,
}

pub enum OpenError {
	IO(std::io::Error),
	NotASoundFont,
	BadSoundFont(String),
}

pub enum SampleError {
	IO(std::io::Error),
	BadPreset,
	NoSamples,
	NoFile,
	Vorbis(String), // vorbis decode error (sf3 only)
}

pub struct SamplesRequest {
	// time note has been held down for, multiplied by frequency
	// so if a note has been played for 2s at 440Hz, this would be 880
	// if a note is played for 2s at 440Hz, then 1s at 110Hz, this is 990
	// storing it like this allows a note to change pitch without causing clicks
	t: f64,

	key: u8,
	vel: u8,
	falloff: f32,
	falloff_speed: f32,
	tune: i32,
	volume: f32,
	zones: Vec<Zone>,
}

mod vorbis {
	use std::ffi::{c_int, c_uchar};
	extern crate libc;

	// stb_vorbis.h seems about 3x faster than libvorbis
	//  (maybe i was using libvorbis wrong?)
	// and much easier to use.
	#[link(name = "stb_vorbis")]
	extern "C" {
		fn stb_vorbis_decode_memory(
			mem: *const c_uchar,
			len: c_int,
			channels: *mut c_int,
			sample_rate: *mut c_int,
			output: *mut *mut i16,
		) -> c_int;
	}

	/// decode vorbis data into PCM
	pub fn decode(data: &[u8]) -> Result<Vec<i16>, String> {
		let mut channels: c_int = 0;
		let mut sample_rate: c_int = 0;
		let mut output: *mut i16 = 0 as _;
		let samples = unsafe {
			stb_vorbis_decode_memory(
				&data[0] as _,
				data.len() as _,
				(&mut channels) as _,
				(&mut sample_rate) as _,
				(&mut output) as _,
			)
		};
		if samples < 0 {
			Err("bad vorbis file".to_string())
		} else {
			let samples = samples as usize;
			let mut vec = Vec::with_capacity(samples);
			for i in 0..samples {
				vec.push(unsafe { *output.add(i) });
			}
			unsafe { libc::free(output as _) };
			Ok(vec)
		}
	}
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

impl From<std::io::Error> for SampleError {
	fn from(err: std::io::Error) -> SampleError {
		SampleError::IO(err)
	}
}

impl From<&SampleError> for String {
	fn from(err: &SampleError) -> String {
		use SampleError::*;
		match err {
			IO(e) => format!("IO error: {}", e),
			BadPreset => "bad preset index".to_string(),
			NoSamples => "no samples".to_string(),
			NoFile => "file is closed, but samples from it are needed".to_string(),
			Vorbis(s) => format!("vorbis error: {}", s),
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

impl Sample {
	// fills in the self.data field if necessary.
	fn get_data(
		&mut self,
		maybe_file: &mut Option<File>,
		file_type: FileType,
		smpl_offset: u64,
	) -> Result<(), SampleError> {
		if self.data.is_empty() {
			match maybe_file {
				None => Err(SampleError::NoFile),
				Some(file) => {
					let offset = match file_type {
						FileType::SF2 => smpl_offset + 2 * self.start as u64,
						FileType::SF3 => smpl_offset + self.start as u64,
					};
					file.seek(std::io::SeekFrom::Start(offset))?;
					let len = self.file_len as usize;
					let mut data8 = vec![0u8; 2 * len];
					file.read_exact(&mut data8)?;
					match file_type {
						FileType::SF2 => {
							self.data = vec![0i16; len];
							for i in 0..len as usize {
								self.data[i] = i16::from_le_bytes([data8[2 * i], data8[2 * i + 1]]);
							}
						}
						FileType::SF3 => {
							self.data = vorbis::decode(&data8).map_err(SampleError::Vorbis)?;
						}
					}
					Ok(())
				}
			}
		} else {
			Ok(())
		}
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
	f.read_exact(&mut bytes)?;
	FourCC::new(bytes[0], bytes[1], bytes[2], bytes[3]).ok_or(OpenError::NotASoundFont)
}

fn read_u8(f: &mut File) -> std::io::Result<u8> {
	let mut bytes = [0; 1];
	f.read_exact(&mut bytes)?;
	Ok(bytes[0])
}

fn read_u16(f: &mut File) -> std::io::Result<u16> {
	let mut bytes = [0; 2];
	f.read_exact(&mut bytes)?;
	Ok(u16::from_le_bytes(bytes))
}

fn read_u32(f: &mut File) -> std::io::Result<u32> {
	let mut bytes = [0; 4];
	f.read_exact(&mut bytes)?;
	Ok(u32::from_le_bytes(bytes))
}

fn read_i8(f: &mut File) -> std::io::Result<i8> {
	Ok(read_u8(f)? as i8)
}

fn bad_sound_font(s: &str) -> OpenError {
	OpenError::BadSoundFont(s.to_string())
}

fn read_utf8_fixed_len(file: &mut File, len: usize) -> Result<String, OpenError> {
	let mut name_vec = vec![0; len];
	file.read_exact(&mut name_vec)?;
	while !name_vec.is_empty() && name_vec[name_vec.len() - 1] == 0 {
		name_vec.pop();
	}
	Ok(String::from_utf8_lossy(&name_vec).to_string())
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

	fn is_global(&self) -> bool {
		// a "global" zone is a template for all other zones.
		// global zones have no instrument or sample ID.
		matches!(self.reference, ZoneReference::None)
	}

	fn contains(&self, key: u8, vel: u8) -> bool {
		if self.is_global() {
			return false;
		}

		key >= self.key_range.0
			&& key <= self.key_range.1
			&& vel >= self.vel_range.0
			&& vel <= self.vel_range.1
	}

	fn distance_to(&self, key: u8, vel: u8) -> u32 {
		let key = key as i32;
		let vel = vel as i32;
		let key0 = self.key_range.0 as i32;
		let key1 = self.key_range.1 as i32;
		let vel0 = self.vel_range.0 as i32;
		let vel1 = self.vel_range.1 as i32;
		use std::cmp::min;
		let key_dist = min((key - key0).abs(), (key - key1).abs()) as u32;
		let vel_dist = min((vel - vel0).abs(), (vel - vel1).abs()) as u32;

		// key matters more than velocity.
		key_dist * 100 + vel_dist
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
			key_range: (0, 0), // not relevant
			vel_range: (0, 0), // not relevant
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

fn read_gen_zone(file: &mut File, zone: &mut Zone, gen_count: u16) -> Result<(), OpenError> {
	for _gen_ndx in 0..gen_count {
		let gen_type = read_u16(file)?;
		let amount_u16 = read_u16(file)?;
		let amount_i16 = amount_u16 as i16;
		let mut amount_range = ((amount_u16 >> 8) as u8, amount_u16 as u8);
		// "LS byte indicates the highest and the MS byte the lowest valid key." (soundfont § 8.1.2)
		// but "TimGM6mb.sf2" seems to disagree. maybe something something endianness.
		amount_range = (
			u8::min(amount_range.0, amount_range.1),
			u8::max(amount_range.0, amount_range.1),
		);

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
			INITIAL_ATTENUATION => zone.initial_attenuation = amount_i16,
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
	Ok(())
}

// reads the ibag or pbag chunk of a soundfont
struct GenIndex {
	obj: u16,
	gen: u16,
}

#[allow(unused)]
struct ModIndex {
	obj: u16,
	r#mod: u16,
}

// read pbag or ibag
fn read_bag_chunk(
	file: &mut File,
	bag_indices: Vec<u16>,
) -> Result<(Vec<GenIndex>, Vec<ModIndex>), OpenError> {
	let mut gen_indices = vec![];
	let mut mod_indices = vec![];
	for obj_ndx in 0..bag_indices.len() - 1 {
		let start_ndx = bag_indices[obj_ndx];
		let end_ndx = bag_indices[obj_ndx + 1];
		for _i in start_ndx..end_ndx {
			let gen_ndx = read_u16(file)?;
			let mod_ndx = read_u16(file)?;
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
		let gen_ndx = read_u16(file)?;
		let mod_ndx = read_u16(file)?;
		gen_indices.push(GenIndex {
			obj: obj_ndx as u16,
			gen: gen_ndx,
		});
		mod_indices.push(ModIndex {
			obj: obj_ndx as u16,
			r#mod: mod_ndx,
		});
	}

	Ok((gen_indices, mod_indices))
}

// read pgen or igen chunk
fn read_gen_zones<Item: SFObject>(
	file: &mut File,
	items: &mut [Item],
	gen_indices: Vec<GenIndex>,
	_mod_indices: Vec<ModIndex>,
) -> Result<(), OpenError> {
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

		read_gen_zone(file, &mut zone, end_gen - start_gen)?;

		if zone.is_global() {
			// this is a global zone. everyone should copy it.
			global_zone = Some(zone.clone());
		} else {
			items[inst_ndx as usize].add_zone(zone);
		}
	}
	Ok(())
}

/// request for sound font samples.
impl SamplesRequest {
	/// `tune` is in cents
	pub fn set_tune(&mut self, tune: i32) {
		self.tune = tune;
	}

	/// 0 = silent, 1 = max volume.
	pub fn set_volume(&mut self, volume: f32) {
		self.volume = volume;
	}

	/// every t seconds, volume will be multiplied by amount ^ t
	/// e.g. when a user lets go of a piano key, you might call `set_falloff(0.01)`
	pub fn set_falloff(&mut self, amount: f32) {
		self.falloff_speed = amount;
	}
}

impl SoundFont {
	/// Open a soundfont.
	/// This does not load any sample data, since that would be slow
	/// (soundfont files can be hundreds of megabytes large).
	/// Instead, a handle to the file is kept open, and whenever samples are needed,
	/// they are loaded from the file, and cached into memory.
	/// If you're only dealing with a few presets, you may want to call
	/// `load_samples_for_preset()` after opening to avoid lag when getting samples.
	pub fn open(filename: &str) -> Result<Self, OpenError> {
		let file_type = if filename.ends_with(".sf3") {
			FileType::SF3
		} else {
			FileType::SF2
		};
		Self::open_file(File::open(filename)?, file_type)
	}

	/// Like `open()` but takes a file instead of a file name.
	pub fn open_file(mut file: File, file_type: FileType) -> Result<Self, OpenError> {
		const RIFF: FourCC = fourcc("RIFF");
		const SFBK: FourCC = fourcc("sfbk");
		const LIST: FourCC = fourcc("LIST");
		const INFO: FourCC = fourcc("INFO");
		const INAM: FourCC = fourcc("INAM");
		const SDTA: FourCC = fourcc("sdta");
		const SMPL: FourCC = fourcc("smpl");
		const PDTA: FourCC = fourcc("pdta");
		const INST: FourCC = fourcc("inst");
		const IBAG: FourCC = fourcc("ibag");
		const IGEN: FourCC = fourcc("igen");
		const IMOD: FourCC = fourcc("imod");
		const SHDR: FourCC = fourcc("shdr");
		const PHDR: FourCC = fourcc("phdr");
		const PGEN: FourCC = fourcc("pgen");
		const PBAG: FourCC = fourcc("pbag");

		let riff = read_fourcc(&mut file)?;
		if riff != RIFF {
			// definitely not a soundfont
			return Err(OpenError::NotASoundFont);
		}

		let _sfbk_size = read_u32(&mut file)?;

		let sfbk = read_fourcc(&mut file)?;
		if sfbk != SFBK {
			// could be a WAV file, for example.
			return Err(OpenError::NotASoundFont);
		}

		// at this point, the file *should* be a soundfont.

		let list = read_fourcc(&mut file)?;
		let info_size = read_u32(&mut file)?;
		let info_end = file.stream_position()? + info_size as u64;
		let info = read_fourcc(&mut file)?;
		if list != LIST || info != INFO {
			return Err(bad_sound_font("no INFO chunk"));
		}

		let mut name = None;

		// read INFO data
		while file.stream_position()? < info_end {
			let chunk_type = read_fourcc(&mut file)?;
			let chunk_size = read_u32(&mut file)?;
			let chunk_end = file.stream_position()? + chunk_size as u64;

			if chunk_type == INAM {
				if chunk_size < 256 {
					let mut data = vec![0; chunk_size as usize];
					file.read_exact(&mut data)?;
					data.pop(); // null terminator
					if let Ok(n) = String::from_utf8(data) {
						name = Some(n);
					}
				}
				if name.is_none() {
					return Err(bad_sound_font("bad INAM"));
				}
			}

			file.seek(std::io::SeekFrom::Start(chunk_end))?;
		}

		let name_unwrapped = match name {
			None => return Err(bad_sound_font("no INAM")),
			Some(n) => n,
		};

		let list = read_fourcc(&mut file)?;
		let sdta_size = read_u32(&mut file)?;
		let sdta_offset = file.stream_position()?;
		let sdta_end = sdta_offset + sdta_size as u64;
		let sdta = read_fourcc(&mut file)?;

		if list != LIST || sdta != SDTA {
			return Err(bad_sound_font("no sdta chunk"));
		}

		let smpl = read_fourcc(&mut file)?;
		let _smpl_size = read_u32(&mut file)?;
		let smpl_offset = file.stream_position()?;

		if smpl != SMPL {
			return Err(bad_sound_font("no smpl chunk"));
		}

		file.seek(std::io::SeekFrom::Start(sdta_end))?;

		let list = read_fourcc(&mut file)?;
		let pdta_size = read_u32(&mut file)?;
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
			let chunk_size = read_u32(&mut file)?;
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

			file.seek(std::io::SeekFrom::Start(chunk_end))?;
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
			file.seek(std::io::SeekFrom::Start(inst.offset))?;
			let inst_count = inst.size / 22;
			if inst_count < 2 || inst.size % 22 != 0 {
				return Err(OpenError::BadSoundFont(format!(
					"bad INST chunk size ({} should be at least 44, and a multiple of 22)",
					inst.size
				)));
			}
			for i in 0..inst_count {
				let name = read_utf8_fixed_len(&mut file, 20)?;
				if name.is_empty() && i != inst_count - 1 {
					return Err(bad_sound_font("instrument with no name."));
				}

				// oddly, musescore's sf3 file has an empty name for the terminal record.
				if i == inst_count - 1 && !name.is_empty() && name != "EOI" {
					return Err(bad_sound_font("no terminal instrument."));
				}

				let bag_ndx = read_u16(&mut file)?;

				instruments.push(Instrument {
					name,
					..Default::default()
				});
				instrument_bag_indices.push(bag_ndx);
			}
		}

		// --- read ibag chunk ---
		file.seek(std::io::SeekFrom::Start(ibag.offset))?;
		// these are vecs of (instrument idx, gen idx)
		//               and (instrument idx, mod idx)
		let (instrument_gen_indices, instrument_mod_indices) =
			read_bag_chunk(&mut file, instrument_bag_indices)?;

		// --- read igen chunk ---
		// annoyingly, the igen chunk appears after the imod chunk, even though you need it first
		// to figure out which modifiers are global.
		file.seek(std::io::SeekFrom::Start(igen.offset))?;
		read_gen_zones(
			&mut file,
			&mut instruments,
			instrument_gen_indices,
			instrument_mod_indices,
		)?;

		// --- read phdr chunk ---
		let mut presets = vec![];
		let mut preset_bag_indices = vec![];
		file.seek(std::io::SeekFrom::Start(phdr.offset))?;
		if phdr.size < 38 * 2 || phdr.size % 38 != 0 {
			return Err(OpenError::BadSoundFont(format!(
				"Bad PHDR size: {}",
				phdr.size
			)));
		}
		for _i in 0..phdr.size / 38 {
			let name = read_utf8_fixed_len(&mut file, 20)?;
			let _preset = read_u16(&mut file)?;
			let _bank = read_u16(&mut file)?;
			let bag_ndx = read_u16(&mut file)?;
			let _library = read_u32(&mut file)?;
			let _genre = read_u32(&mut file)?;
			let _morphology = read_u32(&mut file)?;
			presets.push(Preset {
				name,
				..Default::default()
			});
			preset_bag_indices.push(bag_ndx);
		}

		// --- read pbag chunk ---
		file.seek(std::io::SeekFrom::Start(pbag.offset))?;
		// these are vecs of (preset idx, gen idx)
		//               and (preset idx, mod idx)
		let (preset_gen_indices, preset_mod_indices) =
			read_bag_chunk(&mut file, preset_bag_indices)?;

		// --- read pgen chunk ---
		file.seek(std::io::SeekFrom::Start(pgen.offset))?;
		read_gen_zones(
			&mut file,
			&mut presets,
			preset_gen_indices,
			preset_mod_indices,
		)?;

		// --- read shdr chunk ---
		file.seek(std::io::SeekFrom::Start(shdr.offset))?;
		let samples_count = shdr.size / 46;
		if shdr.size % 46 != 0 || samples_count < 1 {
			return Err(OpenError::BadSoundFont(format!(
				"bad SHDR size ({}) -- should be a multiple of 46",
				shdr.size
			)));
		}
		let mut samples = Vec::with_capacity(samples_count as usize);
		for i in 0..samples_count {
			// a sample
			let sample_name = read_utf8_fixed_len(&mut file, 20)?;
			if i == samples_count - 1 {
				if sample_name.is_empty() || sample_name == "EOS" {
					break;
				} else {
					return Err(bad_sound_font("no terminal sample."));
				}
			}

			let start = read_u32(&mut file)?;
			let end = read_u32(&mut file)?;
			if end < start {
				return Err(OpenError::BadSoundFont(format!(
					"sample starts at {}, and ends before then (at {})",
					start, end
				)));
			}
			let mut startloop = read_u32(&mut file)?;
			let mut endloop = read_u32(&mut file)?;
			/*
			for sample rates:
			 "If an illegal or impractical value is encountered,
			 the nearest practical value should be used"
			*/
			let sample_rate = read_u32(&mut file)?.clamp(400, 100000);
			let mut original_pitch = read_u8(&mut file)?;
			if original_pitch == 255 {
				// unpitched instrument
				original_pitch = 60;
			}
			let pitch_correction = read_i8(&mut file)?;
			let _sample_link = read_u16(&mut file)?;
			let _sample_type = read_u16(&mut file)?;

			match file_type {
				FileType::SF2 => {
					startloop -= start;
					endloop -= start;
				}
				FileType::SF3 => {
					// seems like sf3 files have startloop, endloop already relative to start.
				}
			}

			let sample = Sample {
				start,
				file_len: end - start,
				startloop,
				endloop,
				sample_rate,
				root_key: original_pitch,
				pitch_correction,
				data: vec![],
			};
			samples.push(sample);
		}

		instruments.pop(); // remove EOI
		presets.pop(); // remove EOP

		// check instrument & sample indices
		for p in presets.iter() {
			for zone in p.zones.iter() {
				if let ZoneReference::Instrument(inst) = zone.reference {
					if inst as usize >= instruments.len() {
						return Err(OpenError::BadSoundFont(format!(
							"preset zone references instrument {}, but there are only {} instruments.",
							inst, instruments.len())));
					}
				}
			}
		}
		for i in instruments.iter() {
			for zone in i.zones.iter() {
				if let ZoneReference::SampleID(sample) = zone.reference {
					if sample as usize >= samples.len() {
						return Err(OpenError::BadSoundFont(format!(
							"instrument zone references sample {}, but there are only {} samples.",
							sample,
							samples.len()
						)));
					}
				}
			}
		}

		Ok(SoundFont {
			file: Some(file),
			file_type,
			smpl_offset,
			name: name_unwrapped,
			instruments,
			samples,
			presets,
		})
	}

	/// loads all sample data for the given preset into memory.
	/// you can use `clear_cache()` to unload them.
	/// this can take a while -- musescore's sf3 file has 100MB of Grand Piano samples (when decoded).
	/// in that case, there's really no good option, since loading them as needed is also slow
	#[allow(unused)]
	pub fn load_samples_for_preset(&mut self, preset_idx: usize) -> Result<(), SampleError> {
		if preset_idx >= self.presets.len() {
			return Err(SampleError::BadPreset);
		}

		let preset = &self.presets[preset_idx];
		for pzone in preset.zones.iter() {
			if let ZoneReference::Instrument(inst) = pzone.reference {
				for izone in self.instruments[inst as usize].zones.iter() {
					if let ZoneReference::SampleID(sample) = izone.reference {
						self.samples[sample as usize].get_data(
							&mut self.file,
							self.file_type,
							self.smpl_offset,
						)?;
					}
				}
			}
		}
		Ok(())
	}

	/// close the input file.
	/// no more samples can be loaded.
	/// before calling this function, call `load_samples_for_preset()` with the preset(s) you want to use.
	/// do not call `clear_cache()` after this function, or you won't be able to get any samples.
	#[allow(unused)]
	pub fn close_file(&mut self) {
		self.file = None;
	}

	/// clears any cached samples.
	/// use with `cache_size()` if you really care about memory usage.
	#[allow(unused)]
	pub fn clear_cache(&mut self) {
		for sample in self.samples.iter_mut() {
			sample.data.clear();
		}
	}

	/// size of all cached samples in bytes.
	#[allow(unused)]
	pub fn cache_size(&self) -> usize {
		let mut total = 0;
		for sample in self.samples.iter() {
			total += sample.data.len() * 2;
		}
		total
	}

	fn get_zones(&self, preset: &Preset, key: u8, vel: u8) -> Vec<Zone> {
		let mut zones = vec![];
		for pzone in preset.zones.iter() {
			if pzone.contains(key, vel) {
				if let ZoneReference::Instrument(i) = pzone.reference {
					let inst = &self.instruments[i as usize];
					for izone in inst.zones.iter() {
						if izone.contains(key, vel) {
							if let ZoneReference::SampleID(_) = izone.reference {
								zones.push(Zone::add(pzone, izone));
							}
						}
					}
				}
			}
		}

		if zones.is_empty() {
			// that didn't work. try finding closest zone(s).
			let mut closest_l = None;
			let mut closest_l_dist = u32::MAX;
			let mut closest_m = None;
			let mut closest_m_dist = u32::MAX;
			let mut closest_r = None;
			let mut closest_r_dist = u32::MAX;
			for pzone in preset.zones.iter() {
				let d1 = pzone.distance_to(key, vel);
				if let ZoneReference::Instrument(i) = pzone.reference {
					let inst = &self.instruments[i as usize];
					for izone in inst.zones.iter() {
						let d2 = izone.distance_to(key, vel);
						let dist = d1 + d2;
						if let ZoneReference::SampleID(_) = izone.reference {
							// seems like musescore's sf3 sets sampleType = mono even when that's not the case
							// (kinda makes sense since you might want to use the same sample for both L+R)
							// so we'll use the pan instead
							let pan = pzone.pan + izone.pan;
							if pan < -100 {
								if dist < closest_l_dist {
									closest_l_dist = dist;
									closest_l = Some(Zone::add(pzone, izone));
								}
							} else if pan > 100 {
								if dist < closest_r_dist {
									closest_r_dist = dist;
									closest_r = Some(Zone::add(pzone, izone));
								}
							} else if dist < closest_m_dist {
								closest_m_dist = dist;
								closest_m = Some(Zone::add(pzone, izone));
							}
						}
					}
				}
			}
			if let Some(m) = closest_m {
				zones.push(m);
			} else if let Some(l) = closest_l {
				if let Some(r) = closest_r {
					zones.push(l);
					zones.push(r);
				}
			}
		}

		zones
	}

	fn _debug_sample_to_file(&mut self, sample: &Sample) {
		let filename = "raw_sample.out";
		println!("Exporting {}Hz sample to {}.", sample.sample_rate, filename);
		let mut out = std::fs::File::create(filename).unwrap();

		for s in sample.data.iter() {
			let bytes = i16::to_le_bytes(*s);
			out.write_all(&bytes).unwrap();
		}
	}

	pub fn _debug_preset_zones(&self, preset_idx: usize) {
		let preset = &self.presets[preset_idx];
		for pzone in preset.zones.iter() {
			println!("{:?}", pzone);
		}
	}

	pub fn _debug_instrument_zones(&self, inst_idx: usize) {
		let inst = &self.instruments[inst_idx];
		for izone in inst.zones.iter() {
			println!("{:?}", izone);
		}
	}

	/// create a new sample request.
	/// this struct is passed to & updated by `add_samples_interlaced()`
	pub fn request(
		&self,
		preset_idx: usize,
		key: u8,
		vel: u8,
	) -> Result<SamplesRequest, SampleError> {
		if preset_idx >= self.presets.len() {
			return Err(SampleError::BadPreset);
		}

		let zones = self.get_zones(&self.presets[preset_idx], key, vel);
		if zones.is_empty() {
			return Err(SampleError::NoSamples);
		}

		Ok(SamplesRequest {
			volume: 1.0,
			key,
			vel,
			tune: 0,
			t: 0.0,
			falloff: 1.0,
			falloff_speed: 1.0,
			zones,
		})
	}

	/// adds sample data to `samples` which is an i16 slice containing samples LRLRLRLR...
	///   (`samples` should have even length)
	/// volume (0 to 1) = volume of max velocity note.
	/// returns `Ok(true)` if the note should still be held. increments `request.hold_time` as needed.
	pub fn add_samples_interlaced(
		&mut self,
		request: &mut SamplesRequest,
		samples: &mut [i16],
		sample_rate: f64,
	) -> Result<bool, SampleError> {
		let key = request.key;
		let vel = request.vel;

		let mut held = false;
		let mut final_t = 0.0;

		for zone in request.zones.iter() {
			let sample = match zone.reference {
				ZoneReference::SampleID(id) => &mut self.samples[id as usize],
				_ => return Err(SampleError::NoSamples),
			};
			sample.get_data(&mut self.file, self.file_type, self.smpl_offset)?;
			let sample_len = sample.data.len();

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
			tune += request.tune;
			let freq_modulation =
				f64::powf(1.0005777895065548 /* 2 ^ (1/1200) */, tune as f64);

			//	if key == 60 && hold_time == 0.0 {
			//		self._debug_sample_to_file(&sample);
			//	}

			let mut this_held = true;

			let pan = zone.pan as f32 * 0.001 + 0.5;

			let velnum = if zone.force_vel != -1 {
				zone.force_vel as u8
			} else {
				vel
			};
			let fvel = (velnum as f32) * (1.0 / 127.0);
			let amplitude = request.volume * fvel * fvel;

			let mut startloop = (zone.startloop_offset as i64 + (sample.startloop as i64)) as u64;
			if startloop > sample_len as u64 {
				// uh this is bad
				startloop = 0;
			}
			let mut endloop = (zone.endloop_offset as i64 + (sample.endloop as i64)) as u64;
			if endloop > sample_len as u64 {
				// uh this is bad
				endloop = sample_len as u64;
			}
			if endloop <= startloop {
				// uh this is bad
				startloop = 0;
				endloop = sample_len as u64;
			}
			let data_start = zone.start_offset.clamp(0, sample_len as _);
			let data_end = (sample_len as i32 + zone.end_offset).clamp(0, sample_len as _);

			/*
			//i've taken initial attenuation out because i dont want it (better to just control the volume).
			//if you want it back in, multiply amplitude by 0.9885530946569389^attenuation = 0.1^(attenuation/200)
			// initial attenuation is in cB, so why 1/200 instead of 1/100?
			//   here's the key:   audio samples measure voltage, not power
			//                          P = V^2 / R
			//                     so a 10x larger sample will have 100x the power (and will be 20dB, not 10dB louder).
				*/

			let mut t = request.t;
			let t_inc = freq_modulation / sample_rate;
			let data = &sample.data[data_start as usize..data_end as usize];
			let tmul = sample.sample_rate as f64;
			let mut falloff = request.falloff;
			let falloff_mul = f32::powf(request.falloff_speed, (1.0 / sample_rate) as f32);
			for i in 0..samples.len() / 2 {
				let mut s_frac = t * tmul;
				let mut s = s_frac as u64;
				s_frac -= s as f64;
				if zone.loops && s >= startloop {
					s = (s - startloop) % (endloop - startloop) + startloop;
				}
				let s = s as usize;
				if s + 1 >= data.len() {
					this_held = false;
					break;
				}
				// interpolate between one sample and the next
				let sample1 = data[s] as f32;
				let sample2 = data[s + 1] as f32;
				let mut sample = sample1 + (sample2 - sample1) * (s_frac as f32);

				sample *= falloff;
				falloff *= falloff_mul;
				samples[2 * i] += (amplitude * sample * (1.0 - pan)) as i16;
				samples[2 * i + 1] += (amplitude * sample * pan) as i16;
				t += t_inc;
			}

			request.falloff = falloff;
			if falloff * request.volume < 1.0 / 32767.0 {
				this_held = false;
			}
			final_t = f64::max(final_t, t);
			held |= this_held;
		}
		request.t = final_t;
		Ok(held)
	}

	/// get the number of presets in this soundfont.
	/// essentially, a preset is an instrument (but instrument
	/// already means something in soundfont terminology...)
	pub fn preset_count(&self) -> usize {
		self.presets.len()
	}

	/// get the name of the given preset.
	pub fn preset_name(&self, idx: usize) -> Option<&str> {
		if idx >= self.presets.len() {
			None
		} else {
			Some(&self.presets[idx].name)
		}
	}
}
