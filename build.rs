extern crate cc;

fn main() {
	println!("cargo:rerun-if-changed=src/vorbis.c");
	cc::Build::new().file("src/vorbis.c").compile("stb_vorbis");
}
