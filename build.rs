extern crate cc;

fn main() {
	cc::Build::new().file("src/vorbis.c").compile("stb_vorbis");
}
