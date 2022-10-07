#!/bin/sh

cargo build --release

rm -rf progmidi || exit 1
mkdir progmidi

cp target/release/progmidi progmidi/
cp README.md progmidi/
cp config.rhai progmidi/
cp -r examples progmidi/examples


tar czf progmidi-linux.tar.gz progmidi
