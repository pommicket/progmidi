@echo off
cargo build --release
del progmidi-windows.zip
rd /s/q progmidi
mkdir progmidi
cd progmidi
copy ..\README.md
copy ..\target\release\progmidi.exe
copy ..\config.rhai
mkdir examples
copy ..\examples\* examples
cd ..
7z a progmidi-windows.zip progmidi
