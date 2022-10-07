# progmidi

A programmable MIDI keyboard audio synthesizer.

Check out the [releases](https://github.com/pommicket/progmidi/releases)
for Windows and Linux executables.

## what

If you have a MIDI keyboard, or other MIDI input device,
you can use progmidi to play sound with it and generate
.wav and .mid files.

progmidi's behavior is controlled through the config script `config.rhai`,
written in the [Rhai scripting language](https://rhai.rs/book/language/).

You will need a soundfont file. Soundfonts contain samples of various "presets" (instruments).
Both .sf2 and .sf3 files are supported.

Musescore has a very extensive soundfont file.
According to [this web page](https://musescore.org/en/handbook/3/soundfonts-and-sfz-files),
after installing musescore, the soundfont will be located here:

Windows x86 (32-bit) / MuseScore x86: `%ProgramFiles%\MuseScore 3\sound\MuseScore_General.sf3`

Windows x64 (64-bit) / MuseScore x86: `%ProgramFiles(x86)%\MuseScore 3\sound\MuseScore_General.sf3`

Windows x64 (64-bit) / MuseScore x86\_64: `%ProgramFiles%\MuseScore 3\sound\MuseScore_General.sf3`

macOS: `/Applications/MuseScore 3.app/Contents/Resources/sound/MuseScore_General.sf3`

Linux (Ubuntu): `/usr/share/mscore-xxx/sounds/MuseScore_General.sf3` (with xxx being the MuseScore version)

You can either change the path in the `config.rhai` file, or copy (or link) the
file to `soundfont.sf3` in the same directory as `progmidi`.

## scripting

The best way to learn how to use progmidi is to check out the default script
`config.rhai`.

Here is a full description of how your script will interact with progmidi.
Note that these functions and constants all begin with
`pm_` or `PM_`. For compatibility with future versions of progmidi,
it's recommended that you don't define any other functions/variables/etc.
starting with `pm_` or `PM_`.

Below, `i64` is an integer, and `f64` is a floating-point number (number with decimals).
`bool`s are either `true` or `false`.

You can keep track of state between function calls using `this` (see `examples/fun.rhai`).

### user-supplied constants

- `PM_DEVICE_ID: i64` - define this to control which MIDI device is used.
If this is -1 or undefined, then you will be asked which device
to use when you run progmidi. If this is 0, the default device
will be used. Otherwise, the device with ID `PM_DEVICE_ID` will be used.

### user-supplied functions

- `pm_start()` - called when progmidi is started.

- `pm_note_played(channel: i64, note: i64, velocity: i64)` - Called
when a note is played (MIDI "note on" event, or "note on" with velocity 0).
`note` is a number from 0 to 127 — 60±n indicates n semitones above/below middle C.
`velocity` is a number from 1 to 127, indicating how forcefully the
key was struck.
`channel` ranges from 0 to 15, and indicates which portion of the MIDI device
was used (e.g. on my MIDI keyboard, the piano keys are channel 0 and the
drum pad is channel 9).

- `pm_note_released(channel: i64, note: i64, velocity: i64)` - Called
when a note is released (MIDI "note off" event). `note` and `channel` are as in `pm_note_played`.
`velocity` (0 to 127) indicates how forcefully the note was released.
For most keyboards, the `velocity` is always just 0, so it can be ignored.

- `pm_pitch_bent(amount: f32)` - Called when the pitch wheel
is changed. `amount` ranges from -1 to 1.

- `pm_control_changed(channel: i64, controller: i64, value: i64)` - Called
when a "controller" value is changed. Typically these are buttons, pedals,
etc. `controller` ranges from 0 to 127; different buttons have different
`controller` numbers, but typically everything is `channel` 0.

### built-in functions

- `pm_load_soundfont(filename: string)` - Load the sound font with the given file name.

- `pm_print_presets()` - Print a list of presets from the soundfont, together with their indices.

- `pm_load_preset(channel: i64, name: string)` - Load the preset whose name closestly matches 
`name` to channel `channel`. If `channel` is -1, the command applies to all channels. 

- `pm_load_preset(channel: i64, index: i64)` - Load the preset with the given index.

- `pm_play_note(channel: i64, note: i64, velocity: i64)` - Play the
given note on the given channel with the given velocity.

- `pm_set_pedal(down: bool)` - Set `down` to `true`/`false` to enable/disable the sustain pedal.

- `pm_bend_pitch(amount: f64)` - Bend pitch by `amount` cents (100 cents = 1 semitones).

- `pm_set_volume(channel: i64, volume: f64)` - Set the volume (0-1) for the given channel.
If `channel` is -1, the master volume is set.

- `pm_set_metronome(key: i64, bpm: f64)` - Set the metronome to play the given key,
every `60/bpm` seconds. Use `pm_load_preset`/`pm_set_volume` with channel 16 to set
the preset and volume of the metronome.

- `pm_set_release_falloff(falloff: i64)` - When a key is released, the note's volume
decreases according to the release falloff (default 0.1). e.g. if the
release falloff is 0.5, the volume of the note goes down by 50% every second.
If `falloff` = 0, the note immediately cuts off when it is released.
If `falloff` = 1, the note's volume is not affected when it is released.

- `pm_start_midi_recording()` - Start a .mid recording.

- `pm_stop_midi_recording()` - Stop the current .mid recording if there is one. 

- `pm_start_midi_recording()` - Start a .wav recording.

- `pm_stop_midi_recording()` - Stop the current .wav recording if there is one. 

- `pm_print()` - `print` with no added newline.

- `pm_get_time() -> i64` - get timestamp in milliseconds since application was started.

## building from source

You can build progmidi with `cargo build --release`,
or run it with `cargo run --release` (it is *way* slower
without the `--release`, and you may get audio glitches as a result).

## bugs

if you find a bug, please create a github issue.

## license

`progmidi` is hereby dedicated to the public domain .
