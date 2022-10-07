const PM_DEVICE_ID = 0;

fn pm_note_played(channel, note, vel) {
	pm_play_note(channel, note, vel);
}

fn pm_note_released(channel, note, vel) {
	pm_release_note(channel, note);
}

fn pm_pitch_bent(channel, amount) {
	pm_bend_pitch(amount * 100.0);
}

fn pm_control_changed(channel, controller, value) {
	print(`controller change: channel=${channel} controller=${controller} value=${value}`);
	if controller == 64 {
		// pedal down if value < 127.
		pm_set_pedal(value < 127);
	} else if controller == 1 {
		// set master volume
		pm_set_volume(-1, value / 127.0);
	} else if controller == 20 {
		// set metronome
		let bpm = 0.0;
		if value != 0 {
			bpm = round(30.0 + 1.5 * value);
		}
		print("setting metronome to " + bpm.to_int());
		pm_set_metronome(60, bpm, 1.5);
	} else if controller == 50 {
		// start/stop .wav recording
		if value == 127 {
			pm_start_wav_recording();
		} else {
			pm_stop_wav_recording();
		}
	} else if controller == 51 {
		// start/stop .mid recording
		if value == 127 {
			pm_start_midi_recording();
		} else {
			pm_stop_midi_recording();
		}
	}
}

pm_load_soundfont("soundfont.sf3");
pm_load_preset(-1, "grand piano"); // default = piano
pm_load_preset(9, "standard"); // drum pad
pm_load_preset(16, "standard"); // metronome
