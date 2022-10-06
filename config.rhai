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
	if controller == 64 {
		// pedal down if value < 127.
		pm_set_pedal(value < 127);
	}
}

pm_load_soundfont("/etc/alternatives/default-GM.sf3");
pm_load_preset(0, 299);
pm_load_preset(9, 102);
