const PM_DEVICE_ID = 0;

fn pm_note_played(channel, note, vel) {
	pm_play_note(0, note, vel);
}

pm_load_soundfont("/etc/alternatives/default-GM.sf3");
pm_load_preset(0, 299);
