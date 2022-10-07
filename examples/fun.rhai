// plays ode to joy, no matter which keys you press

const data = [64, 64, 65, 67, 67, 65, 64, 62, 60, 60, 62, 64, 64, 62, 62,
	64, 64, 65, 67, 67, 65, 64, 62, 60, 60, 62, 64, 62, 60, 60];

fn pm_note_played(channel, note, vel) {
	pm_play_note(0, data[this.i], vel); 
	this.i += 1;
	this.i %= data.len;
}

fn pm_start() {
	this.i = 0;
	pm_load_soundfont("soundfont.sf3");
	pm_load_preset(-1, "grand piano");
}
