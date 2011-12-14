function liberty_launchChat() {
	var sid = 98323;
	var wW = 641;
	var wH = 641;
	var wL = (window.screen.width - wW) / 2;
	var wT = (window.screen.height - wH) / 3;
	window.open('chat.html', '_blank', 'width=' + wW + ',height=' + wH + ',left=' + wL + ',top=' + wT + ',location=no,menubar=no,status=no,toolbar=no');
}
