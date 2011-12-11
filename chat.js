function liberty_launchChat() {
	var sid = 98323;
	var wW = 659;
	var wH = 428;
	var wL = (window.screen.width - wW) / 2;
	var wT = (window.screen.height - wH) / 3;
	window.open('chat.html', '_blank', 'width=' + wW + ',height=' + wH + ',left=' + wL + ',top=' + wT + ',location=no,menubar=no,status=no,toolbar=no');
}
