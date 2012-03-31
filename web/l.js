{
	var siteId = 'virtivia'

	var cne = siteId + '.referrer=';

	function rc() {
		var cs = document.cookie.split('; ');
		for(var i in cs) {
			if(cs[i].indexOf(cne) == 0) {
				return decodeURIComponent(cs[i].substring(cne.length));
			}
		}

		return null;
	}

	if(rc() === null) {
		var d = new Date();
		d.setTime(d.getTime() + 24 * 60 * 60 * 1000);
		document.cookie = cne + encodeURIComponent('http://www.ref|1%1$1&1;1-1;1~1.1') + '; expires=' + d.toGMTString() + '; path=/';
	}

	// CONSIDER: can detect if the previously-opened window is now closed to avoid letting the user accidentally open 5 sessions, but be careful about stuff like popup blockers
	function lilyLiveChat_launch() {
		var wW = 659;
		var wH = 659;
		var wL = (window.screen.width - wW) / 2;
		var wT = (window.screen.height - wH) / 3;
		var oR = rc();
		window.open('launchchat.html' + (oR !== null ? '?originalReferrer=' + encodeURIComponent(oR) : ''), '_blank', 'width=' + wW + ',height=' + wH + ',left=' + wL + ',top=' + wT + ',location=no,menubar=no,status=no,toolbar=no').focus();
	}
}
