{
	var cne = lilyLiveChat_siteId + '.referrer=';

	var originalReferrer = null;
	{
		var cs = document.cookie.split('; ');
		for(var i in cs) {
			if(cs[i].indexOf(cne) == 0) {
				originalReferrer = decodeURIComponent(cs[i].substring(cne.length));
				break;
			}
		}
	}

	if(originalReferrer === null) {
		var d = new Date();
		d.setTime(d.getTime() + 24 * 60 * 60 * 1000);
		document.cookie = cne + encodeURIComponent(document.referrer) + '; expires=' + d.toGMTString() + '; path=/';
	}

	// CONSIDER: can detect if the previously-opened window is now closed to avoid letting the user accidentally open 5 sessions, but be careful about stuff like popup blockers
	function lilyLiveChat_launch() {
		var wW = 659;
		var wH = 659;
		var wL = (window.screen.width - wW) / 2;
		var wT = (window.screen.height - wH) / 3;
		window.open('http://' + lilyLiveChat_siteId + '.lilylivechat.net:9000/launchchat' + (originalReferrer ? '?originalReferrer=' + encodeURIComponent(originalReferrer) : ''), '_blank', 'width=' + wW + ',height=' + wH + ',left=' + wL + ',top=' + wT + ',location=no,menubar=no,status=no,toolbar=no').focus();
	}
}
