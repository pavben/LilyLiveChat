(function() {
	// BEGIN getElementsByClassName
	var getElementsByClassName;
	if (document.getElementsByClassName) {
		getElementsByClassName = function(cn) {
			return document.getElementsByClassName(cn);
		};
	} else {
		getElementsByClassName = function (cn) {
			var rx = new RegExp("(?:^|\\s)" + cn+ "(?:$|\\s)");
			var allT = document.getElementsByTagName("*"), allCN = [], ac="", i = 0, a;
			while (a = allT[i=i+1]) {
				ac=a.className;
				if ( ac && ac.indexOf(cn) !==-1) {
					if(ac===cn){ allCN[allCN.length] = a; continue; }
					rx.test(ac) ? (allCN[allCN.length] = a) : 0;
				}
			}
			return allCN;
		}
	}
	// END getElementsByClassName
	
	// BEGIN originalReferrer cookie code
	var cookieNameAndEq = lilyLiveChat_siteId + '.referrer=';

	var originalReferrer = null;
	{
		var cookieKeyValues = document.cookie.split('; ');
		for(var i in cookieKeyValues) {
			if(cookieKeyValues[i].indexOf(cookieNameAndEq) == 0) {
				originalReferrer = decodeURIComponent(cookieKeyValues[i].substring(cookieNameAndEq.length));
				break;
			}
		}
	}

	if(originalReferrer === null) {
		var d = new Date();
		d.setTime(d.getTime() + 24 * 60 * 60 * 1000);
		document.cookie = cookieNameAndEq + encodeURIComponent(document.referrer) + '; expires=' + d.toGMTString() + '; path=/';
	}
	// END originalReferrer cookie code
	
	// BEGIN code to display the appropriate buttons by class name
	// Parts adapted from jQuery

	// set to true/false and call setChatStatus when the chat status is known
	var g_chatStatus;

	var domContentLoadedCallback;

	// Cleanup functions for the document ready method
	if (document.addEventListener) {
		domContentLoadedCallback = function() {
			document.removeEventListener("DOMContentLoaded", domContentLoadedCallback, false);
			displayChatElements();
		};

	} else if (document.attachEvent) {
		domContentLoadedCallback = function() {
			// Make sure body exists, at least, in case IE gets a little overzealous (ticket #5443).
			if (document.readyState === "complete") {
				document.detachEvent("onreadystatechange", domContentLoadedCallback);
				displayChatElements();
			}
		};
	}

	function setChatStatus(chatStatus) {
		g_chatStatus = chatStatus;

		// Catch cases where $(document).ready() is called after the
		// browser event has already occurred.
		if (document.readyState === "complete") {
			displayChatElements();
		}
		// Mozilla, Opera and webkit nightlies currently support this event
		else if (document.addEventListener) {
			// Use the handy event callback
			document.addEventListener("DOMContentLoaded", domContentLoadedCallback, false);

			// A fallback to window.onload, that will always work
			window.addEventListener("load", displayChatElements, false);

		// If IE event model is used
		} else if (document.attachEvent) {
			// ensure firing before onload,
			// maybe late but safe also for iframes
			document.attachEvent("onreadystatechange", domContentLoadedCallback);

			// A fallback to window.onload, that will always work
			window.attachEvent("onload", function() {
				displayChatElements();
			});
		}
	}

	function displayChatElements() {
		var elementsToDisplay = getElementsByClassName('lilylivechat_' + (g_chatStatus ? 'online' : 'offline'));
		for (var i = 0; i < elementsToDisplay.length; i++) {
			elementsToDisplay[i].style.display = 'block';
		}
	}

	// END code to display the appropriate buttons by class name
	
	// BEGIN get site chat status and call setChatStatus
	var visitorId = '';

	// create the main XHR object
	var xhr = window.XMLHttpRequest ? new window.XMLHttpRequest() : new window.ActiveXObject('Microsoft.XMLHTTP');
	// set a timeout
	var xhrTimeout = setTimeout(function() {
		// on timeout, we consider the chat to be down and abort the request
		setChatStatus(false);
		xhr.abort();
	}, 4000);
	xhr.onreadystatechange = function() {
		if (xhr.readyState == 4) { // 4 is done
			clearTimeout(xhrTimeout); // abort the timeout since the request has completed before timing out
			if (xhr.status == 200) {
				setChatStatus(xhr.responseText == '1');
			} else {
				setChatStatus(false); // if something failed, consider the chat to be offline
			}
		}
	}

	xhr.open('GET', 'http://sl.lilylivechat.net/chatstatus/' + lilyLiveChat_siteId + '/' + visitorId, true);;
	xhr.send(null);
	
	// END get site chat status and call setChatStatus

	// CONSIDER: can detect if the previously-opened window is now closed to avoid letting the user accidentally open 5 sessions, but be careful about stuff like popup blockers
	function lilyLiveChat_launch() {
		var wW = 659;
		var wH = 659;
		var wL = (window.screen.width - wW) / 2;
		var wT = (window.screen.height - wH) / 3;
		window.open('http://sl.lilylivechat.net/launchchat/' + lilyLiveChat_siteId + (originalReferrer ? '?originalReferrer=' + encodeURIComponent(originalReferrer) : ''), '_blank', 'width=' + wW + ',height=' + wH + ',left=' + wL + ',top=' + wT + ',location=no,menubar=no,status=no,toolbar=no').focus();
	}
})();
