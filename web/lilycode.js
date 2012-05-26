var lilyLiveChat_launch;

(function() {
	// if the main code was loaded multiple times on the same page, ignore the second time
	if (lilyLiveChat_launch !== undefined) {
		return;
	}

	// extract the siteId from the script tag
	var siteId;
	var scriptTags = document.getElementsByTagName('script');
	for (var i = 0; i < scriptTags.length; i++) {
		var siteIdRegexMatchResult = scriptTags[i].src.match(/\/lilycode\/([\w\d]+)$/);

		if (siteIdRegexMatchResult !== null && siteIdRegexMatchResult.length == 2) {
			siteId = siteIdRegexMatchResult[1];
			break;
		}
	}

	if (siteId === undefined) {
		return;
	}

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
	
	// BEGIN cookie get/set code
	var cookiePrefix = 'lily.' + siteId + '.';
	function readPrefixedCookie(name) {
		var cookieNameAndEq = cookiePrefix + name + '=';
		var cookieKeyValues = document.cookie.split('; ');
		for(var i in cookieKeyValues) {
			if(cookieKeyValues[i].indexOf(cookieNameAndEq) == 0) {
				return decodeURIComponent(cookieKeyValues[i].substring(cookieNameAndEq.length));
				break;
			}
		}

		// cookie with that name was not found
		return null;
	}
	function setPrefixedCookie(name, val) {
		var d = new Date();
		d.setTime(d.getTime() + 24 * 60 * 60 * 1000);
		document.cookie = cookiePrefix + name + '=' + encodeURIComponent(val) + '; expires=' + d.toGMTString() + '; path=/';
	}
	// END cookie get/set code
	
	// BEGIN originalReferrer cookie code
	var originalReferrer = readPrefixedCookie('referrer');

	// if this is their first landing on this site, store the referrer
	if (originalReferrer === null) {
		originalReferrer = document.referrer;
		setPrefixedCookie('referrer', originalReferrer);
	}
	// END originalReferrer cookie code
	
	// BEGIN visitorId
	function generateRandomVisitorId() {
		function generateRandomString(length, chars) {
			var result = '';
			for (var i = length; i > 0; --i) {
				result += chars[Math.round(Math.random() * (chars.length - 1))];
			}
			return result;
		}

		return generateRandomString(32, '0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ');
	}

	var visitorId = readPrefixedCookie('visitorId');

	if (visitorId === null) {
		visitorId = generateRandomVisitorId();
		setPrefixedCookie('visitorId', visitorId);
	}
	// END visitorId
	
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

	// set a timeout for the AJAX request
	var xhrTimeout = setTimeout(function() {
		// on timeout, we consider the chat to be down and abort the request
		setChatStatus(false);
		xhr.abort();
	}, 4000);

	function onXhrSuccess(text) {
		clearTimeout(xhrTimeout); // abort the timeout since the request has completed before timing out
		setChatStatus(xhr.responseText == '1');
	}

	function onXhrFailure() {
		clearTimeout(xhrTimeout);
		setChatStatus(false);
	}

	// create the main XHR object
	var xhr;
	if (!window.XDomainRequest)
	{
		// standard XHR for normal browsers
		xhr = window.XMLHttpRequest ? new window.XMLHttpRequest() : new window.ActiveXObject('Microsoft.XMLHTTP');
		xhr.onreadystatechange = function() {
			if (xhr.readyState == 4) { // 4 is done
				if (xhr.status == 200) {
					onXhrSuccess(xhr.responseText);
				} else {
					onXhrFailure();
				}
			}
		}
	} else {
		// XDR for IE9
		xhr = new window.XDomainRequest();
		xhr.onload = function() {
			onXhrSuccess(xhr.responseText);
		}
		xhr.onerror = function() {
			onXhrFailure();
		}
	}

	// we use POST instead of GET to avoid caching
	xhr.open('POST', window.location.protocol + '//sl.lilylivechat.net/chatstatus/' + siteId + '/' + visitorId, true);;
	xhr.send(null);
	
	// END get site chat status and call setChatStatus

	// CONSIDER: can detect if the previously-opened window is now closed to avoid letting the user accidentally open 5 sessions, but be careful about stuff like popup blockers
	lilyLiveChat_launch = function() {
		var wW = 659;
		var wH = 659;
		var wL = (window.screen.width - wW) / 2;
		var wT = (window.screen.height - wH) / 3;
		window.open('http://sl.lilylivechat.net/launchchat/' + siteId + '?visitorId=' + visitorId + '&currentPage=' + window.location.href + (originalReferrer ? '&originalReferrer=' + encodeURIComponent(originalReferrer) : ''), '_blank', 'width=' + wW + ',height=' + wH + ',left=' + wL + ',top=' + wT + ',location=no,menubar=no,status=no,toolbar=no').focus();
	};
})();
