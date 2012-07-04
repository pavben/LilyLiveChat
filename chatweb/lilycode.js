//(function() {
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

	// TODO: detect the presence of 'JSON' and load our own if not available

	function ajaxJson(objectToSend, successFunction, failureFunction, timeoutInMillis, encryptionRequired) {
		// set a timeout for the AJAX request
		var xhrTimeout = setTimeout(function() {
			xhr.abort();
			failureFunction(true);
		}, timeoutInMillis);

		function onXhrSuccess(text) {
			// abort the timeout since the request has completed before timing out
			clearTimeout(xhrTimeout);

			try {
				var jsonResponseObject = JSON.parse(text);
				setTimeout(function() {
					successFunction(jsonResponseObject);
				}, 0);
			} catch(ex) {
				// if the parse failed, this is treated as a failure of the request
				failureFunction(false);
			}
		}

		function onXhrFailure() {
			// abort the timeout since the request has completed before timing out
			clearTimeout(xhrTimeout);
			// call the failure callback
			failureFunction(false);
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

		// by default, we use HTTP
		var scheme = 'http';

		// if the current site is being requested with HTTPS or if the caller requested HTTPS, use it
		if (window.location.protocol == 'https:' || encryptionRequired) {
			scheme = 'https';
		}

		// we use POST instead of GET to avoid caching
		xhr.open('POST', scheme + '://lilylivechat.net/vc/' + siteId, true);

		// we're sending JSON in our request
		xhr.setRequestHeader('Content-Type', 'application/json; charset=utf-8');

		xhr.send(JSON.stringify(objectToSend));
	}

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
	// TODO: Set cookie domain using something like window.location.host.match(/([\w]+\.[\w]+)$/)
	function setPrefixedCookie(name, val) {
		var d = new Date();
		d.setTime(d.getTime() + 24 * 60 * 60 * 1000);
		//document.cookie = cookiePrefix + name + '=' + encodeURIComponent(val) + '; domain=lilylivechat.net; path=/; expires=' + d.toGMTString() + ';';
		document.cookie = cookiePrefix + name + '=' + encodeURIComponent(val) + '; path=/; expires=' + d.toGMTString() + ';';
	}
	// END cookie get/set code
	
	// BEGIN session code
	var visitorId = readPrefixedCookie('visitorId');
	var visitorSessionId;
	var lastInSequence;
	var nextOutSequence;

	var ajaxCommandQueue;
	var ajaxCommandSendInProgress;

	resetSession();

	function resetSession() {
		visitorSessionId = null;
		lastInSequence = 0;
		nextOutSequence = 1;
		ajaxCommandQueue = [];
		ajaxCommandSendInProgress = false;
	}

	function sessionEnded() {
		window.console.log("Session ended");
		resetSession();

		//handleSessionEnded();
	}

	function log(x) {
		window.console.log(x);
	}

	var Messages = {
		VMTJoinSuccess : 1,
		VMTSetChatWindowStatus : 2
	};

	function queueAjaxCommand(data) {
		ajaxCommandQueue.push({
			o: nextOutSequence++,
			m: data
		});

		if (!ajaxCommandSendInProgress) {
			setTimeout(sendAjaxCommands, 0);
		}
	}

	function sendAjaxCommands() {
		// these aren't required to queue, but they are required to send
		if (visitorId === null || visitorSessionId === null) {
			return;
		}

		if (!ajaxCommandSendInProgress && ajaxCommandQueue.length > 0) {
			ajaxCommandSendInProgress = true;

			var currentMessageObject = ajaxCommandQueue.shift();

			// add/update visitorId/visitorSessionId
			currentMessageObject.v = visitorId;
			currentMessageObject.s = visitorSessionId;

			/* we now have:
			 * v: ...,      visitorId
			 * s: ...,		visitorSessionId
			 * o: ...,		out sequence
			 * m: [...]		message data
			 */

			ajaxJson(
				currentMessageObject,
				function(data) {
					ajaxCommandSendInProgress = false;
					// immediately after, send the next entry (if any)
					setTimeout(sendAjaxCommands, 0);
				},
				function (isTimeout) {
					log('Send error: isTimeout = ' + isTimeout);
					// re-add the command to the beginning to retry when possible
					ajaxCommandQueue.unshift(currentMessageObject);
					ajaxCommandSendInProgress = false;
					setTimeout(sendAjaxCommands, 1000); // schedule a retry in 1 second
				},
				5000
			);
		}
	}

	function handleMessage(message) {
		messageTypeId = message.shift();
		window.console.log("Msg Type Id: " + messageTypeId);
		log(message);
		switch (messageTypeId) {
			case Messages.VMTJoinSuccess:
				var isOperatorOnline = message[0];

				log('VMTJoinSuccess, operator online = ' + isOperatorOnline);

				queueAjaxCommand([Messages.VMTSetChatWindowStatus, true]);
				break;
			default:
				window.console.log('in handleMessage');
		}
	}

	var consecutiveLongPollFailures = 0;

	function ajaxJsonLongPoll() {
		var longPollRequestObject = { i: lastInSequence };
		if (visitorId !== null) {
			longPollRequestObject.v = visitorId;
		}
		if (visitorSessionId === null) {
			// if we're creating a new session, send siteId
			longPollRequestObject.siteId = siteId;
		} else {
			longPollRequestObject.s = visitorSessionId;
		}

		ajaxJson(
			longPollRequestObject,
			function(data) {
				// since this request succeeded, reset the number of consecutive failures
				consecutiveLongPollFailures = 0;

				window.console.log(data);

				// if the server is asking us to change our visitorId
				if (typeof data.v !== 'undefined') {
					visitorId = data.v;
				}

				// if the server is asking us to change our visitorSessionId
				if (typeof data.s !== 'undefined') {
					visitorSessionId = data.s;
				}

				var messages = data.m;
				for (var i in messages) {
					var message = messages[i];

					if (parseInt(message[0]) <= lastInSequence) {
						alert("lastInSequence is " + lastInSequence + ", but message[0] is " + message[0]);
					}

					lastInSequence = parseInt(message.shift());

					handleMessage(message);
				}

				// terminate long polling if the session has ended
				if (!data.sessionEnded) {
					setTimeout(ajaxJsonLongPoll, 0);
				} else {
					sessionEnded();
				}
			},
			function (isTimeout) {
				window.console.log('Long Poll Error. isTimeout = ' + isTimeout);

				consecutiveLongPollFailures++;
				if (consecutiveLongPollFailures < 10) {
					setTimeout(ajaxJsonLongPoll, 1000); // schedule a retry in 1 second
				} else {
					sessionEnded();
				}
			},
			65000
		);
	}
	// END session code
	
	// start with a clean session
	resetSession();

	// start long polling
	ajaxJsonLongPoll();
//})();
