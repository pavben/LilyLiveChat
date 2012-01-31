// communication protocol globals
var mySessionId;
var lastInSequence;
var nextOutSequence;

var ajaxCommandQueue;
var ajaxCommandSendInProgress;

resetSession();

function resetSession() {
	mySessionId = null;
	lastInSequence = null;
	nextOutSequence = 0;
	ajaxCommandQueue = [];
	ajaxCommandSendInProgress = false;
}

function queueAjaxCommand(data) {
	if (!mySessionId) {
		log("queueAjaxCommand failed due to !mySessionId");
		return;
	}

	ajaxCommandQueue.push([nextOutSequence++].concat(data));

	if (!ajaxCommandSendInProgress) {
		setTimeout(sendAjaxCommands, 0);
	}
}

function sendAjaxCommands() {
	if (!mySessionId) {
		log("sendAjaxCommands failed due to !mySessionId");
		return;
	}

	if (!ajaxCommandSendInProgress && ajaxCommandQueue.length > 0) {
		ajaxCommandSendInProgress = true;

		var currentCommand = ajaxCommandQueue.shift();

		ajaxJson(
			[mySessionId].concat(currentCommand), // mySessionId,nextOutSequence,...
			function(data) {
				ajaxCommandSendInProgress = false;
				// immediately after, send the next entry (if any)
				setTimeout(sendAjaxCommands, 0);
			},
			function (errorThrown) {
				log("SEND Error: " + errorThrown);
				// re-add the command to the beginning to retry when possible
				ajaxCommandQueue.unshift(currentCommand);
				ajaxCommandSendInProgress = false;
				setTimeout(sendAjaxCommands, 5000); // schedule a retry in 5 seconds
			},
			5000
		);
	}
}

function ajaxJsonGetSessionId(onSuccessCallback, onErrorCallback) {
	ajaxJson(
		['NEW', nextOutSequence++],
		function(getSessionIdResponse) {
			if (getSessionIdResponse.sessionId) {
				// set the session ID to use in future requests
				mySessionId = getSessionIdResponse.sessionId;
				lastInSequence = 0; // initialize this to 0
				log("Got session ID: " + getSessionIdResponse.sessionId);
				// begin long-polling
				ajaxJsonLongPoll();
				// call the callback function
				onSuccessCallback();
			}
		},
		onErrorCallback,
		5000
	);
}

var sessionExpiryTimeout = null;

function sessionEnded() {
	log("Session ended");
	resetSession();

	// TODO: Call a page-specific cleanup function
}

function ajaxJsonLongPoll() {
	if (!mySessionId) {
		log("ajaxJsonLongPoll not allowed due to !mySessionId");
		return;
	}
	ajaxJson(
		[mySessionId, lastInSequence],
		function(data) {
			if (sessionExpiryTimeout) {
				clearTimeout(sessionExpiryTimeout);
			}

			log(data);

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
		function (errorThrown) {
			log("Long Poll Error: " + errorThrown);
			setTimeout(ajaxJsonLongPoll, 3000); // schedule a retry in 3 seconds

			// if there isn't already a session expiry timeout, set one
			if (!sessionExpiryTimeout) {
				sessionExpiryTimeout = setTimeout(function() {
					sessionEnded();
				}, 10000);
			}
		},
		65000
	);
}

function ajaxJson(data, successFunction, errorFunction, timeout) {
	$.ajaxSetup({ scriptCharset: "utf-8", contentType: "application/x-www-form-urlencoded; charset=UTF-8" });
	$.ajax({
		type: "POST",
		url: "http://localhost:9802/c",
		data: uriEncodeArray(data),
		dataType: 'json',
		timeout: timeout,
		success: function(data, textStatus, jqXHR) {
			successFunction(data);
		},
		error: function(request, textStatus, errorThrown) {
			if (errorThrown != "timeout") {
				log("Error is: " + errorThrown);
			}
			errorFunction(errorThrown);
		}
	});
}

function uriEncodeArray(arr) {
	var str = [];
	for (var p in arr)
		str.push(p + "=" + encodeURIComponent(arr[p]));
	return str.join("&");
}
// End of AJAX stuff

function messageTypeToId(messageType) {
	return messageTypeToIdTable[messageType];
}

var Messages = {
	CustomerJoinMessage : 1,
	CustomerInLinePositionMessage : 2,
	CustomerNowTalkingToMessage : 3,
	CustomerSendChatMessage : 4,
	CustomerReceiveChatMessage : 5,
	CustomerEndingChatMessage : 6,
	SomethingWentWrongMessage : 7,
	OperatorLoginRequestMessage : 8,
	OperatorLoginSuccessMessage : 9,
	OperatorLoginFailedMessage : 10,
	OperatorLineStatusDetailsMessage : 11,
	OperatorLineStatusEmptyMessage : 12,
	OperatorAcceptNextChatSessionMessage : 13,
	OperatorNowTalkingToMessage : 14
};

// tab switcher

function changeTabTo(tab) {
	if (currentTab) {
		currentTab.fadeTo(300, 0, function() {
			currentTab.hide();

			onOldTabGone();
		});
	} else {
		onOldTabGone();
	}

	function onOldTabGone() {
		currentTab = tab;
		currentTab.fadeTo(600, 1);

		onResize();
	}
}

function onBasicVCenterResize(tabName, minHeight) {
	// disable scrolling as it causes visual glitches
	$('body').css('overflow-y', 'hidden');
	var newTabHeight = $(window).height();
	if (newTabHeight < minHeight) {
		newTabHeight = minHeight;
		// if the scrollbars are needed, enable them
		$('body').css('overflow-y', 'auto');
	}
	var spaceToFill = newTabHeight - $('#' + tabName + '_middle').outerHeight();
	var newTabTopHeight = Math.floor(spaceToFill / 2);
	var newTabBottomHeight = Math.ceil(spaceToFill / 2); // bottom gets the extra pixel
	$('#' + tabName + '_top').css('height', newTabTopHeight);
	$('#' + tabName + '_bottom').css('height', newTabBottomHeight);
	$('#' + tabName + '_tab').css('height', newTabHeight);
}

// other

function Person(name, color, title, iconUrl) {
	this.name = name;
	this.color = color;
	this.title = title;
	this.iconUrl = iconUrl;
}

function replaceCardTextWith(person, card, name, title) {
	// if card is provided, perform the fadeout & fadein
	// otherwise, change the fields instantly
	if (card) {
		card.fadeTo(100, 0);
	}
	name.html(person.name);
	name.css('color', person.color);

	title.html(person.title);

	if (card) {
		card.fadeTo(1000, 1);
	}
}

function replaceIconWith(iconUrl, icon) {
	var fadeOutTime = 100;

	if (icon.css('background-image') == 'none') {
		fadeOutTime = 0;
	}

	icon.fadeTo(fadeOutTime, 0, function() {
		// when faded to 0, clear the old image
		icon.css('background-image', 'none');

		var iconCache = new Image();
		iconCache.onload = function() {
			icon.css('background-image', 'url(\'' + iconUrl + '\')');
			icon.fadeTo(500, 1);
		}
		iconCache.src = iconUrl;
	});
}

// misc
function stripPx(text) {
	return text.replace('px', '');
}

function log(msg) {
	window.console.log(msg);
}

