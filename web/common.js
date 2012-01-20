// communication protocol globals
var mySessionId = 'NEW';
var lastInSequence = null;
var nextOutSequence = 0;

var ajaxCommandQueue = [];
var ajaxCommandSendInProgress = false;

function queueAjaxCommand(data) {
	if (!mySessionId) {
		log("queueAjaxCommand not allowed due to !mySessionId");
		return;
	}

	ajaxCommandQueue.push([nextOutSequence++].concat(data));

	if (!ajaxCommandSendInProgress) {
		setTimeout(sendAjaxCommands, 0);
	}
}

function sendAjaxCommands() {
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
			}
		);
	}
}

function ajaxJsonLongPoll() {
	if (!mySessionId) {
		log("ajaxJsonLongPoll not allowed due to !mySessionId");
		return;
	}
	ajaxJson(
		[mySessionId, lastInSequence],
		function(data) {
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
				alert("Session ended");
			}
		},
		function (errorThrown) {
			log("Long Poll Error: " + errorThrown);
			if (errorThrown == 'timeout') {
				setTimeout(ajaxJsonLongPoll, 0);
			}
			else {
				setTimeout(ajaxJsonLongPoll, 5000); // schedule a retry in 5 seconds
			}
		}
	);
}

function ajaxJson(data, successFunction, errorFunction) {
	$.ajaxSetup({ scriptCharset: "utf-8", contentType: "application/x-www-form-urlencoded; charset=UTF-8" });
	$.ajax({
		type: "POST",
		url: "http://localhost:9802/c",
		data: uriEncodeArray(data),
		dataType: 'json',
		timeout: 5 * 1000,
		success: function(data, textStatus, jqXHR) {
			successFunction(data);
		},
		error: function(request, textStatus, errorThrown) {
			log("Error is: " + errorThrown);
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

