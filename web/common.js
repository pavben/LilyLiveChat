// communication protocol globals
var mySessionId;
var lastInSequence;
var nextOutSequence;

var ajaxCommandQueue;
var ajaxCommandSendInProgress;

resetSession();

function resetSession() {
	mySessionId = null;
	lastInSequence = 0;
	nextOutSequence = 0;
	ajaxCommandQueue = [];
	ajaxCommandSendInProgress = false;
}

function queueAjaxCommand(data) {
	ajaxCommandQueue.push([nextOutSequence++].concat(data));

	if (!ajaxCommandSendInProgress) {
		setTimeout(sendAjaxCommands, 0);
	}
}

function sendAjaxCommands() {
	if (!mySessionId) {
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
	// start with a clean session
	resetSession();
	ajaxJson(
		['NEW', nextOutSequence++],
		function(getSessionIdResponse) {
			if (getSessionIdResponse.sessionId) {
				// set the session ID to use in future requests
				mySessionId = getSessionIdResponse.sessionId;
				log("Got session ID: " + getSessionIdResponse.sessionId);
				// begin long-polling
				ajaxJsonLongPoll();
				// send any AJAX commands that have accumulated before the session was acquired
				sendAjaxCommands();
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

	handleSessionEnded();
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
		url: '/c',
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

function encodeHtml(str) {
	return str.replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;').replace(/"/g, '&quot;').replace(/'/g, '&#039;');
}

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
	OperatorNowTalkingToMessage : 14,
	OperatorReceiveChatMessage : 15,
	OperatorSendChatMessage : 16,
	OperatorEndingChatMessage : 17,
	OperatorChatEndedMessage : 18,
	CustomerChatEndedMessage : 19,
	UnregisteredSelectSiteMessage : 20,
	UnregisteredSiteSelectedMessage : 21,
	UnregisteredSiteInvalidMessage : 22,
	CustomerNoOperatorsAvailableMessage : 23,
	AdminLoginRequestMessage : 24,
	AdminOperatorCreateMessage : 25,
	AdminOperatorReplaceMessage : 26,
	AdminOperatorDeleteMessage : 27,
	AdminLoginSuccessMessage : 28,
	AdminLoginFailedMessage : 29,
	AdminSiteInfoMessage : 30,
	AdminOperatorDetailsStartMessage : 31,
	AdminOperatorDetailsMessage : 32,
	AdminOperatorDetailsEndMessage : 33,
	AdminOperatorCreateSuccessMessage : 34,
	AdminOperatorCreateFailedMessage : 35,
	AdminOperatorReplaceSuccessMessage : 36,
	AdminOperatorReplaceFailedMessage : 37,
    AdminSetSiteNameMessage : 38,
    AdminSetSiteNameSuccessMessage : 39,
	SALoginRequestMessage : 41,
	SASiteCreateMessage : 42,
	SASiteDeleteMessage : 43,
	SAGetSiteInfoMessage : 44,
	SASetExpiryTimestampMessage : 45,
	SALoginSuccessMessage : 46,
	SALoginFailedMessage : 47,
	SASiteCreateSuccessMessage : 48,
	SASiteCreateFailedMessage : 49,
	SASiteDeleteSuccessMessage : 50,
	SASiteDeleteFailedMessage : 51,
	SASiteInfoMessage : 52,
	SASiteInfoFailedMessage : 53,
	SASetExpiryTimestampSuccessMessage : 54,
    AdminOperatorDeleteSuccessMessage : 62,
    AdminOperatorDeleteFailedMessage : 63
};

function initializeAutoGrowingTextArea(chatBox, appendShadowTo) {
	var shadow = $('<div/>').addClass('chatboxshadow').appendTo(appendShadowTo);

	var checkHeight = function() {
		// manually control scrolling as it causes visual glitches
		chatBox.css('overflow-y', 'hidden');
		shadow.css('width', chatBox.width());

		var newContentHtml = chatBox.val().replace(/</g, '&lt;')
			.replace(/>/g, '&gt;')
			.replace(/&/g, '&amp;')
			.replace(/\n$/, '<br/>.')
			.replace(/\n/g, '<br/>')
			.replace(/ {2,}/g, function(space) { return (new Array(space.length).join('&nbsp;')) + ' '; })
			.replace(/^$/g, '.');

		shadow.html(newContentHtml);

		var targetHeight = shadow.height();
		var minHeight = stripPx(chatBox.css('line-height'));
		if (targetHeight > 150) {
			targetHeight = 150;

			// now scrolling will be needed
			chatBox.css('overflow-y', 'auto');
		} else if (targetHeight < minHeight) {
			targetHeight = minHeight;
		}
		chatBox.css('height', targetHeight);
		onResize();
	};
	chatBox.change(checkHeight);
	chatBox.keyup(checkHeight);

	// call it initially to set the initial height
	checkHeight();
}

// tab switcher

var currentTab = null;
var currentTabTarget = undefined;

function changeTabTo(tab, onTabLoadCallback) {
	var alreadyBusy = (currentTabTarget !== undefined);

	currentTabTarget = tab;

	if (!alreadyBusy) {
		if (currentTab) {
			currentTab.fadeOut(300, 0, function() {
				onOldTabGone();
			});
		} else {
			onOldTabGone();
		}
	}

	function onOldTabGone() {
		currentTab = tab;
		if (currentTab === currentTabTarget) {
			currentTabTarget = undefined;
			currentTab.fadeTo(0, 0, function() {
				onResize();
				if (onTabLoadCallback) {
					onTabLoadCallback();
				}
				currentTab.fadeTo(600, 1);
			});
		} else {
			// here we reset currentTabTarget to fail the alreadyBusy check
			var target = currentTabTarget;
			currentTabTarget = undefined;
			changeTabTo(target);
		}
	}
}

function getCurrentTabOrTarget() {
	return (currentTabTarget !== undefined) ? currentTabTarget : currentTab;
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
	name.text(person.name);
	name.css('color', person.color);

	title.text(person.title);

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
		iconCache.addEventListener('load', function() {
			icon.css('background-image', 'url(\'' + iconUrl + '\')');
			icon.fadeTo(500, 1);
		});
		iconCache.addEventListener('error', function() {
			icon.fadeTo(500, 1);
		});
		iconCache.src = iconUrl;
	});
}

// TODO: Support clickable links
function writeMessageToChatLog(name, color, msg, chatLogDiv) {
	var tempDiv = $('<div/>').addClass('chatmsgtext');
	tempDiv.append($('<span/>').css('color', color).text(name + ': '));
	var lines = msg.split('\n');
	if (lines.length > 1) {
		tempDiv.append($('<br/>'));
	}
	for (var i in lines) {
		tempDiv.append($('<span/>').text(lines[i]));
		tempDiv.append($('<br/>'));
	}

	// append tempDiv to chatLogDiv
	chatLogDiv.append(tempDiv);

	/*
	chatLogDiv.append(tempDiv.fadeTo(1, 0.5, function() {
		tempDiv.fadeTo(500, 1);
	}));
	*/

	chatLogWritten(chatLogDiv);
}

function writeInfoTextToChatLog(text, chatlogDiv) {
	chatlogDiv.append($('<div/>').addClass('chatinfotext').text(text));

	chatLogWritten(chatlogDiv);
}

function chatLogWritten(chatlogDiv) {
	var chatlogObject = chatlogDiv[0];
	if (chatlogObject.lastScrollTopTarget && chatlogDiv.scrollTop() >= chatlogObject.lastScrollTopTarget - 30) {
		// if they scroll within 200px of the bottom
		chatlogObject.scrollLock = false;
	}
	else if (chatlogObject.lastScrollTop && chatlogDiv.scrollTop() < chatlogObject.lastScrollTop) {
		// if the user scrolled up the chat log
		chatlogObject.scrollLock = true;
	}

	var scrollTopTarget = getScrollTopTarget(chatlogDiv);

	if (!chatlogObject.scrollLock)
	{
		// here we use a custom "scroll" queue to make sure scrolling does not interfere with other animations
		// we do this because we are using .stop() and clearing the queue, and we only want scroll tasks cleared
		chatlogDiv.stop('scroll', true, false)
		.queue('scroll', function(next) {
			chatlogDiv.animate({scrollTop: scrollTopTarget}, {duration:500, queue:false});
			next();
		})
		.dequeue('scroll');
	}

	chatlogObject.lastScrollTop = chatlogDiv.scrollTop();
	chatlogObject.lastScrollTopTarget = scrollTopTarget;
}

function getScrollTopTarget(theDiv) {
	// scrollHeight of 0 means the div is out of view, so we check for that case to avoid returning a negative
	if (theDiv[0].scrollHeight > 0) {
		return theDiv[0].scrollHeight // start with the total scroll height
			- theDiv.outerHeight() // subtract (height + padding + border)
			+ parseInt(theDiv.css('border-top-width')) // readd the top border
			+ parseInt(theDiv.css('border-bottom-width')) // readd the bottom border
	} else {
		return 0;
	}
}

// NOTE: Do not switch from one miscmessage tab to another! Content is switched BEFORE the fade-out.
function showMiscMessageTab(title, content, buttons) {
	$('#miscmessage_title').text(title);
	$('#miscmessage_content').empty().append(
		content
	).append(
		buttons
	)
	changeTabTo(miscMessageTab);
}

// misc
function stripPx(text) {
	return text.replace('px', '');
}

function log(msg) {
	window.console.log(msg);
}

