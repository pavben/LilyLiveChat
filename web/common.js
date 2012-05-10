// communication protocol globals
var siteId = null;
{
	var siteIdMatch = document.location.pathname.match(/^\/([\w\d]+)\//);
	if (siteIdMatch !== null && siteIdMatch[1] !== undefined) {
		siteId = siteIdMatch[1];
	}
}
var mySessionId;
var lastInSequence;
var nextOutSequence;

var ajaxCommandQueue;
var ajaxCommandSendInProgress;

resetSession();

function resetSession() {
	mySessionId = null;
	lastInSequence = 0;
	nextOutSequence = 1;
	ajaxCommandQueue = [];
	ajaxCommandSendInProgress = false;
}

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
	if (!mySessionId) {
		return;
	}

	if (!ajaxCommandSendInProgress && ajaxCommandQueue.length > 0) {
		ajaxCommandSendInProgress = true;

		var currentMessageObject = ajaxCommandQueue.shift();

		// add/update the sessionId
		currentMessageObject.s = mySessionId;

		/* we now have:
		 * s: ...,		sessionId
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
			function (errorThrown) {
				log("SEND Error: " + errorThrown);
				// re-add the command to the beginning to retry when possible
				ajaxCommandQueue.unshift(currentMessageObject);
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
		{
			s: ''
		},
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

function sessionEnded() {
	log("Session ended");
	resetSession();

	handleSessionEnded();
}

var consecutiveLongPollFailures = 0;

function ajaxJsonLongPoll() {
	if (!mySessionId) {
		log("ajaxJsonLongPoll not allowed due to !mySessionId");
		return;
	}
	ajaxJson(
		{
			s: mySessionId,
			i: lastInSequence
		},
		function(data) {
			// since this request succeeded, reset the number of consecutive failures
			consecutiveLongPollFailures = 0;

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
			log('Long Poll Error: ' + errorThrown);

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

function ajaxJson(messageObject, successFunction, errorFunction, timeout) {
	$.ajaxSetup({ scriptCharset: "utf-8", contentType: "application/x-www-form-urlencoded; charset=UTF-8" });
	$.ajax({
		type: "POST",
		url: '/c',
		data: JSON.stringify(messageObject),
		contentType: 'application/json; charset=utf-8',
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
	AdminOperatorCreateDuplicateUsernameMessage : 35,
	AdminOperatorReplaceSuccessMessage : 36,
	AdminOperatorReplaceDuplicateUsernameMessage : 37,
	AdminSetSiteNameMessage : 38,
	AdminSetSiteNameSuccessMessage : 39,
	AdminOperatorReplaceInvalidIdMessage : 40,
	AdminSetAdminPasswordMessage : 55,
	AdminSetAdminPasswordSuccessMessage : 56,
	AdminOperatorDeleteSuccessMessage : 62,
	AdminOperatorDeleteFailedMessage : 63
};

function initializeAutoGrowingTextArea(chatBox, appendShadowTo) {
	var shadow = $('<div/>').addClass('chatboxshadow').appendTo(appendShadowTo);

	var checkHeight = function() {
		// manually control scrolling as it causes visual glitches
		chatBox.css('overflow-y', 'hidden');
		shadow.css('width', chatBox.width());

		var previousHeight = chatBox.height();

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

		if (targetHeight != previousHeight) {
			chatBox.css('height', targetHeight);
			onResize();
		}
	};
	bindTextChangeEvents(chatBox, checkHeight);

	// call it initially to set the initial height
	checkHeight();
}

function bindTextChangeEvents(field, checkForChangeFunction) {
	field.bind({
		'input': checkForChangeFunction,
		'paste': checkForChangeFunction,
		'keypress': checkForChangeFunction,
		'keydown': checkForChangeFunction,
		'change': checkForChangeFunction
	});
}

function setFieldValue(field, value) {
	field.val(value)[0].lastKnownValue = value;
}

function onChangeToFieldValue(field, callback) {
	var checkForChange = function() {
		if (field.val() != field[0].lastKnownValue) {
			// update the last known value
			field[0].lastKnownValue = field.val();

			callback(field);
		}
	}

	bindTextChangeEvents(field, checkForChange);
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

function onBasicVCenterResizeMinPadding(tabName, minPaddingHeight) {
	// disable scrolling as it causes visual glitches
	$('body').css('overflow-y', 'hidden');

	var middleHeight = $('#' + tabName + '_middle').outerHeight();
	var topAndBottomHeight = ($(window).height() - middleHeight) / 2; // can be negative
	if (topAndBottomHeight < minPaddingHeight) {
		topAndBottomHeight = minPaddingHeight;
		// if the scrollbars are needed, enable them
		$('body').css('overflow-y', 'auto');
	}
	$('#' + tabName + '_top').css('height', topAndBottomHeight);
	$('#' + tabName + '_bottom').css('height', topAndBottomHeight);
	$('#' + tabName + '_tab').css('height', middleHeight + topAndBottomHeight * 2);
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

var ChunkType = {
	Text : 1,
	Url : 2,
	Email : 3
}

function textMessageToChunks(textMessage) {
	/*
	\b(https?:\/\/www\.|https?:\/\/|www\.)
	(([-\w\d]+\.)+([\w]{2,4})|(\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3}))
	(:\d{1,5})?
	(\/
		(
			(
				[-\w\d+&@#\/%=$?~_\|\.]*
				\([-\w\d+&@#\/%=$?~_\|\.]*\)
				(
					[-\w\d+&@#\/%=$?~_\|]+
					(\.[-\w\d+&@#\/%=$?~_\|]+)*
				)?
			)
			|
			(
				[-\w\d+&@#\/%=$?~_\|]+
				(\.[-\w\d+&@#\/%=$?~_\|]+)*
			)
		)?
	)?
	*/
	var urlRegex = /\b(https?:\/\/www\.|https?:\/\/|www\.)(([-\w\d]+\.)+([\w]{2,4})|(\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3}))(:\d{1,5})?(\/(([-\w\d+&@#\/%=$?~_\|\.]*\([-\w\d+&@#\/%=$?~_\|\.]*\)([-\w\d+&@#\/%=$?~_\|]+(\.[-\w\d+&@#\/%=$?~_\|]+)*)?)|([A-Z]+(\.[-\w\d+&@#\/%=$?~_\|]+)*))?)?/i;
	var emailRegex = /\b[A-Z0-9._%-]+@[A-Z0-9.-]+\.[A-Z]{2,4}\b/i;

	var first = true;
	var tempString = textMessage;
	var chunks = [];

	function addChunk(type, text) {
		chunks.push({ type: type, text: text });
	}

	while (true) {
		var nextMatchPosUrl = tempString.search(urlRegex);
		var nextMatchPosEmail = tempString.search(emailRegex);

		var nextMatchType = null;
		var nextMatchPos = -1;

		if (nextMatchPosUrl >= 0 && (nextMatchPosEmail == -1 || nextMatchPosUrl <= nextMatchPosEmail)) {
			nextMatchType = ChunkType.Url;
			nextMatchPos = nextMatchPosUrl;
		} else if (nextMatchPosEmail >= 0 /* && (nextMatchPosUrl == -1 || nextMatchPosEmail <= nextMatchPosUrl */) {
			nextMatchType = ChunkType.Email;
			nextMatchPos = nextMatchPosEmail;
		}

		if (nextMatchPos >= 0) {
			// the text before the match is considered to be normal text
			var normalTextChunk = tempString.substring(0, nextMatchPos);
			if (normalTextChunk != '') {
				addChunk(ChunkType.Text, normalTextChunk);
			}

			// advance the string past the normal text to the beginning of the match
			tempString = tempString.substring(nextMatchPos);

			var matchLength = tempString.match(nextMatchType == ChunkType.Url ? urlRegex : emailRegex)[0].length;
			addChunk(nextMatchType, tempString.substring(0, matchLength));

			// advance the string past the match
			tempString = tempString.substring(matchLength);
		} else {
			// take the last normal text chunk and push it, if non-empty
			if (tempString != '') {
				addChunk(ChunkType.Text, tempString);
			}
			break;
		}
	}
	
	return chunks;
}

function writeMessageToChatlog(name, color, msg, chatlogDiv) {
	var tempDiv = $('<div/>').addClass('chatmsgtext');
	tempDiv.append($('<span/>').css('color', color).text(name + ': '));
	var lines = msg.split('\n');
	if (lines.length > 1) {
		tempDiv.append($('<br/>'));
	}
	for (var i in lines) {
		var chunks = textMessageToChunks(lines[i]);
		for (var c in chunks) {
			var chunk = chunks[c];
			switch (chunk.type) {
			case ChunkType.Text:
				tempDiv.append($('<span/>').text(chunk.text));
				break;
			case ChunkType.Url:
			case ChunkType.Email:
				var linkPrefix = '';
				if (chunk.type == ChunkType.Url && chunk.text.match(/^https?:\/\//i) === null) {
					linkPrefix = 'http://';
				} else if (chunk.type == ChunkType.Email) {
					linkPrefix = 'mailto:';
				}
				tempDiv.append($('<a/>').attr('href', linkPrefix + chunk.text).attr('target', '_blank').text(chunk.text));
				break;
			}
		}
		if (i < lines.length - 1) {
			tempDiv.append($('<br/>'));
		}
	}

	// append tempDiv to chatlogDiv
	chatlogDiv.append(tempDiv);

	/*
	chatlogDiv.append(tempDiv.fadeTo(1, 0.5, function() {
		tempDiv.fadeTo(500, 1);
	}));
	*/

	chatlogWritten(chatlogDiv);
}

function writeInfoTextToChatlog(text, chatlogDiv) {
	chatlogDiv.append($('<div/>').addClass('chatinfotext').text(text));

	chatlogWritten(chatlogDiv);
}

function chatlogWritten(chatlogDiv) {
	var chatlogObject = chatlogDiv[0];
	if (chatlogObject.lastScrollTopTarget && chatlogDiv.scrollTop() >= chatlogObject.lastScrollTopTarget - 30) {
		// if they scroll near the bottom
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
		.stop(true, false)
		.queue('scroll', function(next) {
			chatlogDiv.animate({scrollTop: scrollTopTarget}, {duration:500, queue:false});
			next();
		})
		.dequeue('scroll');
	}

	chatlogObject.lastScrollTop = chatlogDiv.scrollTop();
	chatlogObject.lastScrollTopTarget = scrollTopTarget;
}

function instantScrollChatlogToBottom(chatlogDiv) {
	chatlogDiv.stop('scroll', true, false)
	.stop(true, false)
	
	chatlogDiv.scrollTop(getScrollTopTarget(chatlogDiv));
}

function getScrollTopTarget(theDiv) {
	if (!theDiv) {
		// BUG: When clicking on the next in line icon as operator: SCRIPT5007: Unable to get value of the property 'scrollHeight': object is null or undefined 
		alert('getScrollTopTarget scrollHeight bug still present');
		return 0;
	}
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

// icons and suffixes table

var iconsAndSuffixes = [
	['/images/cc/batty.png', 'Bat'],
	['/images/cc/bird.png', 'Bird'],
	['/images/cc/dog.png', 'Dog'],
	['/images/cc/froggy.png', 'Frog'],
	['/images/cc/kitty.png', 'Cat'],
	['/images/cc/lion.png', 'Lion'],
	['/images/cc/panda.png', 'Panda'],
	['/images/cc/penguin.png', 'Penguin']
];

function generatePersonColor() {
	var lowOffset = 50;
	var highOffset = 100;

	return ('#' + getRandomComponent() + getRandomComponent() + getRandomComponent());

	function getRandomComponent() {
		return (0x100 +
			(
				Math.floor(lowOffset + Math.random() * (256 - lowOffset - highOffset)) / 256 * 0xff
			)
		).toString(16).substr(1,2);
	}
}

// misc
function getUrlParameter(name, queryString) {
	queryString = (queryString !== undefined) ? queryString : location.search;
	var v = (RegExp(name + '=' + '(.*?)(&|$)').exec(queryString)||[,null])[1];
	return (v !== null) ? decodeURIComponent(v) : v;
}

function refreshThroughSiteLocator() {
	window.location = 'http://sl.lilylivechat.net' + window.location.pathname + window.location.search;
}

function stripPx(text) {
	return text.replace('px', '');
}

function log(msg) {
	window.console.log(msg);
}

