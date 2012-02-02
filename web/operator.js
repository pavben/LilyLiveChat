var currentTab = null;

// these will be set onload
var loginTab = null;
var menuTab = null;
var chatTab = null;

// Person object representing the operator
var me = null;

$(document).ready(function() {
	loginTab = $('#login_tab');
	menuTab = $('#menu_tab');
	chatTab = $('#chat_tab');

	replaceIconWith('images/lock.png', $('#login_icon'));
	// default these labels
	$('#chat_nextinlineheadertext').text('No customers waiting');
	$('#chat_activechatsheader').text('No active chats');

	// login tab handlers

	// clicking the OK button
	$('#login_btn_ok').click(loginTabOkHandler);
	// or pressing Enter inside the name box
	$('#login_username').keypress(function(e) {
		if (e.which == 13) { // enter
			loginTabOkHandler();
		}
	});
	$('#login_password').keypress(function(e) {
		if (e.which == 13) { // enter
			loginTabOkHandler();
		}
	});

	// menu tab handlers
	$('#menu_btn_chat').click(function() {
		changeTabTo(chatTab);
	});

	// chat tab handlers
	$('#chat_nextinlinebutton').click(function() {
		queueAjaxCommand([Messages.OperatorAcceptNextChatSessionMessage]);
	});

	changeTabTo(loginTab);
	//changeTabTo(menuTab);
	//changeTabTo(chatTab);
	$('#login_btn_ok').click();

	$(window).resize(onResize);
});

var loginTabOkActive = false;

function loginTabOkHandler() {
	if (!loginTabOkActive) {
		loginTabOkActive = true;

		var username = $.trim($('#login_username').val());
		var password = $.trim($('#login_password').val());

		if (username.length == 0 || password.length == 0) {
			// TODO: REMOVE THIS DEV CODE
			username = "mike";
			password = "mike";
			/*
			alert("Please enter both the username and password.");
			loginTabOkActive = false;
			return;
			*/
		}

		ajaxJsonGetSessionId(
			function() {
				// site id, name, ...
				queueAjaxCommand([Messages.OperatorLoginRequestMessage, "virtivia", username, password]);

				// re-enable the OK button
				loginTabOkActive = false;
			},
			function() {
				alert("Failed to acquire Session ID");
				nextOutSequence = 0; // reset this to 0

				// re-enable the OK button
				loginTabOkActive = false;
			}
		);
	}
}

function handleMessage(message) {
	log(message);
	messageTypeId = message.shift();
	log("Msg Type Id: " + messageTypeId);
	switch (messageTypeId) {
		case Messages.SomethingWentWrongMessage:
			break;
		case Messages.OperatorLoginSuccessMessage:
			if (currentTab == loginTab) {
				var name = message[0];
				var color = message[1];
				var title = message[2];
				var iconUrl = message[3];
				me = new Person(name, color, title, iconUrl);
				$('#menu_welcomelabel').text('Hey, ' + name + '!');
				//changeTabTo(menuTab); // TODO: for now, we jump directly to the chat tab
				changeTabTo(chatTab);
			}
			log("Login successful");
			break;
		case Messages.OperatorLoginFailedMessage:
			alert("Login failed: Invalid credentials");
			break;
		case Messages.OperatorLineStatusDetailsMessage:
			var name = message[0];
			var color = message[1];
			var lineLength = parseInt(message[2]);

			setLineStatus(name, color, lineLength);

			break;
		case Messages.OperatorLineStatusEmptyMessage:
			setLineStatus(null, null, 0);
			break;
		case Messages.OperatorNowTalkingToMessage:
			var chatSessionId = message[0];
			var name = message[1];
			var color = message[2];
			var iconUrl = message[3];
			addActiveChatSession(chatSessionId, name, color, iconUrl);
			break;
		default: // Invalid message type
			log("Got invalid message type: " + messageTypeId);
	}
}

function addActiveChatSession(chatSessionId, name, color, iconUrl) {
	var buttonWrapper = $('<div/>').attr('id', 'chat_activesessionbuttonwrapper_' + chatSessionId).append(
		$('<div/>').attr('id', 'chat_activesessionbutton_' + chatSessionId).addClass('chat_sessionlistbutton').css('color', color).text(name)
	).append(
		$('<div/>').addClass('chat_sessionlistsep')
	);

	$('#chat_activechatscontainer').prepend(buttonWrapper);

	buttonWrapper.hide().slideDown();
}

/* Line status updating and effects */
function testLineStatusUpdating() {
	handleMessage([11, 'Bob', '#cc3332', 1]);
	handleMessage([12]);
	handleMessage([11, 'Joe', '#cc3332', 1]);
	handleMessage([11, 'Joe', '#cc3332', 2]);
	handleMessage([11, 'Joe2', '#cc3332', 1]);
	handleMessage([11, 'Joe2', '#cc3332', 2]);
	handleMessage([11, 'Joe2', '#cc3332', 1]);
}

var latestLineStatus = null; // name, color, line length; instantly updated when new data is available
var lineStatusBusy = false; // true if effects are running
var currentDisplayedLineLength = 0; // the value reflecting what is displayed to the user as the line length
var currentDisplayedNextInLine = null; // name and color; reflecting what is displayed to the user

function setLineStatus(name, color, lineLength) {
	latestLineStatus = [name, color, lineLength];
	checkLineStatus();
}

function checkLineStatus() {
	if (!lineStatusBusy && latestLineStatus) {
		lineStatusBusy = true;

		var name = latestLineStatus[0];
		var color = latestLineStatus[1];
		var lineLength = latestLineStatus[2];

		if (lineLength > 0) {
			updateNextInLine(name, color, lineLength);
		} else {
			emptyNextInLine();
		}

		latestLineStatus = null;
	}
}

function lineStatusFinished() {
	lineStatusBusy = false;
	setTimeout(checkLineStatus, 0);
}

function updateNextInLine(name, color, lineLength) {
	var nextInLineButton = $('#chat_nextinlinebutton');
	var nextInLineButtonWrapper = $('#chat_nextinlinebuttonwrapper');
	var nextInLineHeader = $('#chat_nextinlineheader');
	var nextInLineHeaderText = $('#chat_nextinlineheadertext');

	var effectsActivated = false;

	if (!currentDisplayedNextInLine) {
		// if the next in line button currently isn't showing

		// fade out the text
		nextInLineHeaderText.fadeTo(100, 0, function() {
			// now that the text is faded out, begin the slide
			nextInLineHeader.animate({height: '29px'}, 250, function() {
				// the slide finished, so show the text
				nextInLineHeaderText.text('Next in line (' + lineLength + ' waiting)');
				nextInLineHeaderText.fadeTo(100, 1);

				nextInLineButton.text(name);
				nextInLineButton.css('color', color);
				nextInLineButtonWrapper.fadeTo(100, 1);

				lineStatusFinished();
			});
		});

		effectsActivated = true;
	} else {
		// the button is already showing, so find out what needs to be updated (if anything) and do it
		if (lineLength != currentDisplayedLineLength) {
			// fade out the text
			nextInLineHeaderText.fadeTo(100, 0, function() {
				nextInLineHeaderText.text('Next in line (' + lineLength + ' waiting)');
				nextInLineHeaderText.fadeTo(100, 1);

				lineStatusFinished();
			});

			effectsActivated = true;
		}
		if (currentDisplayedNextInLine[0] != name || currentDisplayedNextInLine[1] != color) {
			nextInLineButtonWrapper.fadeTo(100, 0, function() {
				nextInLineButton.text(name);
				nextInLineButton.css('color', color);
				nextInLineButtonWrapper.fadeTo(100, 1);

				lineStatusFinished();
			});

			effectsActivated = true;
		}
	}

	currentDisplayedNextInLine = [name, color];
	currentDisplayedLineLength = lineLength;

	// if no effects to activate, signal that we're done
	if (!effectsActivated) {
		lineStatusFinished();
	}
}

function emptyNextInLine() {
	var effectsActivated = false;

	if (currentDisplayedNextInLine) {
		var nextInLineButtonWrapper = $('#chat_nextinlinebuttonwrapper');
		var nextInLineHeader = $('#chat_nextinlineheader');
		var nextInLineHeaderText = $('#chat_nextinlineheadertext');

		nextInLineButtonWrapper.fadeTo(100, 0, function() {
			nextInLineButtonWrapper.hide();

			nextInLineHeaderText.fadeTo(100, 0, function() {
				// now that the button has been hidden and the text is faded out, begin the slide
				nextInLineHeader.animate({height: '71px'}, 250, function() {
					// the slide finished, so show the text
					nextInLineHeaderText.text('No customers waiting');
					nextInLineHeaderText.fadeTo(100, 1);

					lineStatusFinished();
				});
			});
		});

		effectsActivated = true;
	}

	currentDisplayedNextInLine = null;
	currentDisplayedLineLength = 0;

	// if no effects to activate, signal that we're done
	if (!effectsActivated) {
		lineStatusFinished();
	}
}

/* End of line status updating and effects */

function onResize() {
	if (currentTab == loginTab) {
		onBasicVCenterResize('login', 600);
	} else if (currentTab == menuTab) {
		onBasicVCenterResize('menu', 600);
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

