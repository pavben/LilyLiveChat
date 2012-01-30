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
				// OperatorLoginRequestMessage, site id, name, ...
				queueAjaxCommand([8, "virtivia", username, password]);

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
	switch (messageTypeId) {
		case 7: // SomethingWentWrongMessage
			break;
		case 9: // OperatorLoginSuccessMessage
			if (currentTab == loginTab) {
				var name = message[0];
				var color = message[1];
				var title = message[2];
				var iconUrl = message[3];
				me = new Person(name, color, title, iconUrl);
				$('#menu_welcomelabel').html('Hey, ' + name + '!');
				//changeTabTo(menuTab); // TODO: for now, we jump directly to the chat tab
				changeTabTo(chatTab);
			}
			log("Login successful");
			break;
		case 10: // OperatorLoginFailedMessage
			alert("Login failed: Invalid credentials");
			break;
		case 11: // LineStatusUpdateMessage
			var name = message[0];
			var color = message[1];
			var numInLine = parseInt(message[2]);
			// TODO: Create a message queue system to avoid mixing of effects
			updateNextInLine(name, color, numInLine);
			break;
		case 12: // LineIsEmptyMessage
			emptyNextInLine();
			break;
		default: // Invalid message type
			log("Got invalid message type: " + messageTypeId);
	}
}

var lastNumInLine = 0;
var currentNextInLine = [null, null]; // name and color

function hey() {
	updateNextInLine('Joe', '#cc3332', 1);
	setTimeout(function() {
		updateNextInLine('Joe', '#cc3332', 2);
	}, 1);
	setTimeout(function() {
		updateNextInLine('Joe2', '#cc3332', 1);
	}, 2);
	setTimeout(function() {
		updateNextInLine('Joe2', '#cc3332', 2);
	}, 3);
}

function updateNextInLine(name, color, numInLine) {
	var nextInLineButton = $('#chat_nextinlinebutton');
	var nextInLineButtonWrapper = $('#chat_nextinlinebuttonwrapper');
	var nextInLineHeader = $('#chat_nextinlineheader');
	var nextInLineHeaderText = $('#chat_nextinlineheadertext');

	if (!currentNextInLine[0]) {
		// if the next in line button currently isn't showing

		// fade out the text
		nextInLineHeaderText.fadeTo(100, 0, function() {
			// now that the text is faded out, begin the slide
			nextInLineHeader.animate({height: '29px'}, 250, function() {
				// the slide finished, so show the text
				nextInLineHeaderText.html('Next in line (' + numInLine + ' waiting)');
				nextInLineHeaderText.fadeTo(100, 1);

				nextInLineButton.html(name);
				nextInLineButton.css('color', color);
				nextInLineButtonWrapper.fadeTo(100, 1);
			});
		});
	} else {
		// the button is already showing, so find out what needs to be updated (if anything) and do it
		if (numInLine != lastNumInLine) {
			// fade out the text
			nextInLineHeaderText.fadeTo(100, 0, function() {
				nextInLineHeaderText.html('Next in line (' + numInLine + ' waiting)');
				nextInLineHeaderText.fadeTo(100, 1);
			});
		}
		if (currentNextInLine[0] != name || currentNextInLine[1] != color) {
			nextInLineButtonWrapper.fadeTo(100, 0, function() {
				nextInLineButton.html(name);
				nextInLineButton.css('color', color);
				nextInLineButtonWrapper.fadeTo(100, 1);
			});
		}
	}

	currentNextInLine = [name, color];
	lastNumInLine = numInLine;
}

function emptyNextInLine() {
	if (currentNextInLine[0]) {
		var nextInLineButtonWrapper = $('#chat_nextinlinebuttonwrapper');
		var nextInLineHeader = $('#chat_nextinlineheader');
		var nextInLineHeaderText = $('#chat_nextinlineheadertext');

		nextInLineButtonWrapper.fadeTo(100, 0, function() {
			nextInLineButtonWrapper.hide();

			nextInLineHeaderText.fadeTo(100, 0, function() {
				// now that the button has been hidden and the text is faded out, begin the slide
				nextInLineHeader.animate({height: '71px'}, 250, function() {
					// the slide finished, so show the text
					nextInLineHeaderText.html('No customers waiting');
					nextInLineHeaderText.fadeTo(100, 1);
				});
			});
		});
	}

	currentNextInLine = [null, null];
	lastNumInLine = 0;
}

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

