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

	/* TODO: this is temp */
	initializeAutoGrowingTextArea($('#chat_chatbox_X'));

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
	// create the chat button
	var buttonWrapper = $('<div/>').attr('id', 'chat_activesessionbuttonwrapper_' + chatSessionId).append(
		$('<div/>').attr('id', 'chat_activesessionbutton_' + chatSessionId).addClass('chat_sessionlistbutton').css('color', color).text(name)
	).append(
		$('<div/>').addClass('chat_sessionlistsep')
	);

	$('#chat_activechatscontainer').prepend(buttonWrapper);

	buttonWrapper.hide().slideDown();

	// create the chat session tab
	$('#chat_maincell').append(
		$('<div/>').attr('id', 'chat_maincell_' + chatSessionId).addClass('tab').append(
			$('<div/>').addClass('fixedtable').append(
				$('<div/>').addClass('tablerow').append(
					$('<div/>').addClass('cell').append(
						$('<div/>').addClass('fixedtable').append(
							$('<div/>').addClass('tablerow').append(
								$('<div/>').addClass('iconcell').append(
									$('<div/>').attr('id', 'chat_myicon_' + chatSessionId).addClass('framedicon')
								)
							).append(
								$('<div/>').addClass('leftcardtext').append(
									$('<div/>').attr('id', 'chat_myname_' + chatSessionId).addClass('personname')
								).append(
									$('<div/>').attr('id', 'chat_mytitle_' + chatSessionId).addClass('persontitle')
								)
							)
						)
					)
				).append(
					$('<div/>').addClass('vlinecell')
				).append(
					$('<div/>').attr('id', 'chat_theircardcell_' + chatSessionId).addClass('cell').append(
						$('<div/>').addClass('fixedtable').append(
							$('<div/>').addClass('tablerow').append(
								$('<div/>').addClass('cell')
							).append(
								$('<div/>').attr('id', 'chat_theircardtext_' + chatSessionId).addClass('rightcardtext').append(
									$('<div/>').attr('id', 'chat_theirname_' + chatSessionId).addClass('personname')
								).append(
									$('<div/>').attr('id', 'chat_theirtitle_' + chatSessionId).addClass('persontitle')
								)
							).append(
								$('<div/>').addClass('iconcell').append(
									$('<div/>').attr('id', 'chat_theiricon_' + chatSessionId).addClass('framedicon')
								)
							)
						)
					)
				)
			)
		).append(
			$('<div/>').attr('id', 'chat_chatlog_' + chatSessionId).addClass('chatlog')
		).append(
			$('<div/>').attr('id', 'chat_chatboxwrapper_' + chatSessionId).addClass('chatboxwrapper').append(
				$('<textarea/>').attr('id', 'chat_chatbox_' + chatSessionId).addClass('chatbox')
			)
		)
	);

	initializeAutoGrowingTextArea($('#chat_chatbox_' + chatSessionId));

	setVisibleChatSessionId(chatSessionId);
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

var visibleChatSessionId = null;
var visibleChatSessionIdTarget = null;

// TODO: test setVisibleChatSessionId with concurrent calls
/* null for no visible chat session */
function setVisibleChatSessionId(chatSessionId) {
	var alreadyBusy = (visibleChatSessionIdTarget !== null);
	visibleChatSessionIdTarget = chatSessionId;

	if (!alreadyBusy) {
		followVisibleChatSessionIdTarget();
	}
}

function followVisibleChatSessionIdTarget() {
	if (visibleChatSessionIdTarget !== null) {
		chatSessionIdToChatMaincell(visibleChatSessionId).fadeOut(200, function() {
			var currentVisibleChatSessionIdTarget = visibleChatSessionIdTarget;
			visibleChatSessionId = currentVisibleChatSessionIdTarget;

			var targetCell = chatSessionIdToChatMaincell(currentVisibleChatSessionIdTarget);

			targetCell.fadeTo(0, 0, function() {
				updateChatLogHeight();
				targetCell.fadeTo(300, 1, function() {
					if (currentVisibleChatSessionIdTarget === visibleChatSessionIdTarget) {
						// if the target hasn't changed during the fadeIn, we're done
						visibleChatSessionIdTarget = null;
					} else {
						// otherwise, transition to the new target
						setTimeout(followVisibleChatSessionIdTarget, 0);
					}
				});
			});
		});
	}
}

function chatSessionIdToChatMaincell(chatSessionId) {
	var divId = '#chat_maincell_';
	if (chatSessionId) {
		divId += chatSessionId;
	} else {
		divId += 'none';
	}

	return $(divId);
}

function updateChatLogHeight() {
	if (visibleChatSessionId) {
		// disable scrolling as it causes scrollbar flickering
		$('body').css('overflow-y', 'hidden');

		var chatlogDiv = $('#chat_chatlog_' + visibleChatSessionId);
		var chatboxWrapper = $('#chat_chatboxwrapper_' + visibleChatSessionId);
		var chatMaincellDiv = $('#chat_maincell');
		var newChatLogHeight = $(window).height()
			- chatlogDiv.offset().top // remove the space from the start of maincell to the start of chatlog
			- stripPx(chatlogDiv.css('padding-top')) // top and bottom paddings are not counted in the height
			- stripPx(chatlogDiv.css('padding-bottom'))
			- stripPx(chatlogDiv.css('border-top-width')) // same for border
			- stripPx(chatlogDiv.css('border-bottom-width'))
			- stripPx(chatboxWrapper.css('margin-top')) // remove the height of the spacer above the chatbox
			- chatboxWrapper.outerHeight() // remove the height of the chatbox wrapper
			- stripPx(chatMaincellDiv.css('padding-bottom')); // remove the height of the padding below the chatbox

		if (newChatLogHeight < 200) {
			newChatLogHeight = 200;
			// if the scrollbars are needed, enable them
			$('body').css('overflow-y', 'auto');
		}
		chatlogDiv.css('height', newChatLogHeight);
		$('#chat_chatbox_' + visibleChatSessionId).focus();
	}
}

function onResize() {
	if (currentTab == loginTab) {
		onBasicVCenterResize('login', 600);
	} else if (currentTab == menuTab) {
		onBasicVCenterResize('menu', 600);
	} else if (currentTab == chatTab) {
		onChatTabResize();
	}
}

function onChatTabResize() {
	var chatMaincellDiv = $('#chat_maincell');
	if (visibleChatSessionId) {
		updateChatLogHeight();
		chatMaincellDiv.css('height', 'auto');
	} else {
		// disable scrolling as it causes scrollbar flickering
		$('body').css('overflow-y', 'hidden');
		var newChatMaincellHeight = $(window).height() - chatMaincellDiv.offset().top;
		if (newChatMaincellHeight < 500) {
			newChatMaincellHeight = 500;
			// if the scrollbars are needed, enable them
			$('body').css('overflow-y', 'auto');
		}
		chatMaincellDiv.css('height', newChatMaincellHeight);
	}
}

