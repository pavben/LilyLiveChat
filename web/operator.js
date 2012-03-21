// these will be set onload
var loginTab = null;
var chatTab = null;
var miscMessageTab = null;

// Person object representing the operator
var me = null;

$(window).bind('load', function() {
	loginTab = $('#login_tab');
	chatTab = $('#chat_tab');
	miscMessageTab = $('#miscmessage_tab');

	replaceIconWith('images/globes.png', $('#login_icon'));
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

	// chat tab handlers
	$('#chat_nextinlinebutton').click(function() {
		queueAjaxCommand([Messages.OperatorAcceptNextChatSessionMessage]);
	});

	// chat tab's menu effects
	// TODO: simplify this to be click-based as the hover is annoying
	var menuSlideTarget = null;
	var menuSlideBusy = false;

	$('#chat_menuouterwrapper').click(function() {
		// toggle
		menuSlideTarget = menuSlideTarget == 0 ? 1 : 0;
		followSlideTarget();
	});

	function followSlideTarget() {
		if (!menuSlideBusy) {
			menuSlideBusy = true;

			if (menuSlideTarget === 0) {
				$('#chat_menuwrapper').slideUp(400, function() {
					menuSlideBusy = false;

					if (menuSlideTarget !== 0) {
						followSlideTarget();
					}
				});
			} else if (menuSlideTarget === 1) {
				$('#chat_menuwrapper').slideDown(400, function() {
					menuSlideBusy = false;

					if (menuSlideTarget !== 1) {
						followSlideTarget();
					}
				});
			}
		}
	}
	// end chat tab's menu effects

	$(window).resize(onResize);

	ajaxJsonGetSessionId(
		function() {
			queueAjaxCommand([Messages.UnregisteredSelectSiteMessage, 'virtivia']);
		},
		function() {
			resetSession();

			showCantConnectScreen();
		}
	);
});

var loginTabOkActive = false;

function loginTabOkHandler() {
	if (!loginTabOkActive) {
		loginTabOkActive = true;

		var username = $.trim($('#login_username').val());
		var password = $.trim($('#login_password').val());

		// TODO: replace this check with the appropriate enabling/disabling of the login button
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

		queueAjaxCommand([Messages.OperatorLoginRequestMessage, username, password]);
	}
}

function handleMessage(message) {
	messageTypeId = message.shift();
	log("Msg Type Id: " + messageTypeId);
	log(message);
	switch (messageTypeId) {
		case Messages.UnregisteredSiteSelectedMessage:
			var siteName = message[0];
			var siteActive = message[1]; // we don't care if it's active or not for operators

			// set the proper login box title
			$('#login_operloginlabel').text(siteName + ' Operator Login');

			changeTabTo(loginTab, function () {
				// focus the username box
				$('#login_username').focus();
			});

			// Auto-login
			//$('#login_btn_ok').click();
			break;
		case Messages.UnregisteredSiteInvalidMessage:
			// display the invalid site screen
			showInvalidSiteScreen();

			break;
		case Messages.SomethingWentWrongMessage:
			break;
		case Messages.OperatorLoginSuccessMessage:
			if (currentTab == loginTab) {
				var name = message[0];
				var color = message[1];
				var title = message[2];
				var iconUrl = message[3];
				me = new Person(name, color, title, iconUrl);
				changeTabTo(chatTab);
			}
			log("Login successful");
			$('#chat_menulabel').text(name);
			$('#chat_menulabel').css('color', color);
			break;
		case Messages.OperatorLoginFailedMessage:
			showLoginFailedScreen();
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
		case Messages.OperatorReceiveChatMessage:
			var chatSessionId = message[0];
			var text = message[1];

			var they = getChatSessionData(chatSessionId).they;

			writeMessageToChatLog(they.name, they.color, text, $('#chat_chatlog_' + chatSessionId));
			if (visibleChatSessionId !== chatSessionId) {
				setChatSessionIndicator(chatSessionId, ButtonIndicatorStates.Active);
			}

			break;
		case Messages.OperatorChatEndedMessage:
			var chatSessionId = message[0];

			var chatSessionData = getChatSessionData(chatSessionId);
			if (chatSessionData !== null) {
				// if the chat window still exists, it means the customer (not the operator) ended the chat session
				writeInfoTextToChatLog('The customer has ended the chat session.', $('#chat_chatlog_' + chatSessionId));
				// set the session indicator to ended
				setChatSessionIndicator(chatSessionId, ButtonIndicatorStates.Ended);

				chatSessionData.chatSessionEnded = true;

				// and set the "End Chat" button label to "Close"
				$('#chat_btn_endchat_' + chatSessionId).text('Close');
			}

			decreaseNumActiveChats();
			break;
		default: // Invalid message type
			log("Got invalid message type: " + messageTypeId);
	}
}

function handleSessionEnded() {
	switch (getCurrentTabOrTarget()) {
	case miscMessageTab:
		// if we're already on the misc message tab (some error), do nothing
		break;
	default:
		// in all other cases, just show the connection problems screen
		showDisconnectedScreen();
	}
}

var numActiveChats = 0;

function increaseNumActiveChats() {
	numActiveChats++;
	updateActiveChatsLabel();
}

function decreaseNumActiveChats() {
	numActiveChats--;
	updateActiveChatsLabel();
}

function updateActiveChatsLabel() {
	var activeChatsHeader = $('#chat_activechatsheader');
	activeChatsHeader.fadeTo(250, 0, function() {
		if (numActiveChats == 0) {
			activeChatsHeader.text('No active chats');
		} else {
			activeChatsHeader.text('My chats (' + numActiveChats + ' active)');
		}
		activeChatsHeader.fadeTo(500, 1);
	});
}

function addActiveChatSession(chatSessionId, name, color, iconUrl) {
	// create the chat button
	$('#chat_activechatscontainer').prepend(
		$('<div/>').attr('id', 'chat_sessionlistbuttonwrapper_' + chatSessionId).append(
			$('<div/>').attr('id', 'chat_sessionlistbutton_' + chatSessionId).addClass('chat_sessionlistbutton_open').click(function() {
				setVisibleChatSessionId(chatSessionId);
			}).append(
				$('<div/>').addClass('fixedtable').append(
					$('<div/>').addClass('tablerow').append(
						$('<div/>').addClass('chat_sessionlistbutton_ind_base').addClass('cell').append(
							$('<div/>').attr('id', 'chat_sessionlistbutton_ind_' + chatSessionId).fadeTo(0, 0)
						)
					).append(
						$('<div/>').addClass('chat_sessionlistbutton_open_text').addClass('cell').css('color', color).text(name)
					)
				)
			)
		).append(
			$('<div/>').addClass('chat_sessionlistsep')
		).hide().slideDown()
	);

	// create the chat session tab
	$('#chat_maincell').append(
		$('<div/>').attr('id', 'chat_maincell_' + chatSessionId).addClass('tab').append(
			$('<div/>').addClass('fixedtable').append(
				$('<div/>').addClass('tablerow').append(
					$('<div/>').attr('id', 'chat_mycardcell_' + chatSessionId).addClass('cell').append(
						$('<div/>').addClass('fixedtable').append(
							$('<div/>').addClass('tablerow').append(
								$('<div/>').addClass('iconcell').append(
									$('<div/>').attr('id', 'chat_myicon_' + chatSessionId).addClass('framedicon')
								)
							).append(
								$('<div/>').addClass('cardtextwrappercell').append(
									$('<div/>').addClass('leftcardtext').append(
										$('<div/>').attr('id', 'chat_myname_' + chatSessionId).addClass('personname')
									).append(
										$('<div/>').attr('id', 'chat_mytitle_' + chatSessionId).addClass('persontitle')
									)
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
								$('<div/>').addClass('cardtextwrappercell').append(
									$('<div/>').addClass('rightcardtext').append(
										$('<div/>').attr('id', 'chat_theirname_' + chatSessionId).addClass('personname')
									).append(
										$('<div/>').attr('id', 'chat_theirtitle_' + chatSessionId).addClass('persontitle')
									).append(
										$('<div/>').addClass('personmenuwrapper').append(
											$('<div/>').addClass('fixedtable').append(
												$('<div/>').addClass('cell')
											).append(
												$('<div/>').addClass('cell').css('width', '84px').append(
													$('<div/>').attr('id', 'chat_btn_endchat_' + chatSessionId).addClass('personmenubutton personmenubuttonenabled').text('End Chat')
												)
											)
										)
									)
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
			$('<div/>').attr('id', 'chat_chatlog_' + chatSessionId).addClass('chatlogfixed')
		).append(
			$('<div/>').attr('id', 'chat_chatboxwrapper_' + chatSessionId).addClass('chatboxwrapper').append(
				$('<textarea/>').attr('id', 'chat_chatbox_' + chatSessionId).addClass('chatbox')
			)
		)
	);

	replaceIconWith(me.iconUrl, $('#chat_myicon_' + chatSessionId));
	replaceCardTextWith(me, $('#chat_mycardcell_' + chatSessionId), $('#chat_myname_' + chatSessionId), $('#chat_mytitle_' + chatSessionId));

	var they = (getChatSessionData(chatSessionId).they = new Person(name, color, 'Customer', iconUrl));

	replaceIconWith(they.iconUrl, $('#chat_theiricon_' + chatSessionId));
	replaceCardTextWith(they, $('#chat_theircardcell_' + chatSessionId), $('#chat_theirname_' + chatSessionId), $('#chat_theirtitle_' + chatSessionId));

	// mark the chat session as open; this is set to false when the session is beginning its fade-out
	getChatSessionData(chatSessionId).isOpen = true;

	// start in the normal state, which means no active or ended class in the indicator
	getChatSessionData(chatSessionId).buttonIndicatorState = ButtonIndicatorStates.Normal;

	initializeAutoGrowingTextArea($('#chat_chatbox_' + chatSessionId), $('#chat_chatboxwrapper_' + chatSessionId));

	// close handler
	$('#chat_btn_endchat_' + chatSessionId).click(function() {
		// setVisibleChatSessionId to another chat session, if any
		var openChatSessionIds = getOpenChatSessionIds().filter(function(x) { return (x !== chatSessionId); });
		var targetSessionId = (openChatSessionIds.length > 0) ? openChatSessionIds[0] : null;

		var currentChatSessionData = getChatSessionData(chatSessionId);
		
		// set isOpen to false to indicate that this session window is closed
		currentChatSessionData.isOpen = false;

		setVisibleChatSessionId(targetSessionId);

		// disable the session button click handler
		$('chat_sessionlistbutton_' + chatSessionId).off('click');
		var buttonWrapper = $('#chat_sessionlistbuttonwrapper_' + chatSessionId);
		buttonWrapper.slideUp(300, function() {
			// when the slide is finished, remove the button wrapper and everything inside
			buttonWrapper.remove();

			// if this chat session hasn't already been ended by the customer, tell the server that we're ending it
			if (!currentChatSessionData.chatSessionEnded) {
				queueAjaxCommand([Messages.OperatorEndingChatMessage, chatSessionId]);
			}

			// by now, setVisibleChatSessionId should have finished, so also remove chat_maincell_X
			$('#chat_maincell_' + chatSessionId).remove();
		});
	});

	// send handler
	var chatBox = $('#chat_chatbox_' + chatSessionId);
	chatBox.keypress(function(e) {
		if (e.which == 13 && !e.shiftKey && !e.altKey && !e.ctrlKey) { // enter
			if (!getChatSessionData(chatSessionId).chatSessionEnded) {
				if ($.trim(chatBox.val()).length > 0) {
					queueAjaxCommand([Messages.OperatorSendChatMessage, chatSessionId, chatBox.val()]);
					writeMessageToChatLog(me.name, me.color, chatBox.val(), $('#chat_chatlog_' + chatSessionId));
				}
			} else {
				writeInfoTextToChatLog('This chat session is no longer active.', $('#chat_chatlog_' + chatSessionId));
			}
			chatBox.val('');
			return false;
		}
	});

	setVisibleChatSessionId(chatSessionId);

	// increase the count and update the header
	increaseNumActiveChats();
}

/* Session-specific storage functions */
function getChatSessionData(chatSessionId) {
	var mainCell = $('#chat_maincell_' + chatSessionId);
	
	if (mainCell.length !== 0) {
		if (mainCell[0].lilyData === undefined) {
			mainCell[0].lilyData = {}; // create a new object
		}
		return mainCell[0].lilyData;
	} else {
		return null; // invalid chat session
	}
}

function isChatSessionOpen(chatSessionId) {
	var chatSessionData = getChatSessionData(chatSessionId);

	if (chatSessionData != null) {
		return (chatSessionData.isOpen === true);
	} else {
		return false;
	}
}

function getOpenChatSessionIds() {
	var objects = $('[id^=chat_maincell_]');

	var chatSessionIds = [];

	for (var i = 0; i < objects.length; i++) {
		var objectId = objects[i].id;
		if (objectId !== 'chat_maincell_none') {
			chatSessionIds.push(objectId.replace(/^chat_maincell_/, ''));
		}
	}

	return chatSessionIds;
}

var SessionListButtonClasses = {
	Active : 'chat_sessionlistbutton_ind_active',
	Ended : 'chat_sessionlistbutton_ind_ended'
}

var ButtonIndicatorStates = {
	Normal : 0,
	Active : 1,
	Ended : 2
}

function setChatSessionIndicator(chatSessionId, targetValue) {
	if (isChatSessionOpen(chatSessionId)) {
		var sessionButtonIndicator = $('#chat_sessionlistbutton_ind_' + chatSessionId);

		var sessionData = getChatSessionData(chatSessionId);

		/* Current  | Next    | Action
		 * ---------------------------
		 * Normal   | Active  | Show Active
		 * Normal   | Ended   | Show Ended
		 * Active   | Normal  | Fade Active
		 * Active   | Ended   | Fade Active, Show Ended
		 *
		 * and ignore all others
		 */

		var fadeInTime = 500;
		var fadeOutTime = 500;

		if (sessionData.buttonIndicatorState === ButtonIndicatorStates.Normal) {
			switch (targetValue) {
			case ButtonIndicatorStates.Active:
				sessionButtonIndicator.addClass(SessionListButtonClasses.Active);
				// fade in
				sessionButtonIndicator.fadeTo(fadeInTime, 1);
				sessionData.buttonIndicatorState = targetValue;
				break;
			case ButtonIndicatorStates.Ended:
				sessionButtonIndicator.addClass(SessionListButtonClasses.Ended);
				// fade in
				sessionButtonIndicator.fadeTo(fadeInTime, 1);
				sessionData.buttonIndicatorState = targetValue;
				break;
			}
		} else if (sessionData.buttonIndicatorState === ButtonIndicatorStates.Active) {
			switch (targetValue) {
			case ButtonIndicatorStates.Normal:
				sessionButtonIndicator.fadeTo(fadeOutTime, 0, function() {
					sessionButtonIndicator.removeClass(SessionListButtonClasses.Active);
				});
				sessionData.buttonIndicatorState = targetValue;
				break;
			case ButtonIndicatorStates.Ended:
				sessionButtonIndicator.fadeTo(fadeOutTime, 0, function() {
					// now that it's faded out, switch the class
					sessionButtonIndicator.removeClass(SessionListButtonClasses.Active);
					sessionButtonIndicator.addClass(SessionListButtonClasses.Ended);
					// fade in
					sessionButtonIndicator.fadeTo(fadeInTime, 1);
				});
				sessionData.buttonIndicatorState = targetValue;
				break;
			}
		}
	}
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
	var nextInLineButtonText = $('#chat_nextinlinebutton_text');
	var nextInLineButtonWrapper = $('#chat_nextinlinebuttonwrapper');
	var nextInLineHeader = $('#chat_nextinlineheader');
	var nextInLineHeaderText = $('#chat_nextinlineheadertext');

	var effectsActivated = false;

	if (!currentDisplayedNextInLine) {
		// if the next in line button currently isn't showing

		// first, do the slide
		nextInLineHeader.animate({height: '29px'}, 250, function() {
			// then fade the text
			nextInLineHeaderText.delay(100).fadeTo(250, 0, function() {
				// show the text
				nextInLineHeaderText.text('Next in line (' + lineLength + ' waiting)');
				nextInLineHeaderText.fadeTo(500, 1);

				nextInLineButtonText.text(name);
				nextInLineButtonText.css('color', color);
				nextInLineButtonWrapper.fadeTo(500, 1);

				lineStatusFinished();
			});
		});

		effectsActivated = true;
	} else {
		// the button is already showing, so find out what needs to be updated (if anything) and do it
		if (lineLength != currentDisplayedLineLength) {
			// fade out the text
			nextInLineHeaderText.fadeTo(250, 0, function() {
				nextInLineHeaderText.text('Next in line (' + lineLength + ' waiting)');
				nextInLineHeaderText.fadeTo(500, 1);

				lineStatusFinished();
			});

			effectsActivated = true;
		}
		if (currentDisplayedNextInLine[0] != name || currentDisplayedNextInLine[1] != color) {
			nextInLineButtonWrapper.fadeTo(250, 0, function() {
				nextInLineButtonText.text(name);
				nextInLineButtonText.css('color', color);
				nextInLineButtonWrapper.fadeTo(500, 1);

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

			// first, do the slide
			nextInLineHeader.animate({height: '71px'}, 250, function() {
				// fade out the text
				nextInLineHeaderText.delay(120).fadeTo(250, 0, function() {
					nextInLineHeaderText.text('No customers waiting');
					nextInLineHeaderText.fadeTo(500, 1);

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
var visibleChatSessionIdTarget = undefined;

/* null for no visible chat session */
function setVisibleChatSessionId(chatSessionId) {
	var alreadyBusy = (visibleChatSessionIdTarget !== undefined);
	visibleChatSessionIdTarget = chatSessionId;

	if (!alreadyBusy) {
		followVisibleChatSessionIdTarget();
	}
}

// TODO: test setVisibleChatSessionId with concurrent calls (DONE, remove the code)
function testSetVisibleChatSessionId(sessions) {
	setVisibleChatSessionId(sessions[0]);
	setTimeout(function() {
		setVisibleChatSessionId(sessions[1]);
	}, 50);
	setTimeout(function() {
		setVisibleChatSessionId(sessions[2]);
	}, 100);
	setTimeout(function() {
		setVisibleChatSessionId(sessions[0]);
	}, 150);
}

function followVisibleChatSessionIdTarget() {
	if (visibleChatSessionIdTarget !== undefined) {
		chatSessionIdToObject('#chat_maincell_', visibleChatSessionId).fadeOut(200, function() { // NOTE: When increasing the fade time here, make sure buttonWrapper.slideUp(x..) is higher
			var currentVisibleChatSessionIdTarget = visibleChatSessionIdTarget;
			visibleChatSessionId = currentVisibleChatSessionIdTarget;

			var targetCell = chatSessionIdToObject('#chat_maincell_', currentVisibleChatSessionIdTarget);

			targetCell.fadeTo(0, 0, function() {
				onChatTabResize();
				if (currentVisibleChatSessionIdTarget !== null) {
					var chatLogDiv = chatSessionIdToObject('#chat_chatlog_', currentVisibleChatSessionIdTarget);
					// scroll to the bottom, if possible
					chatLogDiv.scrollTop(getScrollTopTarget(chatLogDiv));
				}
				targetCell.fadeTo(300, 1, function() {
					if (currentVisibleChatSessionIdTarget === visibleChatSessionIdTarget) {
						// if the target hasn't changed during the fade-in, we're done
						visibleChatSessionIdTarget = undefined;

						// always set chat session activity to false when opening
						setChatSessionIndicator(currentVisibleChatSessionIdTarget, ButtonIndicatorStates.Normal);

						// and scroll to the bottom again, in case something messed up our last scroll
						if (currentVisibleChatSessionIdTarget !== null) {
							chatLogWritten(chatLogDiv);
						}
					} else {
						// otherwise, transition to the new target
						setTimeout(followVisibleChatSessionIdTarget, 0);
					}
				});
			});
		});
	}
}

function chatSessionIdToObject(prefix, chatSessionId) {
	var divId = prefix;
	if (chatSessionId) {
		divId += chatSessionId;
	} else {
		divId += 'none';
	}

	return $(divId);
}

function showLoginFailedScreen() {
	showMiscMessageTab('No match...',
		$('<div/>').addClass('miscmessage_content_textwrapper').append(
			$('<div/>').text('Can\'t remember your password? Contact your administrator.')
		),
		$('<div/>').addClass('fixedtable').addClass('miscmessage_buttontable').append(
			$('<div/>').addClass('tablerow').append(
				$('<div/>').addClass('cell')
			).append(
				$('<div/>').addClass('cell').css('width', '100px').append(
					$('<div/>').addClass('basicbutton').text('Try again').click(function() {
						window.location.reload();
					})
				)
			).append(
				$('<div/>').addClass('cell').css('width', '7px')
			).append(
				$('<div/>').addClass('cell').css('width', '80px').append(
					$('<div/>').addClass('basicbutton').text('Close').click(function() {
						window.close();
					})
				)
			)
		)
	);
}

function showInvalidSiteScreen() {
	showMiscMessageTab('Invalid Site',
		$('<div/>').addClass('miscmessage_content_textwrapper').append(
			$('<div/>').text('The site you\'ve specified isn\'t registered with LilyLiveChat.')
		),
		$('<div/>').addClass('fixedtable').addClass('miscmessage_buttontable').append(
			$('<div/>').addClass('tablerow').append(
				$('<div/>').addClass('cell')
			).append(
				$('<div/>').addClass('cell').css('width', '80px').append(
					$('<div/>').addClass('basicbutton').text('Close').click(function() {
						window.close();
					})
				)
			)
		)
	);
}

function showDisconnectedScreen() {
	showMiscMessageTab('Connection lost...',
		$('<div/>').addClass('miscmessage_content_textwrapper').append(
			$('<div/>').text('Your connection to LilyLiveChat was lost.')
		),
		$('<div/>').addClass('fixedtable').addClass('miscmessage_buttontable').append(
			$('<div/>').addClass('tablerow').append(
				$('<div/>').addClass('cell')
			).append(
				$('<div/>').addClass('cell').css('width', '100px').append(
					$('<div/>').addClass('basicbutton').text('Reconnect').click(function() {
						window.location.reload();
					})
				)
			).append(
				$('<div/>').addClass('cell').css('width', '7px')
			).append(
				$('<div/>').addClass('cell').css('width', '80px').append(
					$('<div/>').addClass('basicbutton').text('Close').click(function() {
						window.close();
					})
				)
			)
		)
	);
}

function showCantConnectScreen() {
	showMiscMessageTab('Can\'t connect...',
		$('<div/>').addClass('miscmessage_content_textwrapper').append(
			$('<div/>').text('You can\'t seem to connect to LilyLiveChat. If the problem persists, check your Internet connection or try again later.')
		),
		$('<div/>').addClass('fixedtable').addClass('miscmessage_buttontable').append(
			$('<div/>').addClass('tablerow').append(
				$('<div/>').addClass('cell')
			).append(
				$('<div/>').addClass('cell').css('width', '100px').append(
					$('<div/>').addClass('basicbutton').text('Reconnect').click(function() {
						window.location.reload();
					})
				)
			).append(
				$('<div/>').addClass('cell').css('width', '7px')
			).append(
				$('<div/>').addClass('cell').css('width', '80px').append(
					$('<div/>').addClass('basicbutton').text('Close').click(function() {
						window.close();
					})
				)
			)
		)
	);
}

function onResize() {
	if (currentTab == loginTab) {
		onBasicVCenterResize('login', 600);
	} else if (currentTab == chatTab) {
		onChatTabResize();
	} else if (currentTab == miscMessageTab) {
		onBasicVCenterResize('miscmessage', 530);
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

// only called by onChatTabResize
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

