// these will be set onload
var welcomeTab = null;
var chatTab = null;
var miscMessageTab = null;

var myName = null;
var myColor = null;
var myIcon = null;

var chatSessionEnded = false;

function randomizeNameAndIcon() {
	var descriptives = [
		"Mystical",
		"Scholarly",
		"Dramatic",
		"Objective",
		"Esoteric",
		"Subjective",
		"Symbolic",
		"Humanistic",
		"Pragmatic",
		"Utilitarian",
		"Humorous",
		"Profound",
		"Radical",
		"Exquisite",
		"Pretty",
		"Adept",
		"Fair",
		"Ravishing",
		"Fascinating",
		"Robust",
		"Agile",
		"Graceful",
		"Skillful",
		"Handsome",
		"Spirited",
		"Hardy",
		"Charming",
		"Immaculate",
		"Lively",
		"Strong",
		"Delicate",
		"Muscular",
		"Dexterous",
		"Vivacious",
		"Elegant",
		"Nimble",
		"Winsome",
		"Astute",
		"Observant",
		"Capable",
		"Precocious",
		"Clever",
		"Prudent",
		"Competent",
		"Rational",
		"Crafty",
		"Reasonable",
		"Cunning",
		"Educated",
		"Sensible",
		"Gifted",
		"Shrewd",
		"Ingenious",
		"Subtle",
		"Intellectual",
		"Intelligent",
		"Inventive",
		"Wise",
		"Affable",
		"Amiable",
		"Amicable",
		"Cheerful",
		"Cordial",
		"Courteous",
		"Elegant",
		"Gracious",
		"Jolly",
		"Jovial",
		"Sociable",
		"Suave",
		"Tactful",
		"Benevolent"
	];

	var iconAndSuffix = iconsAndSuffixes[Math.floor(Math.random() * iconsAndSuffixes.length)];
	var descriptive = descriptives[Math.floor(Math.random() * descriptives.length)];

	return [descriptive + ' ' + iconAndSuffix[1], iconAndSuffix[0]];
}

function handleMessage(message) {
	messageTypeId = message.shift();
	log("Msg Type Id: " + messageTypeId);
	log(message);
	switch (messageTypeId) {
		case Messages.UnregisteredSiteSelectedMessage:
			var siteName = message[0];
			var isActive = message[1];

			if (isActive === true) {
				changeTabTo(welcomeTab);
				$('#welcome_btn_ok').click();
				setTimeout(function() {
					$('#welcome_btn_ok').click();
				}, 300);
			} else {
				showInactiveSiteScreen();
			}
			break;
		case Messages.UnregisteredSiteInvalidMessage:
			// display the invalid site screen
			showInvalidSiteScreen();

			break;
		case Messages.CustomerInLinePositionMessage:
			if (getCurrentTabOrTarget() == welcomeTab) {
				replaceMeWith(new Person(myName, myColor, 'Customer', myIcon));
				changeTabTo(chatTab);

				writeWelcomeTextToChatlog();
				writeSoundsStatusToChatlog();
			}
			updatePositionInLine(parseInt(message[0]));

			break;
		case Messages.CustomerNowTalkingToMessage:
			var name = message[0];
			var color = message[1];
			var title = message[2];
			var iconUrl = message[3];
			replaceThemWith(new Person(name, color, title, iconUrl));
			writeNowTalkingToTextToChatlog(name, color);

			playSoundAfterDing('nowspeakingwithrep');
			break;
		case Messages.CustomerReceiveChatMessage:
			var text = message[0];
			writeMessageToChatlog(they.name, they.color, text, $('#chat_chatlog'));
			break;
		case Messages.SomethingWentWrongMessage:
			break;
		case Messages.CustomerChatEndedMessage:
			if (chatSessionEnded) {
				writeInfoTextToChatlog('The chat session has ended.', $('#chat_chatlog'));
			} else {
				writeInfoTextToChatlog('The operator has ended the chat session.', $('#chat_chatlog'));
				// also disable the End Chat button, since it no longer has any effect
				disableEndChatButton();
			}

			chatSessionEnded = true;

			break;
		case Messages.CustomerNoOperatorsAvailableMessage:
			showNoOperatorsAvailableScreen();
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
	case chatTab:
		// if we're on the chat tab, we ignore the connection close if the chat session already ended
		if (!chatSessionEnded) {
			// otherwise, we show the connection problems screen
			showDisconnectedScreen();
		}
		break;
	default:
		// in all other cases, just show the connection problems screen
		showDisconnectedScreen();
	}
}

var me = null;
var they = null;

function replaceMeWith(person) {
	me = person;
	replaceIconWith(person.iconUrl, $('#chat_myicon'));
	replaceCardTextWith(person, $('#chat_mycardcell'), $('#chat_myname'), $('#chat_mytitle'));
}

function replaceThemWith(person) {
	they = person;
	changeRightSpaceDivTo($('#chat_theircardcell'), function() {
		replaceIconWith(person.iconUrl, $('#chat_theiricon'));
		replaceCardTextWith(person, null, $('#chat_theirname'), $('#chat_theirtitle'));
	});
}

var currentRightSpaceDiv = null;

function changeRightSpaceDivTo(rightSpaceDiv, contentReplaceFunction) {
	if (currentRightSpaceDiv) {
		currentRightSpaceDiv.fadeTo(300, 0, function() {
			currentRightSpaceDiv.hide();

			onOldRightSpaceDivGone();
		});
	} else {
		onOldRightSpaceDivGone();
	}

	function onOldRightSpaceDivGone() {
		currentRightSpaceDiv = rightSpaceDiv;

		if (contentReplaceFunction) {
			contentReplaceFunction();
		}

		currentRightSpaceDiv.fadeTo(600, 1);
	}
}

function getOrdinalSuffixFor(number) {
	if ((number % 100) >= 11 && (number % 100) <= 19) {
		return 'th';
	}

	switch (number % 10) {
		case 1:
			return 'st';
		case 2:
			return 'nd';
		case 3:
			return 'rd';
		case 4:
		case 5:
		case 6:
		case 7:
		case 8:
		case 9:
		case 0:
			return 'th';
	}
}

var firstPositionUpdateProcessed = false;

function updatePositionInLine(position) {
	changeRightSpaceDivTo($('#chat_inlinecell'), function() {
		$('#chat_inlinepos').text(position).append(
			$('<sup/>').text(getOrdinalSuffixFor(position))
		);
	});

	if (firstPositionUpdateProcessed) {
		if (position == 1) {
			playSoundAfterDing('getreadyyourenextinline');
		}
	} else {
		firstPositionUpdateProcessed = true;
	}
}

function onResize() {
	if (currentTab == welcomeTab) {
		onBasicVCenterResize('welcome', 641);
	} else if (currentTab == chatTab) {
		onChatTabResize();
	} else if (currentTab == miscMessageTab) {
		onBasicVCenterResize('miscmessage', 530);
	}
}

function onChatTabResize() {
	// disable scrolling as it interferes with calculations and causes visual glitches
	$('body').css('overflow-y', 'hidden');
	var chatlogDiv = $('#chat_chatlog');
	var chatboxWrapper = $('#chat_chatboxwrapper');
	var newChatlogHeight = $(window).height() // start with the full height
		- chatlogDiv.offset().top // remove all up to the start of chatlog
		- stripPx(chatlogDiv.css('padding-top')) // top and bottom paddings are not counted in the height
		- stripPx(chatlogDiv.css('padding-bottom'))
		- stripPx(chatlogDiv.css('border-top-width')) // same for border
		- stripPx(chatlogDiv.css('border-bottom-width'))
		- stripPx(chatboxWrapper.css('margin-top')) // remove the height of the spacer above the chatbox
		- chatboxWrapper.outerHeight() // remove the height of the chatbox wrapper
		- stripPx($('#chat_tab').css('padding-bottom')); // remove the height of the spacer below the chatbox

	if (newChatlogHeight < 200) {
		newChatlogHeight = 200;
		// if the scrollbars are needed, enable them
		$('body').css('overflow-y', 'auto');
	}

	chatlogDiv.css('height', newChatlogHeight);

	// scroll the chatlog to the bottom, if possible
	instantScrollChatlogToBottom(chatlogDiv);

	$('#chat_chatbox').focus();
}

var welcomeTabOkActive = false;

function welcomeTabOkHandler() {
	if (!welcomeTabOkActive) {
		welcomeTabOkActive = true;

		// myName is global
		myName = $.trim($('#welcome_myname').val());
		// if no valid name was entered, randomize instead of logging in -- next click will login
		// this allows the customer to see their profile before it's used
		if (myName.length == 0) {
			$('#welcome_btn_randomize').click();
			welcomeTabOkActive = false;
			return;
		}
		var referrer = $.cookie(siteId + '.referrer');
		if (referrer === null) {
			referrer = '';
		}
		
		queueAjaxCommand([Messages.CustomerJoinMessage, myName, myColor, myIcon, referrer]);

		// on success, don't re-enable the OK button to avoid a double-send if the user double-clicks
	}
}

function showInactiveSiteScreen() {
	showMiscMessageTab('No representatives available...',
		$('<div/>').addClass('miscmessage_content_textwrapper').append(
			$('<div/>').text('Our representatives are currently unavailable. Please try again later.')
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

function showInvalidSiteScreen() {
	showMiscMessageTab('Invalid Site',
		$('<div/>').addClass('miscmessage_content_textwrapper').append(
			$('<div/>').text('The website you\'re on isn\'t registered with LilyLiveChat. The webmaster is probably not aware of this, so you\'d be doing them a favor by letting them know :-)')
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

function showNoOperatorsAvailableScreen() {
	showMiscMessageTab('Bad news...',
		$('<div/>').addClass('miscmessage_content_textwrapper').append(
			$('<div/>').text('The representative you were waiting for has just become unavailable. Unfortunately, there is nobody else online for us to connect you with. Please try again another time.')
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
						refreshThroughSiteLocator();
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
			$('<div/>').text('Someone is experiencing connection issues! It could be you or us.')
		).append(
			$('<div/>').text('You can try to reconnect. If the problem persists, check your Internet connection or try again later.')
		),
		$('<div/>').addClass('fixedtable').addClass('miscmessage_buttontable').append(
			$('<div/>').addClass('tablerow').append(
				$('<div/>').addClass('cell')
			).append(
				$('<div/>').addClass('cell').css('width', '100px').append(
					$('<div/>').addClass('basicbutton').text('Try again').click(function() {
						refreshThroughSiteLocator();
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

function disableEndChatButton() {
	$('#chat_btn_endchat').off('click').removeClass('personmenubuttonenabled');
	$('#chat_btn_endchat_wrapper').fadeTo(300, 0.5);
}

// sounds

var jPlayerDing;
var jPlayerNext;

var soundsEnabled = true;

function initializeJplayers() {
	jPlayerNext = initializeJplayerNext();
	jPlayerDing = initializeJplayerDing(jPlayerNext);
}

function initializeJplayerDing(jPlayerNext) {
	var jPlayerDiv = $('<div/>');
	$('body').append(jPlayerDiv);
	
	jPlayerDiv.jPlayer({
		ready: function() {
			jPlayerDiv.jPlayer('setMedia', {
				mp3: '/audio/hding-lding.mp3',
				oga: '/audio/hding-lding.ogg'
			}).jPlayer('load');
		},
		ended: function() {
			jPlayerNext.jPlayer('play');
		},
		error: function(e) {
			log('Disabling sounds due to an error in the \'ding\' player');
			soundsEnabled = false;
		},
		swfPath: '/audio',
		solution: 'flash, html',
		supplied: 'oga, mp3',
		errorAlerts: true // TODO: remove
	});

	return jPlayerDiv;
}

function initializeJplayerNext() {
	var jPlayerDiv = $('<div/>');
	$('body').append(jPlayerDiv);
	
	jPlayerDiv.jPlayer({
		error: function(e) {
			log('Disabling sounds due to an error in the \'next\' player');
			soundsEnabled = false;
		},
		swfPath: '/audio',
		solution: 'flash, html',
		supplied: 'oga, mp3',
		errorAlerts: true // TODO: remove
	});

	return jPlayerDiv;
}

function playSoundAfterDing(soundName) {
	if (soundsEnabled) {
		// first, stop the ding player if it's playing
		jPlayerDing.jPlayer('stop');

		// then set the next player to the desired sound (stop is implicit)
		jPlayerNext.jPlayer('setMedia', {
			mp3: '/audio/' + soundName + '.mp3',
			oga: '/audio/' + soundName + '.ogg'
		}).jPlayer('load');

		// begin playing the ding while the next sound is loading
		jPlayerDing.jPlayer('play');

		// the ding player will automatically play the next sound on 'ended'
	}
}

function writeSoundsStatusToChatlog() {
	var chatlogDiv = $('#chat_chatlog');

	if (soundsEnabled) {
		chatlogDiv.append(
			$('<div/>').addClass('chatinfotext').append(
				$('<span/>').text('You\'ll hear a sound when a representative is available. ')
			).append(
				$('<a/>').attr('href', '#').text('Don\'t want sounds?').click(function() {
					if (soundsEnabled) {
						soundsEnabled = false;
						chatlogDiv.append(
							$('<div/>').addClass('chatinfotext').append(
								$('<span/>').text('Okay, we won\'t play any sounds. Just make sure you don\'t miss your turn!')
							)
						);
					} else {
						chatlogDiv.append(
							$('<div/>').addClass('chatinfotext').append(
								$('<span/>').text('Sounds are already off.')
							)
						);
					}
					return false; // avoid following the blank link
				})
			)
		);
	} else {
		// if the sounds are off (due to an error), don't show any extra messages
	}

	chatlogWritten(chatlogDiv);
}

function writeWelcomeTextToChatlog() {
	var chatlogDiv = $('#chat_chatlog');

	chatlogDiv.append(
		$('<div/>').addClass('chatinfotext').append(
			$('<span/>').text('Someone will be with you soon. You can save time by asking your question while you wait.')
		)
	);

	chatlogWritten(chatlogDiv);
}

function writeNowTalkingToTextToChatlog(name, color) {
	var chatlogDiv = $('#chat_chatlog');

	chatlogDiv.append(
		$('<div/>').addClass('chatinfotext').append(
			$('<span/>').text('You are now speaking with ')
		).append(
			$('<span/>').css('color', color).text(name)
		).append(
			$('<span/>').text('.')
		)
	);

	chatlogWritten(chatlogDiv);
}

$(window).bind('load', function() {
	welcomeTab = $('#welcome_tab');
	chatTab = $('#chat_tab');
	miscMessageTab = $('#miscmessage_tab');

	// initially, these are invisible
	$('#welcome_icon').fadeTo(0, 0);
	$('#chat_mycardcell').fadeTo(0, 0);
	$('#chat_myicon').fadeTo(0, 0);
	$('#chat_theiricon').fadeTo(0, 0);
	// and these are hidden
	$('#chat_theircardcell').hide();
	$('#chat_inlinecell').hide();

	// generate an initial color and set it
	myColor = generatePersonColor();
	$('#welcome_myname').css('color', myColor);

	// also pick a random icon to start with
	myIcon = randomizeNameAndIcon()[1];
	replaceIconWith(myIcon, $('#welcome_icon'));

	// set the waiting clock icon
	replaceIconWith('/images/waiting_clock.png', $('#chat_waiticon'));

	// welcome tab handlers
	var nameEdited = false;
	onChangeToFieldValue($('#welcome_myname'), function() {
		nameEdited = true;
	});
	$('#welcome_btn_randomize').click(function(e) {
		$('#welcome_myname').fadeTo(100, 0, function() {
			var nameAndIcon = randomizeNameAndIcon();
			// name
			// only set the random name if the user hasn't edited the name field
			if (nameEdited === false || $.trim($('#welcome_myname').val()) === '') {
				setFieldValue($('#welcome_myname'), nameAndIcon[0]);
				nameEdited = false;
			}
			// color
			myColor = generatePersonColor();
			$('#welcome_myname').css('color', myColor);
			// icon
			myIcon = nameAndIcon[1];
			replaceIconWith(myIcon, $('#welcome_icon'));
			// and fade the name back to 1
			$('#welcome_myname').fadeTo(100, 1);
		});
	});

	// clicking the OK button
	$('#welcome_btn_ok').click(welcomeTabOkHandler);
	// or pressing Enter inside the name box
	$('#welcome_myname').keypress(function(e) {
		if (e.which == 13) { // enter
			welcomeTabOkHandler();
		}
	});

	// chat tab handlers
	var chatBox = $('#chat_chatbox');
	chatBox.keypress(function(e) {
		if (e.which == 13 && !e.shiftKey && !e.altKey && !e.ctrlKey) { // enter
			if (!chatSessionEnded) {
				if ($.trim(chatBox.val()).length > 0) {
					queueAjaxCommand([Messages.CustomerSendChatMessage, chatBox.val()]);
					writeMessageToChatlog(me.name, me.color, chatBox.val(), $('#chat_chatlog'));
				}
			} else {
				writeInfoTextToChatlog('This chat session is no longer active.', $('#chat_chatlog'));
			}
			chatBox.val('');
			return false;
		}
	});

	$('#chat_btn_endchat').click(function() {
		chatSessionEnded = true;

		disableEndChatButton();

		queueAjaxCommand([Messages.CustomerEndingChatMessage]);
	});
	
	// initialize the auto-growing chatbox and append the shadow div to the chatboxwrapper
	initializeAutoGrowingTextArea($('#chat_chatbox'), $('#chat_chatboxwrapper'));

	// we start on welcometab
	//changeTabTo(welcomeTab);
	// DEBUG
	//changeTabTo(chatTab);
	//updatePositionInLine(5);
	// END OF DEBUG
	
	initializeJplayers();
	
	$(window).resize(onResize);

	// check if the siteId was successfully parsed from the URL
	if (siteId !== null) {
		ajaxJsonGetSessionId(
			function() {
				queueAjaxCommand([Messages.UnregisteredSelectSiteMessage, siteId]);

				// TEMP: remove this
				//$('#welcome_btn_ok').click();
				//setTimeout(function() { $('#welcome_btn_ok').click(); }, 800);
			},
			function() {
				resetSession();

				showCantConnectScreen();
			}
		);
	} else {
		showInvalidSiteScreen();
	}
});

