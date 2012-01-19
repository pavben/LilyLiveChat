var currentTab = null;

// these will be set onload
var welcomeTab = null;
var chatTab = null;

var myName = null;
var myColor = null;
var myIcon = null;

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

function randomizeProfileIcon(justNot) {
	var icons = [
		'images/cc/dog1.png',
		'images/cc/foxy.png',
		'images/cc/tuqui.png'
	];

	var newIcon;
	do {
		newIcon = icons[Math.floor(Math.random() * icons.length)];
	} while (justNot && newIcon == justNot); // loop until unique

	return newIcon;
}

function generateName(justNot)
{
	var descriptive = [
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

	var newName;
	do {
		descriptiveVal = descriptive[Math.floor(Math.random() * descriptive.length)];
		newName = descriptiveVal + ' Bear';
	} while (justNot && newName == justNot); // loop until unique

	return newName;
}

function handleMessage(message) {
	log(message);
	messageTypeId = message.shift();
	switch (messageTypeId) {
		case 2: // InLinePositionMessage
			if (currentTab == welcomeTab) {
				replaceMeWith(new Person(myName, myColor, 'Guest', myIcon));
				changeTabTo(chatTab);
			}
			updatePositionInLine(parseInt(message[0]));
			break;
		case 3: // NowTalkingToMessage
			break;
		case 5: // AppendToChatLogMessage
			break;
		case 6: // EndChatMessage
			break;
		case 7: // SomethingWentWrongMessage
			break;
		default: // Invalid message type
	}
}

function ajaxJsonGetSessionId(myName, onSuccessCallback, onErrorCallback) {
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
		onErrorCallback
	);
}

function writeMessageToChatLog(name, nameColor, msg, chatlogDiv) {
	var content = '';
	content += '<span class="chat_msgtext" style="color:' + nameColor + '">' + name + '</span>';
	content += '<span class="chat_msgtext">: ' + msg + '</span><br/>';

	writeContentToChatLog(content, chatlogDiv);
}

function writeContentToChatLog(content, chatlogDiv) {
	chatlogDiv.append(content);

	if (this.lastScrollTopTarget && chatlogDiv.scrollTop() >= this.lastScrollTopTarget - 30) {
		// if they scroll within 200px of the bottom
		this.scrollLock = false;
	}
	else if (this.lastScrollTop && chatlogDiv.scrollTop() < this.lastScrollTop) {
		// if the user scrolled up the chat log
		this.scrollLock = true;
	}

	var scrollTopTarget = getScrollTopTarget(chatlogDiv);

	if (!this.scrollLock)
	{
		// here we use a custom "scroll" queue to make sure scrolling does not interfere with other animations
		// we do this because we are using .stop() and clearing the queue, and we only want scroll tasks cleared
		chatlogDiv.stop('scroll', true, false)
		.queue('scroll', function(next) {
			$(this).animate({scrollTop: scrollTopTarget}, {duration:500, queue:false});
			next();
		})
		.dequeue('scroll');
	}

	this.lastScrollTop = chatlogDiv.scrollTop();
	this.lastScrollTopTarget = scrollTopTarget;

	function getScrollTopTarget(theDiv) {
		return theDiv[0].scrollHeight // start with the total scroll height
			- theDiv.outerHeight() // subtract (height + padding + border)
			+ parseInt(theDiv.css('border-top-width')) // readd the top border
			+ parseInt(theDiv.css('border-bottom-width')) // readd the bottom border
	}
}

var me = null;
var they = null;

function replaceMeWith(person) {
	me = person;
	replaceIconWith(person.iconUrl, $('#chat_myicon'));
	replaceCardTextWith(person, $('#chat_mycard'), $('#chat_myname'), $('#chat_mytitle'));
}

function replaceThemWith(person) {
	they = person;
	changeRightSpaceDivTo($('#chat_theircardrow'), function() {
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

function updatePositionInLine(position) {
	var newContent = position + '<sup>' + getOrdinalSuffixFor(position) + '</sup>';

	changeRightSpaceDivTo($('#chat_inlinecardrow'), function() {
		$('#chat_inlinepos').html(newContent);
	});
}

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

function onResize() {
	if (currentTab == welcomeTab) {
		onWelcomeTabResize();
	} else if (currentTab == chatTab) {
		onChatTabResize();
	}
}

function onWelcomeTabResize() {
	// disable scrolling as it causes visual glitches
	$('body').css('overflow-y', 'hidden');
	var newWelcomeTabHeight = $(window).height();
	if (newWelcomeTabHeight < 641) {
		newWelcomeTabHeight = 641;
		// if the scrollbars are needed, enable them
		$('body').css('overflow-y', 'auto');
	}
	// calculate how much space needs to be filled above and below the background
	var spaceToFill = newWelcomeTabHeight - $('#welcome_bg').outerHeight();
	var newWelcomeTabBgTopHeight = Math.floor(spaceToFill / 2);
	var newWelcomeTabBgBotHeight = Math.ceil(spaceToFill / 2); // bottom gets the extra pixel
	$('#welcome_bgtop').css('height', newWelcomeTabBgTopHeight);
	$('#welcome_bgbot').css('height', newWelcomeTabBgBotHeight);
	$('#welcome_tab').css('height', newWelcomeTabHeight);
}

function onChatTabResize() {
	// disable scrolling as it interferes with calculations and causes visual glitches
	$('body').css('overflow-y', 'hidden');
	var chatlogDiv = $('#chat_chatlog');
	var newChatLogHeight = $(window).height() // start with the full height
		- chatlogDiv.offset().top // remove all up to the start of chatlog
		- stripPx($('#chat_chatlog').css('padding-top')) // top and bottom paddings are not counted in the height
		- stripPx($('#chat_chatlog').css('padding-bottom'))
		- stripPx($('#chat_chatlog').css('border-top-width')) // same for border
		- stripPx($('#chat_chatlog').css('border-bottom-width'))
		- stripPx($('#chat_chatboxwrapper').css('margin-top')) // remove the height of the spacer above the chatbox
		- $('#chat_chatboxwrapper').outerHeight() // remove the height of the chatbox wrapper
		- stripPx($('#chat_chatboxwrapper').css('margin-top')); // allow an extra height of a spacer below the chatbox wrapper (it doesn't actually exist, but we need to account for the space there)
	if (newChatLogHeight < 200) {
		newChatLogHeight = 200;
		// if the scrollbars are needed, enable them
		$('body').css('overflow-y', 'auto');
	}
	$('#chat_chatlog').css('height', newChatLogHeight);
	$('#chat_chatbox').focus();
}

var welcomeTabOkActive = false;

function welcomeTabOkHandler() {
	if (!welcomeTabOkActive) {
		myName = $.trim($('#welcome_myname').val());
		// if no valid name was entered, generate one
		if (myName.length == 0) {
			myName = generateName();
		}
		ajaxJsonGetSessionId(
			myName,
			function() {
				// GuestJoinCommand, site id, name, ...
				queueAjaxCommand([1, "virtivia", myName, myColor, myIcon]);
			},
			function() {
				alert("Failed to acquire Session ID");
				nextOutSequence = 0; // reset this to 0
			}
		);
	}
}

$(document).ready(function() {
	welcomeTab = $('#welcome_tab');
	chatTab = $('#chat_tab');

	// initially, these are invisible
	$('#welcome_icon').fadeTo(0, 0);
	$('#chat_mycard').fadeTo(0, 0);
	$('#chat_myicon').fadeTo(0, 0);
	$('#chat_theiricon').fadeTo(0, 0);
	// and these are hidden
	$('#chat_theircardrow').hide();
	$('#chat_inlinecardrow').hide();

	// generate an initial color and set it
	myColor = generatePersonColor();
	$('#welcome_myname').css('color', myColor);

	// also pick a random icon to start with
	myIcon = randomizeProfileIcon();
	replaceIconWith(myIcon, $('#welcome_icon'));

	// set the waiting clock icon
	replaceIconWith('images/waiting_clock.png', $('#chat_waiticon'));

	// welcome tab handlers
	$('#welcome_btn_randomize').click(function(e) {
		$('#welcome_myname').fadeTo(100, 0, function() {
			// name
			$('#welcome_myname').val(generateName($('#welcome_myname').val()));
			// color
			myColor = generatePersonColor();
			$('#welcome_myname').css('color', myColor);
			// icon
			myIcon = randomizeProfileIcon(myIcon);
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
	$('#chat_chatbox').keypress(function(e) {
		if (e.which == 13) { // enter
			queueAjaxCommand([4, $('#chat_chatbox').val()]);
			writeMessageToChatLog(me.name, me.color, $('#chat_chatbox').val(), $('#chat_chatlog'));
			$('#chat_chatbox').val('');
		}
	});

	// we start on welcometab
	changeTabTo(welcomeTab);
	// DEBUG
	//changeTabTo(chatTab);
	//updatePositionInLine(5);
	// END OF DEBUG

	$(window).resize(onResize);
});

