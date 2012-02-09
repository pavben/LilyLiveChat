var currentTab = null;

// these will be set onload
var welcomeTab = null;
var chatTab = null;

var myName = null;
var myColor = null;
var myIcon = null;

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
		case Messages.CustomerInLinePositionMessage:
			if (currentTab == welcomeTab) {
				replaceMeWith(new Person(myName, myColor, 'Customer', myIcon));
				changeTabTo(chatTab);
			}
			updatePositionInLine(parseInt(message[0]));
			break;
		case Messages.CustomerNowTalkingToMessage:
			var name = message[0];
			var color = message[1];
			var title = message[2];
			var iconUrl = message[3];
			replaceThemWith(new Person(name, color, title, iconUrl));
			break;
		case Messages.CustomerReceiveChatMessage:
			var text = message[0];
			if (they == null) {
				alert("CustomerReceiveChatMessage when no operator is present");
				return;
			}
			writeMessageToChatLog(they.name, they.color, text, $('#chat_chatlog'));
			break;
		case Messages.SomethingWentWrongMessage:
			break;
		case Messages.CustomerChatEndedMessage:
			// TODO
			break;
		default: // Invalid message type
			log("Got invalid message type: " + messageTypeId);
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

function updatePositionInLine(position) {
	changeRightSpaceDivTo($('#chat_inlinecell'), function() {
		$('#chat_inlinepos').text(position).append(
			$('<sup/>').text(getOrdinalSuffixFor(position))
		);
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
		onBasicVCenterResize('welcome', 641);
	} else if (currentTab == chatTab) {
		onChatTabResize();
	}
}

function onChatTabResize() {
	// disable scrolling as it interferes with calculations and causes visual glitches
	$('body').css('overflow-y', 'hidden');
	var chatlogDiv = $('#chat_chatlog');
	var chatboxWrapper = $('#chat_chatboxwrapper');
	var newChatLogHeight = $(window).height() // start with the full height
		- chatlogDiv.offset().top // remove all up to the start of chatlog
		- stripPx(chatlogDiv.css('padding-top')) // top and bottom paddings are not counted in the height
		- stripPx(chatlogDiv.css('padding-bottom'))
		- stripPx(chatlogDiv.css('border-top-width')) // same for border
		- stripPx(chatlogDiv.css('border-bottom-width'))
		- stripPx(chatboxWrapper.css('margin-top')) // remove the height of the spacer above the chatbox
		- chatboxWrapper.outerHeight() // remove the height of the chatbox wrapper
		- stripPx($('#chat_tab').css('padding-bottom')); // remove the height of the spacer below the chatbox

	if (newChatLogHeight < 200) {
		newChatLogHeight = 200;
		// if the scrollbars are needed, enable them
		$('body').css('overflow-y', 'auto');
	}

	chatlogDiv.css('height', newChatLogHeight);
	$('#chat_chatbox').focus();
}

var welcomeTabOkActive = false;

function welcomeTabOkHandler() {
	if (!welcomeTabOkActive) {
		welcomeTabOkActive = true;

		myName = $.trim($('#welcome_myname').val());
		// if no valid name was entered, generate one
		if (myName.length == 0) {
			myName = generateName();
		}
		ajaxJsonGetSessionId(
			function() {
				// site id, name, ...
				queueAjaxCommand([Messages.CustomerJoinMessage, "virtivia", myName, myColor, myIcon]);

				// re-enable the OK button
				welcomeTabOkActive = false;
			},
			function() {
				alert("Failed to acquire Session ID");
				nextOutSequence = 0; // reset this to 0

				// re-enable the OK button
				welcomeTabOkActive = false;
			}
		);
	}
}

var soundManager = null;

function SoundManager() {
	instance = this;

	// formats are listed in order of preference
	formats = [
		{
			mime: 'audio/ogg; codecs=vorbis',
			ext: 'ogg'
		},
		{
			mime: 'audio/mpeg',
			ext: 'mp3'
		}
	];

	// determine which audio format to use, if any
	{
		var dummyAudio = new Audio();
		for (var i = 0; i < formats.length; i++) {
			currentFormat = formats[i];

			if (dummyAudio.canPlayType(currentFormat.mime)) {
				this.fileExt = currentFormat.ext;
				break;
			}
		}
	}

	filesToLoad = [
		'youare9thinline',
		'youarenow8thinline',
		'7thinline',
		'youarenow6thinline',
		'youarenow5thinline',
		'youre4thinline',
		'youarenow3rd',
		'youre2ndinline',
		'getreadyyourenextinline',
		'nowspeakingwithrep'
	];

	if (!this.fileExt) {
		log("Cannot load SoundManager due to no compatible formats. Audio will be disabled.");
		return;
	}

	this.sounds = {};
	{
		soundsLoaded = 0;
		this.allSoundsLoaded = false;
		for (var i = 0; i < filesToLoad.length; i++) {
			var currentFile = filesToLoad[i];
			var filePath = 'audio/' + currentFile + '.' + this.fileExt;
			var s = new Audio(filePath);
			s.addEventListener('canplaythrough', function() {
				log("canplaythrough called");
				this.removeEventListener('canplaythrough', arguments.callee, false);
				soundsLoaded++;
				log("sounds loaded: " + soundsLoaded);
				if (soundsLoaded == filesToLoad.length) {
					instance.allSoundsLoaded = true;
					instance.considerPlayNextSafe();
				}
			}, false);
			s.addEventListener('error', function() {
				log("Unable to load sound: " + filePath);
			}, false);
			s.load();
			instance.sounds[currentFile] = s;
		}
	}

	this.playlist = [];

	this.currentlyPlaying = false;

	this.play = function(soundName) {
		log("play called with sound = " + soundName);
		var s = instance.sounds[soundName];
		if (s) {
			log('added to playlist');
			instance.playlist.push(s);

			instance.considerPlayNextSafe();
		} else {
			log('Invalid sound name: ' + soundName);
		}
	}

	this.considerPlayNextSafe = function() {
		if (!instance.currentlyPlaying && instance.allSoundsLoaded) {
			instance.considerPlayNext();
		}
	}

	this.considerPlayNext = function() {
		instance.currentlyPlaying = true;

		if (instance.playlist.length > 0) {
			var s = instance.playlist.shift();
			s.addEventListener('ended', function() {
				log("ended");
				this.removeEventListener('ended', arguments.callee, false);

				instance.currentlyPlaying = false;

				setTimeout(instance.considerPlayNext, 0);
			});
			s.play();
		}
	}
}

$(document).ready(function() {
	welcomeTab = $('#welcome_tab');
	chatTab = $('#chat_tab');

	// initially, these are invisible
	$('#welcome_icon').fadeTo(0, 0);
	$('#chat_mycardcell').fadeTo(0, 0);
	$('#chat_myicon').fadeTo(0, 0);
	$('#chat_theiricon').fadeTo(0, 0);
	// and these are hidden
	$('#chat_theircardcell').hide();
	$('#chat_inlinecell').hide();

	// initialize the sound manager
	soundManager = new SoundManager();
	/*
	soundManager.play('youare9thinline');
	soundManager.play('youarenow8thinline');
	soundManager.play('7thinline');
	soundManager.play('youarenow6thinline');
	soundManager.play('youarenow5thinline');
	soundManager.play('youre4thinline');
	soundManager.play('youarenow3rd');
	soundManager.play('youre2ndinline');
	soundManager.play('getreadyyourenextinline');
	soundManager.play('nowspeakingwithrep');
	*/

	// generate an initial color and set it
	myColor = generatePersonColor();
	$('#welcome_myname').css('color', myColor);

	// also pick a random icon to start with
	myIcon = randomizeProfileIcon();
	replaceIconWith(myIcon, $('#welcome_icon'));

	// set the waiting clock icon
	replaceIconWith('images/waiting_clock.png', $('#chat_waiticon'));

	// welcome tab handlers
	// TODO: If the user has changed the name value to something non-empty, do not reset it with Randomize
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
		if (e.which == 13 && !e.shiftKey && !e.altKey && !e.ctrlKey) { // enter
			queueAjaxCommand([Messages.CustomerSendChatMessage, $('#chat_chatbox').val()]);
			writeMessageToChatLog(me.name, me.color, $('#chat_chatbox').val(), $('#chat_chatlog'));
			log("[" + $('#chat_chatbox').val() + "]");
			log(e);
			$('#chat_chatbox').val('');
			return false;
		}
	});
	
	// initialize the auto-growing chatbox and append the shadow div to the chatboxwrapper
	initializeAutoGrowingTextArea($('#chat_chatbox'), $('#chat_chatboxwrapper'));

	// we start on welcometab
	changeTabTo(welcomeTab);
	// DEBUG
	//changeTabTo(chatTab);
	//updatePositionInLine(5);
	// END OF DEBUG
	
	// TEMP: remove this
	$('#welcome_btn_ok').click();

	$(window).resize(onResize);
});

