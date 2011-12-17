$(document).ready(function() {
	// set these globals
	welcomeTab = $('#welcome_tab');
	chatTab = $('#chat_tab');

	$('#chat_chatbox').keypress(function(e) {
		if (e.which == 13) { // enter
			writeMessageToChatLog(me.name, me.color, $('#chat_chatbox').val());
			$('#chat_chatbox').val('');
		}
	});

	function writeMessageToChatLog(name, nameColor, msg) {
		var content = '';
		content += '<span class="msgname" style="color:' + nameColor + '">' + name + '</span>';
		content += '<span class="msgtext">: ' + msg + '</span><br/>';

		writeContentToChatLog(content);
	}

	function writeContentToChatLog(content) {
		var chatlogDiv = $('#chat_chatlog');

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

	function Person(name, color, title, iconUrl) {
		this.name = name;
		this.color = color;
		this.title = title;
		this.iconUrl = iconUrl;
	}

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

		icon.fadeTo(fadeOutTime, 0);

		icon.css('background-image', 'none');

		var iconCache = new Image();
		iconCache.onload = function() {
			icon.css('background-image', 'url(\'' + iconUrl + '\')');
			icon.fadeTo(1000, 1);
		}
		iconCache.src = iconUrl;
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

	// initially, these are invisible
	$('#welcome_icon').fadeTo(0, 0);
	$('#chat_mycard').fadeTo(0, 0);
	$('#chat_myicon').fadeTo(0, 0);
	$('#chat_theiricon').fadeTo(0, 0);
	// and these are hidden
	$('#chat_theircardrow').hide();
	$('#chat_inlinecardrow').hide();

	setTimeout(function() {
		testperson = new Person('Circular Cat', generateNewPersonColor(), 'Guest', 'images/funshine_bear.png');
		replaceMeWith(testperson);
		//replaceThemWith(testperson);
	}, 800);

	function generateNewPersonColor() {
		return '#085376';
	}

	// we start on welcometab
	changeTabTo(welcomeTab);
	// DEBUG
	//changeTabTo(chatTab);
	// END OF DEBUG

	replaceIconWith('images/funshine_bear.png', $('#welcome_icon'));
	replaceIconWith('images/waiting_clock.png', $('#chat_waiticon'));

	setTimeout(function() {
		//changeTabTo('chattab');
	}, 500);
});

var currentTab = null;

var welcomeTab = null;
var chatTab = null;

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

$(window).resize(onResize);

function onResize() {
	if (currentTab == welcomeTab) {
		onWelcomeTabResize();
	} else if (currentTab == chatTab) {
		onChatTabResize();
	}
}

function onWelcomeTabResize() {
	// disable scrolling as it causes visual glitches
	$('html').css('overflow-y', 'hidden'); // TODO: should this be html or perhaps body?
	var newWelcomeTabHeight = $(window).height();
	if (newWelcomeTabHeight < 641) {
		newWelcomeTabHeight = 641;
		// if the scrollbars are needed, enable them
		$('html').css('overflow-y', 'auto');
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
	$('html').css('overflow-y', 'hidden');
	var chatlogDiv = $('#chat_chatlog');
	var newChatLogHeight = $(window).height() // start with the full height
		- chatlogDiv.offset().top // remove all up to the start of chatlog
		- stripPx($('#chat_chatlog').css('padding-top')) // top and bottom paddings are not counted in the height
		- stripPx($('#chat_chatlog').css('padding-bottom'))
		- stripPx($('#chat_chatboxwrapper').css('margin-top')) // remove the height of the spacer above the chatbox
		- $('#chat_chatboxwrapper').outerHeight() // remove the height of the chatbox wrapper
		- stripPx($('#chat_chatboxwrapper').css('margin-top')); // allow an extra height of a spacer below the chatbox wrapper (it doesn't actually exist, but we need to account for the space there)
	if (newChatLogHeight < 200) {
		newChatLogHeight = 200;
		// if the scrollbars are needed, enable them
		$('html').css('overflow-y', 'auto');
	}
	$('#chat_chatlog').css('height', newChatLogHeight);
	$('#chat_chatbox').focus();
}

function stripPx(text) {
	return text.replace('px', '');
}

function log(msg) {
	window.console.log(msg);
}

