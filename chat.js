$(document).ready(function() {
	$('#chatbox').keypress(function(e) {
		if (e.which == 13) { // enter
			writeMessageToChatLog(me.name, me.color, $('#chatbox').val());
			$('#chatbox').val('');
		}
	});

	function writeMessageToChatLog(name, nameColor, msg) {
		var content = '';
		content += '<span class="msgname" style="color:' + nameColor + '">' + name + '</span>';
		content += '<span class="msgtext">: ' + msg + '</span><br/>';

		writeContentToChatLog(content);
	}

	function writeContentToChatLog(content) {
		var chatlogDiv = $('#chatlog');

		chatlogDiv.append(content);

		if (this.lastScrollTopTarget && chatlogDiv.scrollTop() >= this.lastScrollTopTarget - 50) {
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
		replaceCardWith(person, $('#myicon'), $('#mycard'), $('#myname'), $('#mytitle'));
	}

	function replaceThemWith(person) {
		they = person;
		replaceCardWith(person, $('#theiricon'), $('#theircard'), $('#theirname'), $('#theirtitle'));
	}

	function replaceCardWith(person, icon, card, name, title) {
		var fadeOutTime = 100;
		if (!me) {
			fadeOutTime = 0;
		}

		icon.fadeTo(fadeOutTime, 0);
		card.fadeTo(fadeOutTime, 0);

		// now both the icon and the card are invisible

		icon.css('background-image', 'none');

		name.html(person.name);
		name.css('color', person.color);

		var iconCache = new Image();
		iconCache.onload = function() {
			icon.css('background-image', 'url(\'' + person.iconUrl + '\')');
			icon.fadeTo(1000, 1);
		}
		iconCache.src = person.iconUrl;

		title.html(person.title);

		card.fadeTo(1000, 1);

	}

	// initially, these are invisible
	$('#myicon').fadeTo(0, 0);
	$('#mycard').fadeTo(0, 0);
	$('#theiricon').fadeTo(0, 0);
	$('#theircard').fadeTo(0, 0);

	testperson = new Person('Circular Cat', generateNewPersonColor(), 'Guest', 'images/guest-icon.png');
	replaceMeWith(testperson);

	function generateNewPersonColor() {
		return '#085376';
	}

	var currentTab = null;

	function changeToTab(tab) {
		if (currentTab) {
			$('#' + currentTab).fadeTo(300, 0, function() {
				$('#' + currentTab).hide();

				onOldTabGone();
			});
		} else {
			onOldTabGone();
		}

		function onOldTabGone() {
			currentTab = tab;
			$('#' + currentTab).fadeTo(600, 1);

			onResize();
		}
	}

	// we start on welcometab
	changeToTab('welcometab');

	setTimeout(function() {
		changeToTab('chattab');
	}, 4000);

	function log(msg) {
		window.console.log(msg);
	}

	$(window).resize(onResize);

	function onResize() {
		if (currentTab == 'chattab') {
			updateChatLogHeight();
		}
	}

	function updateChatLogHeight() {
		// disable scrolling as it interferes with calculations and causes visual glitches
		$('html').css({overflow:'hidden'});
		var chatlogDiv = $('#chatlog');
		var newChatLogHeight = $(window).height() // start with the full height
			- chatlogDiv.offset().top // remove all up to the start of chatlog
			- $('.spacer9v').outerHeight() // remove the height of the spacer above the chatbox
			- $('#chatboxwrapper').outerHeight() // remove the height of the chatbox wrapper
			- $('.spacer9v').height() // allow an extra height of a spacer below the chatbox wrapper (it doesn't actually exist, but we need to account for the space there)
			- parseInt(chatlogDiv.css('padding-top')) // paddings are not double-counted in the width
			- parseInt(chatlogDiv.css('padding-bottom'));
		if (newChatLogHeight < 200) {
			newChatLogHeight = 200;
			// if the scrollbars are needed, enable them
			$('html').css({overflow:'auto'});
		}
		$('#chatlog').css({height: newChatLogHeight + 'px'});
		$('#chatbox').focus();
	}
});
