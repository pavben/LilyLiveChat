$(document).ready(function() {
	$('#chatbox').keypress(function(e) {
		if (e.which == 13) { // enter
			$('#chatbox').val('');
		}
	});

	$(window).resize(updateChatLogHeight);

	function updateChatLogHeight() {
		// disable scrolling as it interferes with calculations and causes visual glitches
		$('html').css({overflow:'hidden'});
		var newChatLogHeight = $(window).height() - $('#chatlog').offset().top - 11 - $('#chatboxwrapper').height() - 11;
		if (newChatLogHeight < 200) {
			newChatLogHeight = 200;
			// if the scrollbars are needed, enable them
			$('html').css({overflow:'auto'});
		}
		$('#chatlog').css({height: newChatLogHeight + 'px'});
	}

	updateChatLogHeight();
});
