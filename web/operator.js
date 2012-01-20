var currentTab = null;

// these will be set onload
var loginTab = null;

$(document).ready(function() {
	loginTab = $('#login_tab');

	changeTabTo(loginTab);

	$(window).resize(onResize);
});

function onResize() {
	if (currentTab == loginTab) {
		onLoginTabResize();
	}
}

function onLoginTabResize() {
	// disable scrolling as it causes visual glitches
	$('body').css('overflow-y', 'hidden');
	var newLoginTabHeight = $(window).height();
	if (newLoginTabHeight < 641) {
		newLoginTabHeight = 641;
		// if the scrollbars are needed, enable them
		$('body').css('overflow-y', 'auto');
	}
	var spaceToFill = newLoginTabHeight - $('#login_middle').outerHeight();
	var newLoginTabTopHeight = Math.floor(spaceToFill / 2);
	var newLoginTabBottomHeight = Math.ceil(spaceToFill / 2); // bottom gets the extra pixel
	$('#login_top').css('height', newLoginTabTopHeight);
	$('#login_bottom').css('height', newLoginTabBottomHeight);
	$('#login_tab').css('height', newLoginTabHeight);
	log("new login tab height would be: " + newLoginTabHeight);
}

