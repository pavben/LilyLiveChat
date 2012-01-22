var currentTab = null;

// these will be set onload
var loginTab = null;
var mainTab = null;

$(document).ready(function() {
	loginTab = $('#login_tab');
	mainTab = $('#main_tab');

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

	//changeTabTo(loginTab);
	changeTabTo(mainTab);

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
				// GuestJoinCommand, site id, name, ...
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
			/*
			if (currentTab == welcomeTab) {
				replaceMeWith(new Person(myName, myColor, 'Guest', myIcon));
				changeTabTo(chatTab);
			}
			*/
			alert("Login successful");
			break;
		case 10: // OperatorLoginFailedMessage
			alert("Login failed: Invalid credentials");
			break;
		default: // Invalid message type
			alert("Got invalid messagea type: " + messageTypeId);
	}
}

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
}

