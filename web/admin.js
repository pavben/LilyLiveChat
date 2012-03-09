// these will be set onload
var loginTab = null;
var mainTab = null;
var miscMessageTab = null;

// Person object representing the operator
var me = null;

$(window).bind('load', function() {
	loginTab = $('#login_tab');
	mainTab = $('#main_tab');
	miscMessageTab = $('#miscmessage_tab');

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

		var password = $.trim($('#login_password').val());

		// TODO: replace this check with the appropriate enabling/disabling of the login button
		if (password.length == 0) {
			// TODO: REMOVE THIS DEV CODE
			password = "mike";
			/*
			alert("Please enter both the username and password.");
			loginTabOkActive = false;
			return;
			*/
		}

		queueAjaxCommand([Messages.AdminLoginRequestMessage, password]);
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
			$('#login_adminloginlabel').text(siteName + ' Admin Login');

			changeTabTo(loginTab, function () {
				// focus the username box
				$('#login_password').focus();
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
		case Messages.AdminLoginSuccessMessage:
			if (currentTab == loginTab) {
				changeTabTo(mainTab);
			}
			log("Login successful");
			//queueAjaxCommand([Messages.AdminOperatorCreateMessage, "mike2", "mike", "Michael", "#000000", "Representative", "images/cc/panda.png"]);
			//queueAjaxCommand([Messages.AdminOperatorReplaceMessage, 1, "mike2", "mike", "Michael", "#000000", "Representative", "images/cc/panda.png"]);
			//queueAjaxCommand([Messages.AdminSetSiteNameMessage, "Virtivia"]);
			break;
		case Messages.AdminLoginFailedMessage:
			showLoginFailedScreen();
			break;
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

function showLoginFailedScreen() {
	showMiscMessageTab('No match...',
		$('<div/>').addClass('miscmessage_content_textwrapper').append(
			$('<div/>').text('Can\'t remember the password? You can reset it from the payment system (TODO).')
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
	} else if (currentTab == mainTab) {
		onBasicVCenterResize('main', 530);
	} else if (currentTab == miscMessageTab) {
		onBasicVCenterResize('miscmessage', 530);
	}
}

