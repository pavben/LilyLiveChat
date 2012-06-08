// these will be set onload
// TODO: activationSuccessfulTab is currently not used
var activationSuccessfulTab = null;
var miscMessageTab = null;

$(window).bind('load', function() {
	activationSuccessfulTab = $('#activationsuccessful_tab');
	miscMessageTab = $('#miscmessage_tab');

	$(window).resize(onResize);

	ajaxJsonGetSessionId(
		function() {
			queueAjaxCommand([Messages.UnregisteredSelectSiteMessage, siteId]);
		},
		function() {
			resetSession();

			showCantConnectScreen();
		}
	);
});

function handleMessage(message) {
	messageTypeId = message.shift();
	log("Msg Type Id: " + messageTypeId);
	log(message);
	switch (messageTypeId) {
		case Messages.UnregisteredSiteSelectedMessage:
			var siteName = message[0];
			var isActive = message[1]; // we don't care if it's active or not for activations
			var isActivated = message[2];

			if (isActivated) {
				// if already activated, show message
				showAlreadyActivatedScreen();
			} else {
				var sessionId = $.cookie('sessionId');
				if (sessionId === null) {
					// if no sessionId, get one and come back
					redirectToLoginAndBack();
				} else {
					// otherwise, let's activate the site
					queueAjaxCommand([Messages.CSMTUnregisteredActivateAdmin, sessionId]);
				}
			}

			break;
		case Messages.CSMTUnregisteredActivateAdminSuccess:
			// redirect to the admin panel on success
			window.location = 'http://lilylivechat.net/admin/' + siteId;
			break;
		case Messages.CSMTUnregisteredActivateAdminFailure:
			// TODO: Show a more appropriate screen
			showDisconnectedScreen();
			break;
		case Messages.UnregisteredSiteInvalidMessage:
			// display the invalid site screen
			showInvalidSiteScreen();

			break;
		case Messages.SomethingWentWrongMessage:
		case Messages.CSUnavailableMessage:
			// TODO PL: Show more appropriate screens for these
			showDisconnectedScreen();
			break;
		case Messages.CSMTWrongChatServer:
			refreshThroughSiteLocator();
			break;
		default:
			log("Received an unknown message!");
			break;
	}
}

function handleSessionEnded() {
	switch (getCurrentTabOrTarget()) {
	case activationSuccessfulTab:
	case miscMessageTab:
		break;
	default:
		// in all other cases, just show the connection problems screen
		showDisconnectedScreen();
	}
}

function showAlreadyActivatedScreen() {
	showMiscMessageTab('Already activated...',
		$('<div/>').addClass('miscmessage_content_textwrapper').append(
			$('<div/>').text('Your LilyLiveChat account is already activated.')
		),
		$('<div/>').addClass('fixedtable').addClass('miscmessage_buttontable').append(
			$('<div/>').addClass('tablerow').append(
				$('<div/>').addClass('cell')
			).append(
				$('<div/>').addClass('cell').css('width', '110px').append(
					$('<div/>').addClass('basicbutton').text('Admin Panel').click(function() {
						window.location = 'http://lilylivechat.net/admin/' + siteId;
					})
				)
			)
		)
	);
}

function showInvalidSiteScreen() {
	showMiscMessageTab('Invalid Site',
		$('<div/>').addClass('miscmessage_content_textwrapper').append(
			$('<div/>').text('This site isn\'t registered with LilyLiveChat.')
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
					$('<div/>').addClass('basicbutton').text('Try again').click(function() {
						refreshThroughSiteLocator();
					})
				)
			)
		)
	);
}

function onResize() {
	if (currentTab == activationSuccessfulTab) {
		onBasicVCenterResizeMinPadding('activationsuccessful', 50);
	} else if (currentTab == miscMessageTab) {
		onBasicVCenterResizeMinPadding('miscmessage', 50);
	}
}

