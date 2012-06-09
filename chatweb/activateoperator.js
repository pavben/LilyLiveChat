// these will be set onload
var beforeActivateTab = null;
// TODO: activationSuccessfulTab is currently not used
var activationSuccessfulTab = null;
var miscMessageTab = null;

var operatorId;
var activationToken;

$(window).bind('load', function() {
	beforeActivateTab = $('#beforeactivate_tab');
	activationSuccessfulTab = $('#activationsuccessful_tab');
	miscMessageTab = $('#miscmessage_tab');

	$(window).resize(onResize);

	operatorId = getUrlParameter('operatorId');
	if (operatorId !== null) {
		operatorId = parseInt(operatorId);
	}
	activationToken = getUrlParameter('activationToken');

	if (operatorId !== null && activationToken !== null) {
		ajaxJsonGetSessionId(
			function() {
				queueAjaxCommand([Messages.UnregisteredSelectSiteMessage, siteId]);
			},
			function() {
				resetSession();

				showCantConnectScreen();
			}
		);
	} else {
		showInvalidParametersScreen();
	}
});

function handleMessage(message) {
	messageTypeId = message.shift();
	log("Msg Type Id: " + messageTypeId);
	log(message);
	switch (messageTypeId) {
		case Messages.UnregisteredSiteSelectedMessage:
			var siteName = message[0];
			var isActive = message[1]; // we don't care if it's active or not for activations
			var isActivated = message[2]; // this is only useful for admin activation

			queueAjaxCommand([Messages.CSMTUnregisteredIsOperatorActivated, operatorId]);

			break;
		case Messages.CSMTUnregisteredIsOperatorActivatedResponse:
			var isActivated = message[0];

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
					queueAjaxCommand([Messages.CSMTUnregisteredActivateOperator, sessionId, operatorId, activationToken]);
				}
			}

			break;
		case Messages.CSMTUnregisteredActivateOperatorSuccess:
			// redirect to the operator panel on success
			window.location = 'http://lilylivechat.net/operator/' + siteId;
			break;
		case Messages.CSMTFailure:
			showOperatorDeletedScreen();
			break;
		case Messages.CSMTUnregisteredActivateOperatorFailure:
			// TODO: Show a more appropriate screen
			showActivationFailedScreen();
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

// TODO: check this
function showAlreadyActivatedScreen() {
	showMiscMessageTab('Already activated...',
		$('<div/>').addClass('miscmessage_content_textwrapper').append(
			$('<div/>').text('Your LilyLiveChat account is already activated.')
		),
		$('<div/>').addClass('fixedtable').addClass('miscmessage_buttontable').append(
			$('<div/>').addClass('tablerow').append(
				$('<div/>').addClass('cell')
			).append(
				$('<div/>').addClass('cell').css('width', '125px').append(
					$('<div/>').addClass('basicbutton').text('Operator Panel').click(function() {
						window.location = 'http://lilylivechat.net/operator/' + siteId;
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

function showInvalidParametersScreen() {
	showMiscMessageTab('Oops...',
		$('<div/>').addClass('miscmessage_content_textwrapper').append(
			$('<div/>').text('It looks like you may have followed an invalid or incomplete link.')
		)
	);
}

function showOperatorDeletedScreen() {
	showMiscMessageTab('Oops...',
		$('<div/>').addClass('miscmessage_content_textwrapper').append(
			$('<div/>').text('It looks like you\'ve followed an outdated link. This operator account no longer exists. If you believe this is a mistake, contact your site admin.')
		)
	);
}

function showActivationFailedScreen() {
	showMiscMessageTab('Activation failed',
		$('<div/>').addClass('miscmessage_content_textwrapper').append(
			$('<div/>').text('This is likely because your Google account is already linked to another operator for this site. If not, contact us and we\'ll help you sort it out.')
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
	if (currentTab == beforeActivateTab) {
		onBasicVCenterResizeMinPadding('beforeactivate', 50);
	} else if (currentTab == activationSuccessfulTab) {
		onBasicVCenterResizeMinPadding('activationsuccessful', 50);
	} else if (currentTab == miscMessageTab) {
		onBasicVCenterResizeMinPadding('miscmessage', 50);
	}
}

