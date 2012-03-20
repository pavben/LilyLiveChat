// these will be set onload
var loginTab = null;
var mainTab = null;
var miscMessageTab = null;

var generalSubtab = null;
var operatorsSubtab = null;
var editOperatorSubtab = null;

// Person object representing the operator
var me = null;

// current icon visible on the edit operator subtab
var editOperatorCurrentIcon = null;
var editOperatorCurrentColor = null;

$(window).bind('load', function() {
	loginTab = $('#login_tab');
	mainTab = $('#main_tab');
	miscMessageTab = $('#miscmessage_tab');

	generalSubtab = $('#main_rightcell_general');
	operatorsSubtab = $('#main_rightcell_operators');
	editOperatorSubtab = $('#main_rightcell_editoperator');

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
	
	// main tab handlers
	onChangeToFieldValue($('#main_general_sitename'), onSiteNameChange);
	$('#main_general_sitename_btn_save').click(siteNameSaveHandler);

	// edit operator subtab
	$('#main_editoperator_btn_changecolor').click(function() {
		editOperatorSetCurrentColor(generatePersonColor());
	});

	$('#main_editoperator_btn_nexticon').click(function() {
		var nextIcon = iconsAndSuffixes[0][0]; // default to the first
		var foundCurrentIcon = false;

		for (var i = 0; i < iconsAndSuffixes.length; i++) {
			var thisIcon = iconsAndSuffixes[i][0];

			if (foundCurrentIcon) {
				nextIcon = thisIcon;
				break;
			}
			else if (thisIcon === editOperatorCurrentIcon) {
				foundCurrentIcon = true;
			}
		}

		editOperatorCurrentIcon = nextIcon;

		replaceIconWith(editOperatorCurrentIcon, $('#main_editoperator_icon'));
	});

	onChangeToFieldValue($('#main_editoperator_name'), function() {
		onNameOrTitleEdited('name');
	});

	onChangeToFieldValue($('#main_editoperator_title'), function() {
		onNameOrTitleEdited('title');
	});

	$('#main_operators_addnew').click(function() {
		addOrEditOperatorHandler(null);
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

function editOperatorSetCurrentColor(color) {
	editOperatorCurrentColor = color;
	$('#main_editoperator_name_preview').css('color', editOperatorCurrentColor);
	$('#main_editoperator_name').css('color', editOperatorCurrentColor);
}

function addOrEditOperatorHandler(operatorId, username, name, color, title, iconUrl) {
	var edit = (operatorId !== null);

	$('#main_editoperator_name').val(edit ? name : 'Name');
	onNameOrTitleEdited('name');

	$('#main_editoperator_title').val(edit ? title : 'Representative');
	onNameOrTitleEdited('title');

	editOperatorSetCurrentColor(edit ? color : generatePersonColor());

	if (edit) {
		editOperatorCurrentIcon = iconUrl;
	} else {
		// start off with a random icon
		editOperatorCurrentIcon = iconsAndSuffixes[Math.floor(Math.random() * iconsAndSuffixes.length)][0];
	}

	replaceIconWith(editOperatorCurrentIcon, $('#main_editoperator_icon'));

	$('#main_editoperator_username').val(edit ? username : '');

	$('#main_editoperator_password').val('');

	var buttonsTableRow = $('<div/>').addClass('tablerow');

	if (edit) {
		buttonsTableRow.append(
			$('<div/>').addClass('cell').css('width', '70px').append(
				$('<div/>').addClass('smallbutton').text('Delete').click(function() {
					queueAjaxCommand([
						Messages.AdminOperatorDeleteMessage,
						operatorId
					]);
				})
			)
		);
	}

	buttonsTableRow.append(
		$('<div/>').addClass('cell')
	).append(
		$('<div/>').addClass('cell').css('width', '90px').append(
			$('<div/>').addClass('smallbutton').text('Cancel').click(function() {
				changeSubtabTo(operatorsSubtab);
			})
		)
	).append(
		$('<div/>').addClass('cell').css('width', '8px')
	).append(
		$('<div/>').addClass('cell').css('width', '100px').append(
			$('<div/>').addClass('smallbutton').text(edit ? 'Save' : 'Create').click(function() {
				var newName = $.trim($('#main_editoperator_name').val());
				if (newName === '') {
					$('#main_editoperator_name').val('Name');
					$('#main_editoperator_name').focus();
					onNameOrTitleEdited('name');
					return;
				}
				var newTitle = $.trim($('#main_editoperator_title').val());
				if (newTitle === '') {
					$('#main_editoperator_title').val('Title');
					$('#main_editoperator_title').focus();
					onNameOrTitleEdited('title');
					return;
				}
				var newUsername = $.trim($('#main_editoperator_username').val());
				if (newUsername === '') {
					$('#main_editoperator_username').val('Username');
					$('#main_editoperator_username').focus();
					return;
				}
				var newPassword = $.trim($('#main_editoperator_password').val());
				if (!edit && newPassword === '') {
					$('#main_editoperator_password').focus();
					return;
				}

				if (edit) {
					queueAjaxCommand([
						Messages.AdminOperatorReplaceMessage,
						operatorId,
						newUsername,
						newPassword,
						newName,
						editOperatorCurrentColor,
						newTitle,
						editOperatorCurrentIcon
					]);
				} else {
					queueAjaxCommand([
						Messages.AdminOperatorCreateMessage,
						newUsername,
						newPassword,
						newName,
						editOperatorCurrentColor,
						newTitle,
						editOperatorCurrentIcon
					]);
				}
			})
		)
	);

	$('#main_editoperator_buttonsdiv').empty().append(
		$('<div/>').addClass('fixedtable').append(buttonsTableRow)
	);

	if (edit) {
		$('#main_editoperator_passwordnote').text('If empty, it will remain unchanged.');
	} else {
		$('#main_editoperator_passwordnote').text('Pick one that\'s hard to guess!');
	}

	changeSubtabTo(editOperatorSubtab);
}

// functions for instant tracking of changes to textboxes
function setFieldValue(field, value) {
	field.val(value)[0].lastKnownValue = value;
}

function onChangeToFieldValue(field, callback) {
	var checkForChange = function() {
		if (field.val() != field[0].lastKnownValue) {
			callback(field);
		}
	}

	field.keyup(checkForChange);
	field.change(checkForChange);
}

function onSiteNameChange(field) {
	if (!field[0].unsaved) {
		field[0].unsaved = true;

		$('#main_general_sitename_savedlabel').fadeTo(0, 0);
		$('#main_general_sitename_savediv').animate({height:'32px'}, 350);
	}
}

function siteNameSaveHandler() {
	var siteName = $('#main_general_sitename');

	// only send if currently not saved (this also prevents double-clicking the Save button)
	if (siteName[0].unsaved) {
		siteName[0].unsaved = false;
		queueAjaxCommand([Messages.AdminSetSiteNameMessage, $.trim(siteName.val())]);

		$('#main_general_sitename_savedlabel').fadeTo(500, 1);
		$('#main_general_sitename_savediv').delay(1800).animate({height:'0px'}, 400);
	}
}

function onNameOrTitleEdited(nameOrTitleStr) {
	$('#main_editoperator_' + nameOrTitleStr + '_preview').text($('#main_editoperator_' + nameOrTitleStr).val());
}

// main tab's subtabs

var currentSubtab = null;
var currentSubtabTarget = undefined;

function changeSubtabTo(subtab, onSubtabLoadCallback) {
	var alreadyBusy = (currentSubtabTarget !== undefined);

	currentSubtabTarget = subtab;

	if (!alreadyBusy) {
		if (currentSubtab) {
			currentSubtab.fadeOut(150, 0, function() {
				onOldSubtabGone();
			});
		} else {
			onOldSubtabGone();
		}
	}

	function onOldSubtabGone() {
		currentSubtab = subtab;
		if (currentSubtab === currentSubtabTarget) {
			currentSubtabTarget = undefined;
			currentSubtab.fadeTo(0, 0, function() {
				onResize();
				if (onSubtabLoadCallback) {
					onSubtabLoadCallback();
				}
				currentSubtab.fadeTo(300, 1);
			});
		} else {
			// here we reset currentSubtabTarget to fail the alreadyBusy check
			var target = currentSubtabTarget;
			currentSubtabTarget = undefined;
			changeSubtabTo(target);
		}
	}
}

function getCurrentSubtabOrTarget() {
	return (currentSubtabTarget !== undefined) ? currentSubtabTarget : currentSubtab;
}

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

var operatorsCount = null;

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
			$('#login_btn_ok').click();
			break;
		case Messages.UnregisteredSiteInvalidMessage:
			// display the invalid site screen
			showInvalidSiteScreen();

			break;
		case Messages.SomethingWentWrongMessage:
			break;
		case Messages.AdminLoginSuccessMessage:
			changeTabTo(mainTab);
			log("Login successful");
			//queueAjaxCommand([Messages.AdminOperatorCreateMessage, "mike2", "mike", "Michael", "#000000", "Representative", "images/cc/panda.png"]);
			//queueAjaxCommand([Messages.AdminOperatorReplaceMessage, 1, "mike2", "mike", "Michael", "#000000", "Representative", "images/cc/panda.png"]);
			//queueAjaxCommand([Messages.AdminSetSiteNameMessage, "Virtivia"]);
			changeSubtabTo(operatorsSubtab);
			break;
		case Messages.AdminLoginFailedMessage:
			showLoginFailedScreen();
			break;
		case Messages.AdminSiteInfoMessage:
			var siteId = message[0];
			var siteName = message[1];
			var siteExpiryTS = message[2];

			setFieldValue($('#main_general_sitename'), siteName);
			break;
		case Messages.AdminSetSiteNameSuccessMessage:
			// site name set successfully
			break;
		case Messages.AdminOperatorDetailsStartMessage:
			$('#main_operators_listbox').empty();
			operatorsCount = 0;
			break;
		case Messages.AdminOperatorDetailsMessage:
			var operatorId = message[0];
			var username = message[1];
			var name = message[2];
			var color = message[3];
			var title = message[4];
			var iconUrl = message[5];

			var listbox = $('#main_operators_listbox');

			if (!listbox.is(':empty')) {
				listbox.append(
					$('<div/>').addClass('main_rightcell_vspacer')
				).append(
					$('<div/>').addClass('main_rightcell_hline')
				).append(
					$('<div/>').addClass('main_rightcell_vspacer')
				);
			}
			$('#main_operators_listbox').append(
				$('<div/>').addClass('main_operators_listbox_item').append(
					$('<div/>').addClass('fixedtable').append(
						$('<div/>').addClass('tablerow').append(
							$('<div/>').addClass('iconcell').append(
								$('<div/>').attr('id', 'main_operators_' + operatorId + '_icon').addClass('framedicon')
							)
						).append(
							$('<div/>').addClass('cardtextwrappercell').append(
								$('<div/>').addClass('leftcardtext').append(
									$('<div/>').addClass('personname').css('color', color).text(name)
								).append(
									$('<div/>').addClass('persontitle').text(title)
								)
							)
						)
					)
				).click(function() {
					addOrEditOperatorHandler(operatorId, username, name, color, title, iconUrl);
				})
			);

			replaceIconWith(iconUrl, $('#main_operators_' + operatorId + '_icon'));

			operatorsCount++;

			break;
		case Messages.AdminOperatorDetailsEndMessage:
			var listbox = $('#main_operators_listbox');

			if (operatorsCount === 0) {
				listbox.append(
					$('<div/>').text('This is where you manage your list of operators. People added here will be able to login at <TODO> and accept chats from your customers.')
				).append(
					$('<div/>').addClass('main_operators_listbox_centertext').text('Currently, there are no operators for your site.')
				);
			}

			// remove all classes from the listbox
			listbox.removeClass('main_operators_listbox');
			listbox.removeClass('main_operators_listbox_large');

			// set either the listbox or listbox_large class depending on the number of operators we're listing
			listbox.addClass(operatorsCount <= 3 ? 'main_operators_listbox' : 'main_operators_listbox_large');

			break;
		case Messages.AdminOperatorCreateSuccessMessage:
			changeSubtabTo(operatorsSubtab);
			break;
		case Messages.AdminOperatorReplaceSuccessMessage:
		case Messages.AdminOperatorDeleteSuccessMessage:
			changeSubtabTo(operatorsSubtab);
			break;
		case Messages.AdminOperatorCreateDuplicateUsernameMessage:
		case Messages.AdminOperatorReplaceDuplicateUsernameMessage:
			showEditOperatorDuplicateUsernameScreen();
			break;
		case Messages.AdminOperatorReplaceInvalidIdMessage:
		case Messages.AdminOperatorDeleteFailedMessage:
			// these are cases when something very unexpected happens which can likely be fixed with a refresh
			window.location.reload();
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

function showEditOperatorDuplicateUsernameScreen() {
	showMiscMessageTab('Duplicate username',
		$('<div/>').addClass('miscmessage_content_textwrapper').append(
			$('<div/>').text('You\'ve already used that username for another operator.')
		),
		$('<div/>').addClass('fixedtable').addClass('miscmessage_buttontable').append(
			$('<div/>').addClass('tablerow').append(
				$('<div/>').addClass('cell')
			).append(
				$('<div/>').addClass('cell').css('width', '80px').append(
					$('<div/>').addClass('basicbutton').text('Back').click(function() {
						changeTabTo(mainTab);
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

