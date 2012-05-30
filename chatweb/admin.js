// these will be set onload
var loginTab = null;
var mainTab = null;
var miscMessageTab = null;

var generalSubtab = null;
var operatorsSubtab = null;
var editOperatorSubtab = null;
var installSubtab = null;
var installButtonSubtab = null;
var adminPasswordSubtab = null;

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
	installSubtab = $('#main_rightcell_install');
	installButtonSubtab = $('#main_rightcell_install_button');
	adminPasswordSubtab = $('#main_rightcell_adminpassword');

	replaceImageWith('/images/lock.png', $('#login_icon'));

	// login tab handlers

	// clicking the OK button
	$('#login_btn_ok').click(loginTabOkHandler);
	// or pressing Enter inside the password box
	$('#login_password').keypress(function(e) {
		if (e.which == 13) { // enter
			loginTabOkHandler();
		}
	});
	
	// main tab handlers
	replaceImageWith('/images/admin_general.png', $('#main_btn_general'));
	$('#main_btn_general').click(function() {
		changeSubtabTo(generalSubtab);
	});

	replaceImageWith('/images/admin_operators.png', $('#main_btn_operators'));
	$('#main_btn_operators').click(function() {
		changeSubtabTo(operatorsSubtab);
	});

	replaceImageWith('/images/admin_install.png', $('#main_btn_install'));
	$('#main_btn_install').click(function() {
		changeSubtabTo(installSubtab);
	});

	replaceImageWith('/images/admin_security.png', $('#main_btn_adminpassword'));
	$('#main_btn_adminpassword').click(function() {
		changeSubtabTo(adminPasswordSubtab);
	});

	// general subtab
	// site info
	(function() {
		var unsaved = false;

		function onSiteInfoChange() {
			if (!unsaved) {
				unsaved = true;

				$('#main_general_siteinfo_savedlabel').fadeTo(0, 0);
				$('#main_general_siteinfo_savediv').animate({height:'32px'}, 350);
			}
		}

		function siteInfoSaveHandler() {
			var siteName = $('#main_general_sitename');
			var adminEmail = $('#main_general_adminemail');

			// only send if currently not saved (this also prevents double-clicking the Save button)
			if (unsaved) {
				unsaved = false;
				queueAjaxCommand([Messages.CSMTAdminSetSiteInfoMessage, $.trim(siteName.val()), $.trim(adminEmail.val())]);

				$('#main_general_siteinfo_savedlabel').fadeTo(500, 1);
				$('#main_general_siteinfo_savediv').delay(1800).animate({height:'0px'}, 400);
			}
		}

		onChangeToFieldValue($('#main_general_sitename'), onSiteInfoChange);
		$('#main_general_sitename').keypress(function(e) {
			if (e.which == 13) { // enter
				siteInfoSaveHandler();
			}
		});

		onChangeToFieldValue($('#main_general_adminemail'), onSiteInfoChange);
		$('#main_general_adminemail').keypress(function(e) {
			if (e.which == 13) { // enter
				siteInfoSaveHandler();
			}
		});

		$('#main_general_siteinfo_btn_save').click(siteInfoSaveHandler);
	})();

	// operators subtab
	$('#main_operators_addnew').click(function() {
		addOrEditOperatorHandler(null);
	});

	// edit operator subtab
	$('#main_editoperator_btn_changecolor').click(function() {
		editOperatorSetCurrentColor(generatePersonColor());
	});

	$('#main_editoperator_btn_nexticon').click(function() {
		var nextIcon = stockIcons[0]; // default to the first
		var foundCurrentIcon = false;

		for (var i = 0; i < stockIcons.length; i++) {
			var thisIcon = stockIcons[i];

			if (foundCurrentIcon) {
				nextIcon = thisIcon;
				break;
			}
			else if (thisIcon === editOperatorCurrentIcon) {
				foundCurrentIcon = true;
			}
		}

		editOperatorCurrentIcon = nextIcon;

		replaceImageWith(editOperatorCurrentIcon, $('#main_editoperator_icon'));
	});

	onChangeToFieldValue($('#main_editoperator_name'), function() {
		onNameOrTitleEdited('name');
	});

	onChangeToFieldValue($('#main_editoperator_title'), function() {
		onNameOrTitleEdited('title');
	});

	// install subtab
	$('#install_next_link').click(function() {
		changeSubtabTo(installButtonSubtab);
	});

	$('#install_button_advanced_link').click(function() {
		$('#install_button_advanced').slideToggle(300, onResize);
	});

	updateInstallCodeMain(siteId);

	$('#install_button_chatbutton1').click(function() {
		$('#install_code_button').fadeTo(100, 0, function() {
			updateInstallCodeButton('1');
			$('#install_code_button').fadeTo(300, 1);
		})
	}).click();

	$('#install_button_chatbutton2').click(function() {
		$('#install_code_button').fadeTo(100, 0, function() {
			updateInstallCodeButton('2');
			$('#install_code_button').fadeTo(300, 1);
		})
	});

	// admin password subtab
	// admin password field
	{
		function onAdminPasswordChange(field) {
			if (!field[0].unsaved) {
				field[0].unsaved = true;

				$('#main_adminpassword_savedlabel').fadeTo(0, 0);
				$('#main_adminpassword_savediv').animate({height:'32px'}, 350);
			} else {
				// if unsaved, check if it was returned to empty
				if (field.val().length == 0) {
					field[0].unsaved = false;

					$('#main_adminpassword_savediv').animate({height:'0px'}, 400);
				}
			}
		}

		function adminPasswordSaveHandler() {
			var inputBox = $('#main_adminpassword_password');

			// only send if currently not saved (this also prevents double-clicking the Save button)
			if (inputBox[0].unsaved) {
				inputBox[0].unsaved = false;

				if (inputBox.val().length > 0) {
					// send the update to the server
					queueAjaxCommand([Messages.AdminSetAdminPasswordMessage, inputBox.val()]);
					// clear the value
					setFieldValue(inputBox, '');

					$('#main_adminpassword_savedlabel').fadeTo(500, 1);
					$('#main_adminpassword_savediv').delay(1800).animate({height:'0px'}, 400);
				}
			}
		}

		onChangeToFieldValue($('#main_adminpassword_password'), onAdminPasswordChange);
		setFieldValue($('#main_adminpassword_password'), ''); // empty

		$('#main_adminpassword_btn_save').click(adminPasswordSaveHandler)
		$('#main_adminpassword_password').keypress(function(e) {
			if (e.which == 13) { // enter
				adminPasswordSaveHandler();
			}
		});
	}

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
		editOperatorCurrentIcon = stockIcons[Math.floor(Math.random() * stockIcons.length)];
	}

	replaceImageWith(editOperatorCurrentIcon, $('#main_editoperator_icon'));

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

function onNameOrTitleEdited(nameOrTitleStr) {
	$('#main_editoperator_' + nameOrTitleStr + '_preview').text($('#main_editoperator_' + nameOrTitleStr).val());
}

// install subtab functions

function updateInstallCodeMain(siteId) {
	$('#install_code_main').val(
		[
'<script async="async" type="text/javascript" src="//lilylivechat.net/lilycode/' + siteId + '"></script>'
		].join('\n')
	);
}

function updateInstallCodeButton(buttonName) {
	$('#install_code_button').val(
		[
'<!-- BEGIN LilyLiveChat button code -->',
'<div class="lilylivechat_online" style="display:none;">',
'	<a href="http://www.lilylivechat.net" title="Live Chat" onclick="try { lilyLiveChat_launch(); } catch(e) { alert(\'LilyLiveChat main code not linked!\'); } finally { return false; }">',
'		<img src="//lilylivechat.net/chatbuttons/' + buttonName + '_on.png" alt="Live Chat" style="display:block; margin:0px auto 0px auto;" />',
'	</a>',
'</div>',
'<div class="lilylivechat_offline" style="display:none;">',
'</div>',
'<!-- END LilyLiveChat button code -->'
		].join('\n')
	);
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

var loginTabOkHandlerEnabled = true;

function loginTabOkHandler() {
	if (loginTabOkHandlerEnabled) {
		var password = $.trim($('#login_password').val());

		if (password.length == 0) {
			$('#login_password').focus();
		} else {
			// disable the login handler to prevent double-clicks
			loginTabOkHandlerEnabled = false;
			queueAjaxCommand([Messages.AdminLoginRequestMessage, password]);
		}
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
			var isActive = message[1]; // we don't care if it's active or not for admins

			// set the proper login box title
			$('#login_adminloginlabel').text(siteName + ' Admin Login');

			changeTabTo(loginTab, function () {
				// focus the username box
				$('#login_password').focus();
			});

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
			// TODO PL: Show a "Please update your bookmarks" screen instead of instantly refreshing through SL
			refreshThroughSiteLocator();
			break;
		case Messages.AdminLoginSuccessMessage:
			changeTabTo(mainTab);
			log("Login successful");
			changeSubtabTo(generalSubtab);
			break;
		case Messages.AdminLoginFailedMessage:
			showLoginFailedScreen();
			break;
		case Messages.AdminSiteInfoMessage:
			var siteId = message[0];
			var siteName = message[1];
			var adminEmail = message[2];

			$('#main_general_siteid').text(siteId);

			var operatorsLoginUrl = 'http://lilylivechat.net/operator/' + siteId;
			$('#main_operators_login_url').attr('href', operatorsLoginUrl).attr('target', '_blank').text(operatorsLoginUrl);

			setFieldValue($('#main_general_sitename'), siteName);
			setFieldValue($('#main_general_adminemail'), adminEmail);
			break;
		case Messages.CSMTAdminSetSiteInfoSuccessMessage:
			// site name set successfully
			break;
		case Messages.AdminOperatorDetailsStartMessage:
			$('#main_operators_notempty').hide();
			$('#main_operators_empty').hide();
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
							$('<div/>').addClass('cell').append(
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

			replaceImageWith(iconUrl, $('#main_operators_' + operatorId + '_icon'));

			operatorsCount++;

			break;
		case Messages.AdminOperatorDetailsEndMessage:
			if (operatorsCount > 0) {
				$('#main_operators_notempty').show();
			} else {
				$('#main_operators_empty').show();
			}

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
		case Messages.AdminSetAdminPasswordSuccessMessage:
			// these are success messages for which success was already assumed, so nothing needs to be done
			break;
		case Messages.AdminOperatorReplaceInvalidIdMessage: // the operator being replaced was deleted
		case Messages.AdminOperatorDeleteFailedMessage: // the operator being deleted is already deleted
			// these are cases when something very unexpected happens which can likely be fixed with a refresh
			refreshThroughSiteLocator();
			break;
		default:
			log("Received an unknown message!");
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

function showLoginFailedScreen() {
	showMiscMessageTab('No match...',
		$('<div/>').addClass('miscmessage_content_textwrapper').append(
			$('<div/>').text('Try again. If you can\'t remember your password, contact us at ').append(
				$('<a/>').attr('href', 'mailto:info@virtivia.com').text('info@virtivia.com')
			).append(
				' or create a new account, whichever is easier.'
			)
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
		onBasicVCenterResizeMinPadding('main', 9);
	} else if (currentTab == miscMessageTab) {
		onBasicVCenterResize('miscmessage', 530);
	}
}

