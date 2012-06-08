var accountCreated = false;

$(window).bind('load', function() {
	watchVideoTab = $('#watchvideo_tab');
	learnMoreTab = $('#learnmore_tab');
	tryItOutTab = $('#tryitout_tab');
	newAccountTab = $('#newaccount_tab');

	$('#menubutton_watchvideo').click(function() {
		changeTabTo(watchVideoTab);
	});

	$('#menubutton_learnmore').click(function() {
		changeTabTo(learnMoreTab);
	});

	function tryItOutHandler() {
		if (!accountCreated) {
			changeTabTo(tryItOutTab);
		} else {
			changeTabTo(newAccountTab);
		}
	}

	$('#menubutton_tryitout').click(tryItOutHandler);
	$('#learnmore_createaccount1').click(tryItOutHandler);
	$('#learnmore_createaccount2').click(tryItOutHandler);

	// pricing tooltip
	var tooltip = $('<div/>').attr('id', 'tooltip');
	$('body').append(tooltip);

	tooltip.hide();

	function addTooltipTo(element, content, width) {
		width = typeof width !== 'undefined' ? width : 250;

		element.mouseenter(function() {
			tooltip.empty();
			tooltip.css('width', width);
			tooltip.append(content);
			tooltip.show();
		}).mousemove(function(e) {
			var maxRight = $(window).width() - 9;
			var right = e.pageX + tooltip.outerWidth() / 2;
			if (right > maxRight) {
				right = maxRight;
			}
			var newLeft = right - tooltip.outerWidth();
			if (newLeft < 10) {
				newLeft = 10;
			}
			tooltip.css({
				left: newLeft,
				top: e.pageY + 10
			});
		}).mouseleave(function() {
			tooltip.hide();
		});
	}

	/*
	 * processReferrer('http://www.google.ca/?q=furry+pink+leather+handbag', 2);
	 * writeCustomerOnPageMessage(2, 'http://www.pricelesshandbags.ca/item/27801', 'currently');
	 */

	addTooltipTo($('#pricing_operatorallowancelabel'), 'Each plan allows adding up to this many operators. Operators are people who can answer chats from your customers.');
	addTooltipTo($('#pricing_customermonitoringlabel'),
		$('<div/>').append(
			$('<div/>').text('The LilyLiveChat customer monitoring system keeps your operators informed on the actions of the customer as they are browsing your website.')
		).append(
			$('<img/>').attr('src', '/images/customermonitoring.png').attr('alt', '')
		),
		709);
	addTooltipTo($('#pricing_customeroriginlabel'), 'Want to know where your customers are coming from? Now you can! See the search keywords they use or the site that linked them to you and use this knowledge to your advantage!', 340);
	addTooltipTo($('#pricing_tlssecuritylabel'), 'We hate to say it, but there are a few bad apples on the Internet. LilyLiveChat protects its customers with the latest and greatest TLS encryption (the successor to SSL) to ensure that your conversations are completely private.', 400);
	addTooltipTo($('#pricing_assistedinstallationlabel'), 'No matter how technical you are, our developers will gladly walk you through setting up LilyLiveChat to integrate smoothly with your website. No one gets left behind!', 250);
	addTooltipTo($('#pricing_exceptionalsupportlabel'), 'You can travel the world, meet all kinds of great people, but you\'ll have a hard time finding a live chat service that lets you talk to the developers who know LilyLiveChat as well as they know their own keyboard!', 390);

	$('#tryitout_btn_newaccount').click(function() {
		$.ajax({
			url: '/cmd/createsite',
			dataType: 'json',
			data: '',
			timeout: 5000,
			success: function(data, textStatus, jqXHR) {
				var adminPanelUrl = 'http://lilylivechat.net/admin/' + data.siteId;
				$('#newaccount_adminpanel').empty().append(
					$('<a/>').attr('href', adminPanelUrl).attr('target', '_blank').text(adminPanelUrl)
				);

				accountCreated = true;
			},
			error: function(jqXHR, textStatus, errorThrown) {
				$('#newaccount_tab').empty().append('We can\'t create your account right now. If you get this message after an hour, contact us and we\'ll see if we can help!');
			}
		});

		changeTabTo(newAccountTab);
	});

	$(window).resize(onResize);

	onResize();

	// fade in the main tab
	$('#main_tab').fadeTo(400, 1);
	// enable top spacer height transition
	$('#main_top').addClass('main_top_heighttrans');

	//changeTabTo(watchVideoTab);
	changeTabTo(learnMoreTab);
});

function onResize() {
	onBasicVCenterResizeMinPadding('main', 9);
}

function onBasicVCenterResizeMinPadding(tabName, minPaddingHeight) {
	// disable scrolling as it causes visual glitches
	$('body').css('overflow-y', 'hidden');

	var middleHeight = $('#' + tabName + '_middle').outerHeight();
	var topAndBottomHeight = ($(window).height() - middleHeight) / 2; // can be negative
	if (topAndBottomHeight < minPaddingHeight) {
		topAndBottomHeight = minPaddingHeight;
		// if the scrollbars are needed, enable them
		$('body').css('overflow-y', 'auto');
	}
	$('#' + tabName + '_top').css('height', topAndBottomHeight);
	$('#' + tabName + '_bottom').css('height', topAndBottomHeight);
	$('#' + tabName + '_tab').css('height', middleHeight + topAndBottomHeight * 2);
}

// tab switcher

var watchVideoTab;
var learnMoreTab;
var tryItOutTab;

var currentTab = null;
var currentTabTarget = undefined;

function changeTabTo(tab, onTabLoadCallback) {
	var alreadyBusy = (currentTabTarget !== undefined);

	currentTabTarget = tab;

	if (!alreadyBusy) {
		if (currentTab) {
			currentTab.fadeOut(150, 0, function() {
				onOldTabGone();
			});
		} else {
			onOldTabGone();
		}
	}

	function onOldTabGone() {
		currentTab = tab;
		if (currentTab === currentTabTarget) {
			currentTabTarget = undefined;
			currentTab.fadeTo(0, 0, function() {
				onResize();
				if (onTabLoadCallback) {
					onTabLoadCallback();
				}
				currentTab.fadeTo(300, 1);
			});
		} else {
			// here we reset currentTabTarget to fail the alreadyBusy check
			var target = currentTabTarget;
			currentTabTarget = undefined;
			changeTabTo(target);
		}
	}
}
