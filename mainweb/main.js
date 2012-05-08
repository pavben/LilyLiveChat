$(window).bind('load', function() {
	watchVideoTab = $('#watchvideo_tab');
	learnMoreTab = $('#learnmore_tab');
	tryItOutTab = $('#tryitout_tab');

	$('#menubutton_watchvideo').click(function() {
		changeTabTo(watchVideoTab);
	});

	$('#menubutton_learnmore').click(function() {
		changeTabTo(learnMoreTab);
	});

	$('#menubutton_tryitout').click(function() {
		changeTabTo(tryItOutTab);
	});

	$(window).resize(onResize);

	onResize();

	// fade in the main tab
	$('#main_tab').fadeTo(400, 1);
	// enable top spacer height transition
	$('#main_top').addClass('main_top_heighttrans');

	changeTabTo(watchVideoTab);
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
