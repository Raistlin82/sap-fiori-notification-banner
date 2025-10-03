sap.ui.define([
    "sap/ui/base/Object",
    "sap/m/MessageStrip",
    "sap/m/MessageToast",
    "sap/m/Button",
    "sap/m/Text",
    "sap/m/Title",
    "sap/m/HBox",
    "sap/m/VBox",
    "sap/m/FlexBox",
    "sap/ui/core/library",
    "sap/ui/model/json/JSONModel",
    "sap/base/Log"
], function(BaseObject, MessageStrip, MessageToast, Button, Text, Title, HBox, VBox, FlexBox, coreLibrary, JSONModel, Log) {
    "use strict";

    var MessageType = coreLibrary.MessageType;

    return BaseObject.extend("com.sap.notifications.banner2.controller.NotificationBanner", {

        constructor: function() {
            BaseObject.apply(this, arguments);
            this._allNotifications = [];         // All active notifications
            this._bannerNotifications = [];      // Only banner/both notifications
            this._bannerContainer = null;
            this._currentBannerIndex = 0;
            this._currentBanner = null;
            this._model = new JSONModel();
            this._isAttachedToShell = false;

            // Error handling & resilience properties
            this._retryCount = 0;
            this._maxRetries = 3;
            this._retryDelay = 1000; // Start with 1 second
            this._consecutiveErrors = 0;
            this._maxConsecutiveErrors = 5;
            this._isCircuitOpen = false;
            this._circuitResetTimeout = null;

            // Dismissed notifications (1 hour timeout)
            this._dismissedTimeout = 60 * 60 * 1000; // 1 hour in milliseconds
        },

        /**
         * Load notifications from backend with retry and circuit breaker
         * @public
         */
        loadNotifications: function() {
            var that = this;

            console.log("[NotificationBanner] Loading notifications...");

            // Check circuit breaker state
            if (this._isCircuitOpen) {
                console.warn("[NotificationBanner] Circuit breaker is open, skipping notification load");
                Log.warning("Circuit breaker is open, skipping notification load");
                return;
            }

            // Get user ID safely (works in both FLP and standalone modes)
            var sUserId = "ANONYMOUS";
            try {
                if (typeof sap !== "undefined" && sap.ushell && sap.ushell.Container) {
                    sUserId = sap.ushell.Container.getUser().getId();
                }
            } catch (e) {
                Log.warning("Could not retrieve user ID from FLP, using ANONYMOUS: " + e.message);
            }

            console.log("[NotificationBanner] User ID:", sUserId);

            jQuery.ajax({
                url: "/sap/bc/rest/zcl_notif_rest/",
                type: "GET",
                data: {
                    user_id: sUserId
                },
                timeout: 10000, // 10 second timeout
                success: function(data) {
                    console.log("[NotificationBanner] Notifications loaded successfully:", data);

                    // Reset error counters on success
                    that._retryCount = 0;
                    that._consecutiveErrors = 0;
                    that._retryDelay = 1000;

                    that._processNotifications(data);
                },
                error: function(xhr, status, error) {
                    console.error("[NotificationBanner] Failed to load notifications:", error, "Status:", xhr.status);
                    that._handleLoadError(xhr, status, error);
                }
            });
        },

        /**
         * Handle notification load error with retry logic
         * @private
         * @param {object} xhr - XMLHttpRequest object
         * @param {string} status - Status text
         * @param {string} error - Error message
         */
        _handleLoadError: function(xhr, status, error) {
            var that = this;
            this._consecutiveErrors++;

            Log.error("Failed to load notifications (attempt " + (this._retryCount + 1) + "): " + error +
                     " (Status: " + xhr.status + ", Consecutive errors: " + this._consecutiveErrors + ")");

            // Check if circuit breaker should open
            if (this._consecutiveErrors >= this._maxConsecutiveErrors) {
                this._openCircuitBreaker();
                return;
            }

            // Retry with exponential backoff
            if (this._retryCount < this._maxRetries) {
                this._retryCount++;
                var delay = this._retryDelay * Math.pow(2, this._retryCount - 1); // Exponential backoff

                Log.info("Retrying notification load in " + delay + "ms (retry " + this._retryCount + "/" + this._maxRetries + ")");

                setTimeout(function() {
                    that.loadNotifications();
                }, delay);
            } else {
                // Max retries reached, show error banner
                this._retryCount = 0;
                this._showErrorBanner(error, xhr.status);
            }
        },

        /**
         * Open circuit breaker to prevent cascade failures
         * @private
         */
        _openCircuitBreaker: function() {
            var that = this;
            this._isCircuitOpen = true;
            this._retryCount = 0;

            Log.warning("Circuit breaker opened after " + this._consecutiveErrors + " consecutive errors. " +
                       "Will attempt to reset in 60 seconds.");

            this._showErrorBanner("Service temporarily unavailable", 503);

            // Reset circuit breaker after 60 seconds
            if (this._circuitResetTimeout) {
                clearTimeout(this._circuitResetTimeout);
            }

            this._circuitResetTimeout = setTimeout(function() {
                that._resetCircuitBreaker();
            }, 60000);
        },

        /**
         * Reset circuit breaker
         * @private
         */
        _resetCircuitBreaker: function() {
            this._isCircuitOpen = false;
            this._consecutiveErrors = 0;
            this._circuitResetTimeout = null;

            Log.info("Circuit breaker reset, resuming normal operation");

            // Remove error banner and try loading again
            this._removeBanner();
            this.loadNotifications();
        },

        /**
         * Show error banner to user
         * @private
         * @param {string} error - Error message
         * @param {number} status - HTTP status code
         */
        _showErrorBanner: function(error, status) {
            if (!this._isAttachedToShell) {
                return;
            }

            // Remove existing banner
            this._removeBanner();

            var errorMessage = "Unable to load system notifications";
            if (status === 503) {
                errorMessage = "Notification service is temporarily unavailable. Please try again later.";
            } else if (status === 401 || status === 403) {
                errorMessage = "You don't have permission to view system notifications";
            } else if (status === 0) {
                errorMessage = "Cannot connect to notification service. Check your network connection.";
            }

            // Create error banner
            this._currentBanner = new MessageStrip({
                text: errorMessage,
                type: MessageType.Error,
                showIcon: true,
                showCloseButton: true,
                class: "sapUiMediumMargin notificationBanner notificationBanner--error",
                close: this._onErrorBannerClose.bind(this)
            });

            // Insert banner
            this._insertBannerInShell();
        },

        /**
         * Handle error banner close
         * @private
         */
        _onErrorBannerClose: function() {
            this._removeBanner();
            // Reset error state and try again
            this._consecutiveErrors = 0;
            this._retryCount = 0;
        },

        /**
         * Get dismissed notifications from localStorage
         * @private
         * @returns {Object} Map of message_id -> timestamp
         */
        _getDismissedNotifications: function() {
            try {
                var dismissed = localStorage.getItem("notificationBanner.dismissed");
                return dismissed ? JSON.parse(dismissed) : {};
            } catch (e) {
                Log.warning("Failed to read dismissed notifications from localStorage: " + e.message);
                return {};
            }
        },

        /**
         * Save dismissed notification to localStorage
         * @private
         * @param {string} messageId - Notification message ID
         */
        _dismissNotification: function(messageId) {
            try {
                var dismissed = this._getDismissedNotifications();
                dismissed[messageId] = Date.now();
                localStorage.setItem("notificationBanner.dismissed", JSON.stringify(dismissed));
                Log.info("Notification dismissed: " + messageId);
            } catch (e) {
                Log.warning("Failed to save dismissed notification to localStorage: " + e.message);
            }
        },

        /**
         * Clean up expired dismissed notifications
         * @private
         */
        _cleanupDismissed: function() {
            try {
                var dismissed = this._getDismissedNotifications();
                var now = Date.now();
                var cleaned = {};
                var expiredCount = 0;

                for (var messageId in dismissed) {
                    if (now - dismissed[messageId] < this._dismissedTimeout) {
                        cleaned[messageId] = dismissed[messageId];
                    } else {
                        expiredCount++;
                    }
                }

                if (expiredCount > 0) {
                    localStorage.setItem("notificationBanner.dismissed", JSON.stringify(cleaned));
                    Log.info("Cleaned up " + expiredCount + " expired dismissed notifications");
                }
            } catch (e) {
                Log.warning("Failed to cleanup dismissed notifications: " + e.message);
            }
        },

        /**
         * Process received notifications
         * @private
         * @param {Array} notifications - array of notifications
         */
        _processNotifications: function(notifications) {
            // Clean up expired dismissed notifications
            this._cleanupDismissed();

            // Filter out dismissed notifications
            var dismissed = this._getDismissedNotifications();
            var now = Date.now();
            var filteredNotifications = notifications.filter(function(notification) {
                var dismissedTime = dismissed[notification.message_id];
                if (dismissedTime && (now - dismissedTime) < this._dismissedTimeout) {
                    console.log("[NotificationBanner] Skipping dismissed notification:", notification.message_id, notification.title);
                    return false; // Skip dismissed notification
                }
                return true;
            }.bind(this));

            console.log("[NotificationBanner] Filtered notifications:", filteredNotifications.length, "of", notifications.length);

            var hasNewNotifications = false;

            // Check for new or updated notifications
            if (filteredNotifications.length !== this._allNotifications.length) {
                hasNewNotifications = true;
            } else {
                for (var i = 0; i < filteredNotifications.length; i++) {
                    var found = this._allNotifications.find(function(n) {
                        return n.message_id === filteredNotifications[i].message_id;
                    });
                    if (!found || found.changed_at !== filteredNotifications[i].changed_at) {
                        hasNewNotifications = true;
                        break;
                    }
                }
            }

            if (hasNewNotifications) {
                console.log("[NotificationBanner] New notifications detected, updating display");
                this._allNotifications = filteredNotifications;
                this._displayNotifications();
            } else {
                console.log("[NotificationBanner] No new notifications");
            }
        },

        /**
         * Display notifications based on their display_mode
         * @private
         */
        _displayNotifications: function() {
            console.log("[NotificationBanner] _displayNotifications called. isAttachedToShell:", this._isAttachedToShell);

            if (!this._isAttachedToShell) {
                console.warn("[NotificationBanner] Not attached to shell - cannot display notifications");
                return;
            }

            // Remove existing banner first
            this._removeBanner();

            // Group notifications by display mode
            var bannerNotifications = [];
            var toastNotifications = [];
            var bothNotifications = [];
            var silentNotifications = [];

            for (var i = 0; i < this._allNotifications.length; i++) {
                var notification = this._allNotifications[i];
                var displayMode = (notification.display_mode || "BANNER").toUpperCase();

                console.log("[NotificationBanner] Notification", i, "- Mode:", displayMode, "Title:", notification.title);

                switch(displayMode) {
                case "BANNER":
                    bannerNotifications.push(notification);
                    break;
                case "TOAST":
                    toastNotifications.push(notification);
                    break;
                case "BOTH":
                    bothNotifications.push(notification);
                    break;
                case "SILENT":
                    silentNotifications.push(notification);
                    break;
                default:
                    bannerNotifications.push(notification); // Default to banner
                }
            }

            console.log("[NotificationBanner] Grouped notifications - BANNER:", bannerNotifications.length,
                       "TOAST:", toastNotifications.length, "BOTH:", bothNotifications.length, "SILENT:", silentNotifications.length);

            // Store banner notifications for navigation (BANNER + BOTH)
            this._bannerNotifications = bannerNotifications.concat(bothNotifications);

            // Display BANNER notifications
            if (this._bannerNotifications.length > 0) {
                this._currentBannerIndex = 0;
                this._showBanner();
            }

            // Display TOAST notifications (TOAST only, not BOTH - those get toasted separately below)
            for (var j = 0; j < toastNotifications.length; j++) {
                this._showToast(toastNotifications[j]);
            }

            // Display BOTH notifications as toast (banner already shown above)
            for (var k = 0; k < bothNotifications.length; k++) {
                this._showToast(bothNotifications[k]);
            }

            // Log SILENT notifications
            for (var l = 0; l < silentNotifications.length; l++) {
                this._logNotification(silentNotifications[l]);
            }
        },

        /**
         * Show notification as toast
         * @private
         * @param {object} notification - Notification object
         */
        _showToast: function(notification) {
            // Check if already shown in this session to prevent duplicates
            if (!this._shownToasts) {
                this._shownToasts = {};
            }

            // Skip if already shown in current session
            if (this._shownToasts[notification.message_id]) {
                console.log("[NotificationBanner] Toast already shown in this session:", notification.message_id);
                return;
            }

            // Add severity prefix to differentiate visually (MessageToast doesn't support colors)
            var severityPrefix = "";
            switch (notification.severity.toUpperCase()) {
            case "HIGH":
                severityPrefix = "🔴 CRITICAL: ";
                break;
            case "MEDIUM":
                severityPrefix = "🟠 WARNING: ";
                break;
            case "LOW":
                severityPrefix = "🔵 INFO: ";
                break;
            }

            var message = severityPrefix + notification.title + "\n" + notification.message_text;

            MessageToast.show(message, {
                duration: 6000, // 6 seconds
                width: "35em",
                my: "center bottom",
                at: "center bottom",
                of: window,
                offset: "0 -50",
                autoClose: true
            });

            // Mark as shown in this session (not localStorage - let user see it on page refresh)
            this._shownToasts[notification.message_id] = true;
            console.log("[NotificationBanner] Toast shown:", notification.message_id, notification.title);
        },

        /**
         * Log silent notification (for audit purposes)
         * @private
         * @param {object} notification - Notification object
         */
        _logNotification: function(notification) {
            Log.info("SILENT NOTIFICATION: [" + notification.severity + "] " +
                    notification.title + ": " + notification.message_text,
            "notification_id: " + notification.message_id);
        },

        /**
         * Update banner display (used for navigation between banner notifications)
         * @private
         */
        _updateBanner: function() {
            if (!this._isAttachedToShell) {
                return;
            }

            // Remove existing banner
            this._removeBanner();

            // Show new banner if banner notifications exist
            if (this._bannerNotifications.length > 0) {
                this._showBanner();
            }
        },

        /**
         * Show notification banner
         * @private
         */
        _showBanner: function() {
            console.log("[NotificationBanner] _showBanner called. Banner notifications count:", this._bannerNotifications.length);

            if (this._bannerNotifications.length === 0) {
                console.log("[NotificationBanner] No banner notifications to show - exiting");
                return;
            }

            var notification = this._bannerNotifications[this._currentBannerIndex];
            var messageType = this._getMessageType(notification.severity);

            console.log("[NotificationBanner] Creating banner for notification:", notification);
            console.log("[NotificationBanner] Title:", notification.title);
            console.log("[NotificationBanner] Message text:", notification.message_text);
            console.log("[NotificationBanner] Message type:", messageType);

            // Build banner text with visual separators (MessageStrip text doesn't support HTML)
            var bannerText = "■ " + notification.title;
            if (notification.message_text) {
                bannerText += " — " + notification.message_text;
            }

            // Add navigation counter if multiple notifications
            if (this._bannerNotifications.length > 1) {
                bannerText += "  (" + (this._currentBannerIndex + 1) + " of " + this._bannerNotifications.length + ")";
            }

            console.log("[NotificationBanner] Final banner text:", bannerText);

            // Create banner
            var messageStrip = new MessageStrip({
                text: bannerText,
                type: messageType,
                showIcon: true,
                showCloseButton: true,
                close: this._onBannerClose.bind(this)
            });

            // Add CSS classes for styling
            messageStrip.addStyleClass("sapUiMediumMargin");
            messageStrip.addStyleClass("notificationBannerCustom");
            messageStrip.addStyleClass("notificationBanner--" + notification.severity.toLowerCase());

            // If multiple notifications, wrap in HBox with navigation buttons
            if (this._bannerNotifications.length > 1) {
                var prevButton = new Button({
                    icon: "sap-icon://navigation-left-arrow",
                    type: "Transparent",
                    press: this._showPreviousNotification.bind(this),
                    tooltip: "Previous notification"
                });

                var nextButton = new Button({
                    icon: "sap-icon://navigation-right-arrow",
                    type: "Transparent",
                    press: this._showNextNotification.bind(this),
                    tooltip: "Next notification"
                });

                // Set explicit width for MessageStrip to fill available space
                messageStrip.setWidth("100%");

                this._currentBanner = new HBox({
                    width: "100%",
                    alignItems: "Center",
                    justifyContent: "SpaceBetween",
                    items: [
                        prevButton,
                        messageStrip,
                        nextButton
                    ]
                });
                this._currentBanner.addStyleClass("sapUiMediumMargin");
            } else {
                this._currentBanner = messageStrip;
            }

            console.log("[NotificationBanner] Banner created, inserting into shell...");

            // Insert banner at the top of the page
            this._insertBannerInShell();
        },

        /**
         * Insert banner into Fiori Shell
         * @private
         */
        _insertBannerInShell: function() {
            var shellContainer = jQuery("#shell-header")[0] ||
                               jQuery(".sapUshellShellHeader")[0] ||
                               jQuery("body")[0];

            console.log("[NotificationBanner] Looking for shell container...");
            console.log("[NotificationBanner] Found container:", shellContainer ? shellContainer.tagName : "NONE");

            if (shellContainer) {
                var bannerContainer = jQuery("<div id='globalNotificationBanner'></div>");

                if (shellContainer.tagName === "BODY") {
                    console.log("[NotificationBanner] Prepending to BODY");
                    bannerContainer.prependTo(jQuery(shellContainer));
                } else {
                    console.log("[NotificationBanner] Inserting after shell header");
                    bannerContainer.insertAfter(jQuery(shellContainer));
                }

                this._currentBanner.placeAt("globalNotificationBanner");
                this._bannerContainer = bannerContainer[0];
                console.log("[NotificationBanner] ========== BANNER DISPLAYED ==========");
            } else {
                console.error("[NotificationBanner] No shell container found - cannot display banner!");
            }
        },

        /**
         * Remove current banner
         * @private
         */
        _removeBanner: function() {
            if (this._currentBanner) {
                this._currentBanner.destroy();
                this._currentBanner = null;
            }

            if (this._bannerContainer) {
                jQuery(this._bannerContainer).remove();
                this._bannerContainer = null;
            }
        },

        /**
         * Handle banner close
         * @private
         */
        _onBannerClose: function(oEvent) {
            console.log("[NotificationBanner] ========== BANNER CLOSE CLICKED ==========");
            console.log("[NotificationBanner] Event:", oEvent);
            console.log("[NotificationBanner] Current banner index:", this._currentBannerIndex);
            console.log("[NotificationBanner] Banner notifications count:", this._bannerNotifications.length);

            // Prevent duplicate close events
            if (this._isClosing) {
                console.log("[NotificationBanner] Already closing, ignoring duplicate event");
                return;
            }
            this._isClosing = true;

            // Save dismissed notification
            var notification = this._bannerNotifications[this._currentBannerIndex];
            if (notification && notification.message_id) {
                console.log("[NotificationBanner] Dismissing notification:", notification.message_id, notification.title);
                this._dismissNotification(notification.message_id);

                // Verify it was saved
                var dismissed = this._getDismissedNotifications();
                console.log("[NotificationBanner] Dismissed saved in localStorage:", dismissed[notification.message_id] ? "YES" : "NO");
            }

            // Remove current notification from display
            this._bannerNotifications.splice(this._currentBannerIndex, 1);
            console.log("[NotificationBanner] Removed from bannerNotifications, new count:", this._bannerNotifications.length);

            // Also remove from all notifications
            if (notification) {
                var index = this._allNotifications.findIndex(function(n) {
                    return n.message_id === notification.message_id;
                });
                if (index !== -1) {
                    this._allNotifications.splice(index, 1);
                    console.log("[NotificationBanner] Removed from allNotifications, new count:", this._allNotifications.length);
                }
            }

            // Adjust index if needed
            if (this._currentBannerIndex >= this._bannerNotifications.length) {
                this._currentBannerIndex = 0;
            }

            // Update display
            console.log("[NotificationBanner] Calling _updateBanner...");
            this._updateBanner();

            // Reset closing flag after a short delay
            setTimeout(function() {
                this._isClosing = false;
            }.bind(this), 500);

            console.log("[NotificationBanner] ========== BANNER CLOSE COMPLETED ==========");
        },

        /**
         * Show previous notification
         * @private
         */
        _showPreviousNotification: function() {
            if (this._bannerNotifications.length <= 1) {
                return;
            }

            this._currentBannerIndex--;
            if (this._currentBannerIndex < 0) {
                this._currentBannerIndex = this._bannerNotifications.length - 1;
            }

            this._updateBanner();
        },

        /**
         * Show next notification
         * @private
         */
        _showNextNotification: function() {
            if (this._bannerNotifications.length <= 1) {
                return;
            }

            this._currentBannerIndex++;
            if (this._currentBannerIndex >= this._bannerNotifications.length) {
                this._currentBannerIndex = 0;
            }

            this._updateBanner();
        },

        /**
         * Get UI5 message type from severity
         * @private
         * @param {string} severity - notification severity
         * @returns {sap.ui.core.MessageType} UI5 message type
         */
        _getMessageType: function(severity) {
            switch (severity.toUpperCase()) {
            case "HIGH":
                return MessageType.Error;
            case "MEDIUM":
                return MessageType.Warning;
            case "LOW":
                return MessageType.Information;
            default:
                return MessageType.Information;
            }
        },

        /**
         * Attach banner to Fiori Shell
         * @public
         */
        attachToShell: function() {
            console.log("[NotificationBanner] ========== ATTACHING TO SHELL ==========");
            this._isAttachedToShell = true;

            // Load and display notifications
            this.loadNotifications();
        },

        /**
         * Destroy banner and clean up
         * @public
         */
        destroy: function() {
            this._removeBanner();
            BaseObject.prototype.destroy.apply(this, arguments);
        }
    });
});