sap.ui.define([
    "sap/ui/base/Object",
    "sap/m/MessageStrip",
    "sap/m/MessageToast",
    "sap/m/Button",
    "sap/m/Text",
    "sap/m/HBox",
    "sap/ui/core/library",
    "sap/ui/model/json/JSONModel",
    "sap/base/Log"
], function(BaseObject, MessageStrip, MessageToast, Button, Text, HBox, coreLibrary, JSONModel, Log) {
    "use strict";

    var MessageType = coreLibrary.MessageType;

    return BaseObject.extend("com.sap.notifications.banner2.controller.NotificationBanner", {

        constructor: function() {
            BaseObject.apply(this, arguments);
            this._notifications = [];
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
         * Process received notifications
         * @private
         * @param {Array} notifications - array of notifications
         */
        _processNotifications: function(notifications) {
            var hasNewNotifications = false;

            // Check for new or updated notifications
            if (notifications.length !== this._notifications.length) {
                hasNewNotifications = true;
            } else {
                for (var i = 0; i < notifications.length; i++) {
                    var found = this._notifications.find(function(n) {
                        return n.message_id === notifications[i].message_id;
                    });
                    if (!found || found.changed_at !== notifications[i].changed_at) {
                        hasNewNotifications = true;
                        break;
                    }
                }
            }

            if (hasNewNotifications) {
                this._notifications = notifications;
                this._displayNotifications();
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

            for (var i = 0; i < this._notifications.length; i++) {
                var notification = this._notifications[i];
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

            // Display BANNER notifications
            if (bannerNotifications.length > 0) {
                this._notifications = bannerNotifications;
                this._currentBannerIndex = 0;
                this._showBanner();
            }

            // Display TOAST notifications
            for (var j = 0; j < toastNotifications.length; j++) {
                this._showToast(toastNotifications[j]);
            }

            // Display BOTH notifications (banner + toast)
            if (bothNotifications.length > 0) {
                // Add to banner display
                this._notifications = this._notifications.concat(bothNotifications);
                if (bannerNotifications.length === 0) {
                    this._currentBannerIndex = 0;
                    this._showBanner();
                }
                // Also show as toast
                for (var k = 0; k < bothNotifications.length; k++) {
                    this._showToast(bothNotifications[k]);
                }
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
            var message = notification.title + ": " + notification.message_text;

            MessageToast.show(message, {
                duration: 5000, // 5 seconds
                width: "25em",
                my: "center bottom",
                at: "center bottom",
                of: window,
                offset: "0 -50",
                autoClose: true
            });
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

            // Show new banner if notifications exist
            if (this._notifications.length > 0) {
                this._showBanner();
            }
        },

        /**
         * Show notification banner
         * @private
         */
        _showBanner: function() {
            console.log("[NotificationBanner] _showBanner called. Notifications count:", this._notifications.length);

            if (this._notifications.length === 0) {
                console.log("[NotificationBanner] No notifications to show - exiting");
                return;
            }

            var notification = this._notifications[this._currentBannerIndex];
            var messageType = this._getMessageType(notification.severity);

            console.log("[NotificationBanner] Creating banner for notification:", notification);
            console.log("[NotificationBanner] Title:", notification.title);
            console.log("[NotificationBanner] Message text:", notification.message_text);
            console.log("[NotificationBanner] Message type:", messageType);

            var bannerText = notification.title;
            if (notification.message_text) {
                bannerText += ": " + notification.message_text;
            }

            // Add navigation counter if multiple notifications
            if (this._notifications.length > 1) {
                bannerText += " (" + (this._currentBannerIndex + 1) + " of " + this._notifications.length + ")";
            }

            console.log("[NotificationBanner] Final banner text:", bannerText);

            // Create banner without styleClass (not supported in constructor)
            var messageStrip = new MessageStrip({
                text: bannerText,
                type: messageType,
                showIcon: true,
                showCloseButton: true,
                close: this._onBannerClose.bind(this)
            });

            // Add CSS classes after creation
            messageStrip.addStyleClass("sapUiMediumMargin");
            messageStrip.addStyleClass("notificationBanner");
            messageStrip.addStyleClass("notificationBanner--" + notification.severity.toLowerCase());

            // If multiple notifications, wrap in HBox with navigation buttons
            if (this._notifications.length > 1) {
                var prevButton = new Button({
                    icon: "sap-icon://navigation-left-arrow",
                    type: "Transparent",
                    press: this._showPreviousNotification.bind(this),
                    tooltip: "Previous notification"
                });
                prevButton.addStyleClass("sapUiTinyMarginEnd");

                var nextButton = new Button({
                    icon: "sap-icon://navigation-right-arrow",
                    type: "Transparent",
                    press: this._showNextNotification.bind(this),
                    tooltip: "Next notification"
                });
                nextButton.addStyleClass("sapUiTinyMarginBegin");

                this._currentBanner = new HBox({
                    alignItems: "Center",
                    items: [
                        prevButton,
                        messageStrip,
                        nextButton
                    ]
                });
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
        _onBannerClose: function() {
            // Remove current notification from display
            this._notifications.splice(this._currentBannerIndex, 1);

            // Adjust index if needed
            if (this._currentBannerIndex >= this._notifications.length) {
                this._currentBannerIndex = 0;
            }

            // Update display
            this._updateBanner();
        },

        /**
         * Show previous notification
         * @private
         */
        _showPreviousNotification: function() {
            if (this._notifications.length <= 1) {
                return;
            }

            this._currentBannerIndex--;
            if (this._currentBannerIndex < 0) {
                this._currentBannerIndex = this._notifications.length - 1;
            }

            this._updateBanner();
        },

        /**
         * Show next notification
         * @private
         */
        _showNextNotification: function() {
            if (this._notifications.length <= 1) {
                return;
            }

            this._currentBannerIndex++;
            if (this._currentBannerIndex >= this._notifications.length) {
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