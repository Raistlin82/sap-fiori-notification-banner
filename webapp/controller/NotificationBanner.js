sap.ui.define([
    "sap/ui/base/Object",
    "sap/m/MessageStrip",
    "sap/m/MessageToast",
    "sap/m/MessageBox",
    "sap/m/Button",
    "sap/m/Text",
    "sap/m/Title",
    "sap/m/HBox",
    "sap/m/VBox",
    "sap/m/FlexBox",
    "sap/m/FlexItemData",
    "sap/ui/core/library",
    "sap/ui/model/json/JSONModel",
    "sap/base/Log",
    "com/sap/notifications/banner2/util/CsrfAjax"
], function(BaseObject, MessageStrip, MessageToast, MessageBox, Button, Text, Title, HBox, VBox, FlexBox, FlexItemData, coreLibrary, JSONModel, Log, CsrfAjax) {
    "use strict";

    var MessageType = coreLibrary.MessageType;
    var LOG_COMPONENT = "com.sap.notifications.banner2.NotificationBanner";

    /**
     * Debug trace via sap/base/Log (visible with sap-ui-debug / log level DEBUG)
     * instead of console output in production
     * @private
     */
    function _debug() {
        if (!Log.isLoggable(Log.Level.DEBUG, LOG_COMPONENT)) {
            return;
        }
        var sMessage = Array.prototype.map.call(arguments, function(vValue) {
            if (vValue !== null && typeof vValue === "object") {
                try {
                    return JSON.stringify(vValue);
                } catch (e) {
                    return String(vValue);
                }
            }
            return String(vValue);
        }).join(" ");
        Log.debug(sMessage, "", LOG_COMPONENT);
    }

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

            _debug("[NotificationBanner] Loading notifications...");

            // Check circuit breaker state
            if (this._isCircuitOpen) {
                Log.warning("Circuit breaker is open, skipping notification load", "", LOG_COMPONENT);
                return;
            }

            // No user parameter: the backend always uses the logged-on user (sy-uname)
            jQuery.ajax({
                url: "/sap/bc/rest/zcl_notif_rest/",
                type: "GET",
                timeout: 10000, // 10 second timeout
                success: function(data) {
                    _debug("[NotificationBanner] Notifications loaded successfully:", data);

                    // Reset error counters on success
                    that._retryCount = 0;
                    that._consecutiveErrors = 0;
                    that._retryDelay = 1000;

                    that._processNotifications(data);
                },
                error: function(xhr, status, error) {
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
                close: this._onErrorBannerClose.bind(this)
            });
            this._currentBanner.addStyleClass("sapUiMediumMargin notificationBanner notificationBanner--error");

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
            // Results received before the banner is attached to the shell are
            // discarded: storing them would make the first load after
            // attachToShell() look "unchanged" and nothing would be displayed
            if (!this._isAttachedToShell) {
                _debug("[NotificationBanner] Not attached to shell yet - ignoring result");
                return;
            }

            // Backend returns an array; tolerate a {notifications: [...]} wrapper
            if (!Array.isArray(notifications)) {
                notifications = (notifications && notifications.notifications) || [];
            }

            // Clean up expired dismissed notifications
            this._cleanupDismissed();

            // Filter out dismissed notifications
            var dismissed = this._getDismissedNotifications();
            var now = Date.now();
            var filteredNotifications = notifications.filter(function(notification) {
                var dismissedTime = dismissed[notification.message_id];
                if (dismissedTime && (now - dismissedTime) < this._dismissedTimeout) {
                    _debug("[NotificationBanner] Skipping dismissed notification:", notification.message_id, notification.title);
                    return false; // Skip dismissed notification
                }
                return true;
            }.bind(this));

            _debug("[NotificationBanner] Filtered notifications:", filteredNotifications.length, "of", notifications.length);

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
                _debug("[NotificationBanner] New notifications detected, updating display");
                this._allNotifications = filteredNotifications;
                this._displayNotifications();
            } else {
                _debug("[NotificationBanner] No new notifications");
            }
        },

        /**
         * Display notifications based on their display_mode
         * @private
         */
        _displayNotifications: function() {
            _debug("[NotificationBanner] _displayNotifications called. isAttachedToShell:", this._isAttachedToShell);

            if (!this._isAttachedToShell) {
                Log.warning("Not attached to shell - cannot display notifications", "", LOG_COMPONENT);
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

                _debug("[NotificationBanner] Notification", i, "- Mode:", displayMode, "Title:", notification.title);

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

            _debug("[NotificationBanner] Grouped notifications - BANNER:", bannerNotifications.length,
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
                _debug("[NotificationBanner] Toast already shown in this session:", notification.message_id);
                return;
            }

            // Add severity prefix to differentiate visually (MessageToast doesn't support colors)
            var severityPrefix = "";
            switch ((notification.severity || "").toUpperCase()) {
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
                // my/at default to "center bottom"
                of: window,
                offset: "0 -50",
                autoClose: true
            });

            // Mark as shown in this session (not localStorage - let user see it on page refresh)
            this._shownToasts[notification.message_id] = true;
            _debug("[NotificationBanner] Toast shown:", notification.message_id, notification.title);
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
            _debug("[NotificationBanner] _showBanner called. Banner notifications count:", this._bannerNotifications.length);

            if (this._bannerNotifications.length === 0) {
                _debug("[NotificationBanner] No banner notifications to show - exiting");
                return;
            }

            var notification = this._bannerNotifications[this._currentBannerIndex];
            var messageType = this._getMessageType(notification.severity);

            _debug("[NotificationBanner] Creating banner for notification:", notification);
            _debug("[NotificationBanner] Title:", notification.title);
            _debug("[NotificationBanner] Message text:", notification.message_text);
            _debug("[NotificationBanner] Message type:", messageType);

            // Build banner text with visual separators (MessageStrip text doesn't support HTML)
            var bannerText = "■ " + notification.title;
            if (notification.message_text) {
                bannerText += " — " + notification.message_text;
            }

            // Add navigation counter if multiple notifications
            if (this._bannerNotifications.length > 1) {
                bannerText += "  (" + (this._currentBannerIndex + 1) + " of " + this._bannerNotifications.length + ")";
            }

            _debug("[NotificationBanner] Final banner text:", bannerText);

            // Check if notification requires acknowledgment
            var requiresAck = notification.requires_ack === "X" || notification.requires_ack === true;
            _debug("[NotificationBanner] Requires acknowledgment:", requiresAck);

            // Create banner (X button only if no acknowledgment is required)
            var messageStrip = new MessageStrip({
                text: bannerText,
                type: messageType,
                showIcon: true,
                showCloseButton: !requiresAck
            });
            if (!requiresAck) {
                messageStrip.attachClose(this._onBannerClose, this);
            }

            // Add CSS classes for styling
            messageStrip.addStyleClass("notificationBannerCustom");
            messageStrip.addStyleClass("notificationBanner--" + (notification.severity || "low").toLowerCase());

            // MessageStrip has no aggregation for buttons and no width property:
            // OK and navigation buttons are placed next to it in an HBox and the
            // strip takes the remaining space via FlexItemData
            messageStrip.setLayoutData(new FlexItemData({ growFactor: 1, shrinkFactor: 1 }));

            var aItems = [messageStrip];

            // If acknowledgment is required, add OK button next to the banner
            if (requiresAck) {
                var that = this;
                aItems.push(new Button({
                    text: "OK",
                    type: "Emphasized",
                    icon: "sap-icon://accept",
                    press: function() {
                        that._acknowledgeNotification(notification);
                    }
                }).addStyleClass("sapUiTinyMarginBegin"));
                _debug("[NotificationBanner] Added OK acknowledgment button");
            }

            // If multiple notifications, add navigation buttons
            if (this._bannerNotifications.length > 1) {
                aItems.unshift(new Button({
                    icon: "sap-icon://navigation-left-arrow",
                    type: "Transparent",
                    press: this._showPreviousNotification.bind(this),
                    tooltip: "Previous notification"
                }));

                aItems.push(new Button({
                    icon: "sap-icon://navigation-right-arrow",
                    type: "Transparent",
                    press: this._showNextNotification.bind(this),
                    tooltip: "Next notification"
                }));
            }

            if (aItems.length > 1) {
                // No explicit width: the HBox fills the row, and width 100% plus
                // the margin class would overflow the viewport and cut off the buttons
                this._currentBanner = new HBox({
                    alignItems: "Center",
                    items: aItems
                });
            } else {
                this._currentBanner = messageStrip;
            }
            this._currentBanner.addStyleClass("sapUiMediumMargin");

            _debug("[NotificationBanner] Banner created, inserting into shell...");

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

            _debug("[NotificationBanner] Looking for shell container...");
            _debug("[NotificationBanner] Found container:", shellContainer ? shellContainer.tagName : "NONE");

            if (shellContainer) {
                var bannerContainer = jQuery("<div id='globalNotificationBanner'></div>");

                if (shellContainer.tagName === "BODY") {
                    _debug("[NotificationBanner] Prepending to BODY");
                    bannerContainer.prependTo(jQuery(shellContainer));
                } else {
                    _debug("[NotificationBanner] Inserting after shell header");
                    bannerContainer.insertAfter(jQuery(shellContainer));
                }

                this._currentBanner.placeAt("globalNotificationBanner");
                this._bannerContainer = bannerContainer[0];
                _debug("[NotificationBanner] ========== BANNER DISPLAYED ==========");
            } else {
                Log.error("No shell container found - cannot display banner", "", LOG_COMPONENT);
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
            _debug("[NotificationBanner] ========== BANNER CLOSE CLICKED ==========");
            _debug("[NotificationBanner] Event:", oEvent);
            _debug("[NotificationBanner] Current banner index:", this._currentBannerIndex);
            _debug("[NotificationBanner] Banner notifications count:", this._bannerNotifications.length);

            // Prevent duplicate close events
            if (this._isClosing) {
                _debug("[NotificationBanner] Already closing, ignoring duplicate event");
                return;
            }
            this._isClosing = true;

            // Save dismissed notification
            var notification = this._bannerNotifications[this._currentBannerIndex];
            if (notification && notification.message_id) {
                _debug("[NotificationBanner] Dismissing notification:", notification.message_id, notification.title);
                this._dismissNotification(notification.message_id);

                // Verify it was saved
                var dismissed = this._getDismissedNotifications();
                _debug("[NotificationBanner] Dismissed saved in localStorage:", dismissed[notification.message_id] ? "YES" : "NO");
            }

            // Remove current notification from display
            this._bannerNotifications.splice(this._currentBannerIndex, 1);
            _debug("[NotificationBanner] Removed from bannerNotifications, new count:", this._bannerNotifications.length);

            // Also remove from all notifications
            if (notification) {
                var index = this._allNotifications.findIndex(function(n) {
                    return n.message_id === notification.message_id;
                });
                if (index !== -1) {
                    this._allNotifications.splice(index, 1);
                    _debug("[NotificationBanner] Removed from allNotifications, new count:", this._allNotifications.length);
                }
            }

            // Adjust index if needed
            if (this._currentBannerIndex >= this._bannerNotifications.length) {
                this._currentBannerIndex = 0;
            }

            // Update display
            _debug("[NotificationBanner] Calling _updateBanner...");
            this._updateBanner();

            // Reset closing flag after a short delay
            setTimeout(function() {
                this._isClosing = false;
            }.bind(this), 500);

            _debug("[NotificationBanner] ========== BANNER CLOSE COMPLETED ==========");
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
            switch ((severity || "").toUpperCase()) {
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
         * Handle notification acknowledgment (user clicked OK)
         * @private
         * @param {object} notification - The notification object
         */
        _acknowledgeNotification: function(notification) {
            var that = this;

            _debug("[NotificationBanner] ========== ACKNOWLEDGE NOTIFICATION ==========");
            _debug("[NotificationBanner] Acknowledging notification:", notification.message_id, notification.title);

            /**
             * CRITICAL: Prevent duplicate acknowledgments
             *
             * When user clicks "OK - I Understand" button, we must prevent:
             * 1. Multiple rapid clicks (double-click)
             * 2. Concurrent AJAX requests to backend
             *
             * This flag ensures only ONE acknowledgment request is processed at a time
             */
            if (this._isAcknowledging) {
                _debug("[NotificationBanner] Already acknowledging, ignoring duplicate request");
                return;
            }
            this._isAcknowledging = true;

            /**
             * POST to ABAP REST endpoint: /sap/bc/rest/zcl_notif_rest/acknowledge
             *
             * Request payload:
             * {
             *   "message_id": "unique_notification_id",
             *   "client_info": "Mozilla/5.0 ..." (browser user agent)
             * }
             *
             * Expected responses:
             * - 200 OK: Acknowledgment recorded successfully in ZNOTIFY_ACK_LOG
             * - 409 Conflict: User already acknowledged this notification (duplicate)
             * - 500 Internal Server Error: Database error or ABAP exception
             */
            CsrfAjax.ajax({
                url: "/sap/bc/rest/zcl_notif_rest/acknowledge",
                type: "POST",
                contentType: "application/json",
                data: JSON.stringify({
                    message_id: notification.message_id,
                    client_info: navigator.userAgent  // Track browser/device for audit trail
                }),
                timeout: 10000,  // 10 second timeout
                success: function(response) {
                    _debug("[NotificationBanner] Acknowledgment recorded successfully:", response);

                    /**
                     * DUAL PERSISTENCE STRATEGY:
                     *
                     * 1. Backend (ABAP): ZNOTIFY_ACK_LOG table (permanent, audit trail)
                     * 2. Frontend (Browser): localStorage (session cache, prevents re-fetch)
                     *
                     * Why both?
                     * - Backend: Legal/compliance requirement, cross-device tracking
                     * - Frontend: Performance optimization, reduces server load
                     */
                    that._markAsAcknowledged(notification.message_id);

                    // Show success feedback to user
                    MessageToast.show("Notification acknowledged");

                    // Remove notification from display and show next one if available
                    that._handleAcknowledgedNotification(notification);

                    that._isAcknowledging = false;
                },
                error: function(xhr, status, error) {
                    that._isAcknowledging = false;

                    /**
                     * ERROR HANDLING STRATEGY:
                     *
                     * 409 Conflict: Already acknowledged by this user
                     *   - This is actually a SUCCESS case (user saw and acknowledged)
                     *   - Show friendly message, not an error
                     *
                     * 500 Server Error: Database failure or ABAP dump
                     *   - Show error dialog
                     *   - Do NOT remove notification from display
                     *   - User can retry acknowledgment
                     */
                    if (xhr.status === 409) {
                        // Already acknowledged (e.g. on another device): treat as success
                        that._markAsAcknowledged(notification.message_id);
                        MessageToast.show("Notification already acknowledged");
                        that._handleAcknowledgedNotification(notification);
                        return;
                    }

                    Log.error("Failed to acknowledge notification " + notification.message_id + ": " + error +
                              " (Status: " + xhr.status + ")", "", LOG_COMPONENT);
                    MessageBox.error("Failed to acknowledge notification: " + error, {
                        title: "Acknowledgment Error",
                        actions: [MessageBox.Action.CLOSE]
                    });
                }
            });
        },

        /**
         * Mark notification as acknowledged in browser localStorage
         *
         * PURPOSE:
         * Creates a client-side cache of acknowledged notifications to improve UX
         *
         * DATA STRUCTURE:
         * {
         *   "MSG_001": { "timestamp": 1728550234567 },
         *   "MSG_002": { "timestamp": 1728550345678 }
         * }
         *
         * WHY TIMESTAMP?
         * - Future cleanup: could implement expiration (e.g., remove after 30 days)
         * - Debugging: helps understand when user acknowledged
         * - Analytics: potential for acknowledgment time tracking
         *
         * ERROR HANDLING:
         * localStorage can fail due to:
         * - QuotaExceededError: Storage full (rarely happens for small data)
         * - SecurityError: Disabled in private browsing mode
         * - Catch all errors and fail gracefully (backend is source of truth)
         *
         * @private
         * @param {string} messageId - The message ID to mark as acknowledged
         */
        _markAsAcknowledged: function(messageId) {
            try {
                var acknowledged = this._getAcknowledgedNotifications();
                acknowledged[messageId] = {
                    timestamp: Date.now()  // Unix timestamp in milliseconds
                };
                localStorage.setItem("acknowledgedNotifications", JSON.stringify(acknowledged));
                _debug("[NotificationBanner] Marked as acknowledged in localStorage:", messageId);
            } catch (e) {
                // IMPORTANT: Don't show error to user - backend persistence is sufficient
                Log.error("Failed to save acknowledged notification to localStorage: " + e.message);
            }
        },

        /**
         * Get all acknowledged notifications from browser localStorage
         *
         * CACHE STRATEGY:
         * - localStorage is SECONDARY cache (backend ZNOTIFY_ACK_LOG is primary)
         * - Used to avoid re-fetching already acknowledged notifications
         * - Survives page refresh within same browser
         * - Does NOT sync across devices/browsers (by design)
         *
         * FALLBACK BEHAVIOR:
         * If localStorage fails or is disabled:
         * - Return empty object {}
         * - App continues to work (fetches from backend every time)
         * - Slight performance impact but fully functional
         *
         * @private
         * @returns {object} Map of messageId -> {timestamp}
         */
        _getAcknowledgedNotifications: function() {
            try {
                var data = localStorage.getItem("acknowledgedNotifications");
                return data ? JSON.parse(data) : {};
            } catch (e) {
                // Fail silently - return empty object to allow app to continue
                Log.error("Failed to read acknowledged notifications from localStorage: " + e.message);
                return {};
            }
        },

        /**
         * Handle acknowledged notification removal from display
         * @private
         * @param {object} notification - The notification object
         */
        _handleAcknowledgedNotification: function(notification) {
            _debug("[NotificationBanner] Removing acknowledged notification from display");

            // Remove current notification from display list
            var index = this._bannerNotifications.indexOf(notification);
            if (index > -1) {
                this._bannerNotifications.splice(index, 1);
                _debug("[NotificationBanner] Removed from bannerNotifications, new count:", this._bannerNotifications.length);
            }

            // Also remove from all notifications
            var allIndex = this._allNotifications.findIndex(function(n) {
                return n.message_id === notification.message_id;
            });
            if (allIndex > -1) {
                this._allNotifications.splice(allIndex, 1);
            }

            // Show next notification or remove banner if no more notifications
            if (this._bannerNotifications.length > 0) {
                // Adjust index if needed
                if (this._currentBannerIndex >= this._bannerNotifications.length) {
                    this._currentBannerIndex = this._bannerNotifications.length - 1;
                }
                _debug("[NotificationBanner] Showing next notification at index:", this._currentBannerIndex);
                this._removeBanner();
                this._showBanner();
            } else {
                _debug("[NotificationBanner] No more notifications to show, removing banner");
                this._removeBanner();
            }
        },

        /**
         * Attach banner to Fiori Shell
         * @public
         */
        attachToShell: function() {
            _debug("[NotificationBanner] ========== ATTACHING TO SHELL ==========");
            this._isAttachedToShell = true;

            // Load and display notifications
            this.loadNotifications();
        },

        /**
         * Destroy banner and clean up
         * @public
         */
        destroy: function() {
            if (this._circuitResetTimeout) {
                clearTimeout(this._circuitResetTimeout);
                this._circuitResetTimeout = null;
            }
            this._removeBanner();
            BaseObject.prototype.destroy.apply(this, arguments);
        }
    });
});