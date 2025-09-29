sap.ui.define([
    "sap/ui/base/Object",
    "sap/m/MessageStrip",
    "sap/m/Button",
    "sap/m/Text",
    "sap/ui/core/library",
    "sap/ui/model/json/JSONModel",
    "sap/base/Log"
], function(BaseObject, MessageStrip, Button, Text, coreLibrary, JSONModel, Log) {
    "use strict";

    var MessageType = coreLibrary.MessageType;

    return BaseObject.extend("com.sap.notifications.banner.controller.NotificationBanner", {

        constructor: function() {
            BaseObject.apply(this, arguments);
            this._notifications = [];
            this._bannerContainer = null;
            this._currentBannerIndex = 0;
            this._currentBanner = null;
            this._model = new JSONModel();
            this._isAttachedToShell = false;
        },

        /**
         * Load notifications from backend
         * @public
         */
        loadNotifications: function() {
            var that = this;

            jQuery.ajax({
                url: "/sap/bc/rest/zcl_notification_rest/",
                type: "GET",
                data: {
                    user_id: sap.ushell.Container.getUser().getId()
                },
                success: function(data) {
                    that._processNotifications(data);
                },
                error: function(xhr, status, error) {
                    Log.error("Failed to load notifications: " + error);
                }
            });
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
                this._updateBanner();
            }
        },

        /**
         * Update banner display
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
            if (this._notifications.length === 0) {
                return;
            }

            var notification = this._notifications[this._currentBannerIndex];
            var messageType = this._getMessageType(notification.severity);

            // Create banner
            this._currentBanner = new MessageStrip({
                text: notification.title + ": " + notification.message_text,
                type: messageType,
                showIcon: true,
                showCloseButton: true,
                class: "sapUiMediumMargin notificationBanner " +
                       "notificationBanner--" + notification.severity.toLowerCase(),
                close: this._onBannerClose.bind(this)
            });

            // Add navigation buttons if multiple notifications
            if (this._notifications.length > 1) {
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

                var counterText = new Text({
                    text: (this._currentBannerIndex + 1) + " of " + this._notifications.length
                });

                // Add custom content
                this._currentBanner.addAggregation("_formattedText", prevButton);
                this._currentBanner.addAggregation("_formattedText", counterText);
                this._currentBanner.addAggregation("_formattedText", nextButton);
            }

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

            if (shellContainer) {
                var bannerContainer = jQuery("<div id='globalNotificationBanner'></div>");

                if (shellContainer.tagName === "BODY") {
                    bannerContainer.prependTo(jQuery(shellContainer));
                } else {
                    bannerContainer.insertAfter(jQuery(shellContainer));
                }

                this._currentBanner.placeAt("globalNotificationBanner");
                this._bannerContainer = bannerContainer[0];
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