sap.ui.define([
    "sap/ui/core/UIComponent",
    "sap/ui/Device",
    "com/sap/notifications/banner/model/models",
    "com/sap/notifications/banner/controller/NotificationBanner"
], function (UIComponent, Device, models, NotificationBanner) {
    "use strict";

    return UIComponent.extend("com.sap.notifications.banner.Component", {

        metadata: {
            manifest: "json"
        },

        /**
         * The component is initialized by UI5 automatically during the startup of the app and calls the init method once.
         * @public
         * @override
         */
        init: function () {
            // call the base component's init function
            UIComponent.prototype.init.apply(this, arguments);

            // enable routing
            this.getRouter().initialize();

            // set the device model
            this.setModel(models.createDeviceModel(), "device");

            // Initialize notification banner
            this._initializeNotificationBanner();
        },

        /**
         * Initialize global notification banner
         * @private
         */
        _initializeNotificationBanner: function() {
            var that = this;

            // Create notification banner instance
            this._notificationBanner = new NotificationBanner();

            // Start polling for notifications every 30 seconds
            this._startNotificationPolling();

            // Listen for shell container ready event or attach immediately in standalone mode
            if (typeof sap !== "undefined" && sap.ushell && sap.ushell.Container) {
                // FLP mode - wait for shell to be ready
                sap.ushell.Container.attachRendererCreatedEvent(function() {
                    that._notificationBanner.attachToShell();
                });
            } else {
                // Standalone mode - attach immediately
                setTimeout(function() {
                    that._notificationBanner.attachToShell();
                }, 1000);
            }
        },

        /**
         * Start polling for notifications
         * @private
         */
        _startNotificationPolling: function() {
            var that = this;

            // Poll immediately on start
            this._notificationBanner.loadNotifications();

            // Set up periodic polling
            setInterval(function() {
                that._notificationBanner.loadNotifications();
            }, 30000); // 30 seconds
        },

        /**
         * Get notification banner instance
         * @public
         * @returns {NotificationBanner} notification banner
         */
        getNotificationBanner: function() {
            return this._notificationBanner;
        },

        /**
         * Clean up component
         * @public
         */
        exit: function() {
            if (this._notificationBanner) {
                this._notificationBanner.destroy();
            }
        }
    });
});