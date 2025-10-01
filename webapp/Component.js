sap.ui.define([
    "sap/ui/core/UIComponent",
    "sap/ui/Device",
    "com/sap/notifications/banner2/model/models",
    "com/sap/notifications/banner2/controller/NotificationBanner",
    "com/sap/notifications/banner2/controller/TileCounter"
], function (UIComponent, Device, models, NotificationBanner, TileCounter) {
    "use strict";

    return UIComponent.extend("com.sap.notifications.banner2.Component", {

        metadata: {
            manifest: "json",
            config: {
                sample: {
                    stretch: true,
                    files: [
                        "index.html",
                        "localService/mockserver.js",
                        "localService/mockdata/notifications.json"
                    ]
                }
            }
        },

        /**
         * The component is initialized by UI5 automatically during the startup of the app and calls the init method once.
         * @public
         * @override
         */
        init: function () {
            var that = this;

            // call the base component's init function
            UIComponent.prototype.init.apply(this, arguments);

            // Check if mock server should be initialized (development mode)
            var oUriParams = new URLSearchParams(window.location.search);
            var bUseMockData = oUriParams.get("sap-ui-xx-mockserver") === "true" ||
                              window.location.hostname === "localhost" ||
                              window.location.hostname === "127.0.0.1";

            if (bUseMockData) {
                // Initialize mock server for local testing
                this._initMockServer().then(function() {
                    that._finishInitialization();
                });
            } else {
                this._finishInitialization();
            }
        },

        /**
         * Initialize mock server
         * @private
         * @returns {Promise} Promise that resolves when mock server is initialized
         */
        _initMockServer: function() {
            return new Promise(function(resolve) {
                sap.ui.require(["com/sap/notifications/banner2/localService/mockserver"], function(mockserver) {
                    mockserver.init();
                    resolve();
                }, function(error) {
                    // Mockserver not available - continue without it
                    resolve();
                });
            });
        },

        /**
         * Finish component initialization
         * @private
         */
        _finishInitialization: function() {
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

            // Create tile counter instance
            this._tileCounter = new TileCounter();

            // Start polling for notifications every 30 seconds
            this._startNotificationPolling();

            // Listen for shell container ready event or attach immediately in standalone mode
            if (typeof sap !== "undefined" && sap.ushell && sap.ushell.Container) {
                // FLP mode - wait for shell to be ready
                sap.ushell.Container.attachRendererCreatedEvent(function() {
                    that._notificationBanner.attachToShell();
                    that._tileCounter.start();
                });
            } else {
                // Standalone mode - attach immediately
                setTimeout(function() {
                    that._notificationBanner.attachToShell();
                    // Tile counter only works in FLP mode
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
         * Get tile counter instance
         * @public
         * @returns {TileCounter} tile counter
         */
        getTileCounter: function() {
            return this._tileCounter;
        },

        /**
         * Clean up component
         * @public
         */
        exit: function() {
            if (this._notificationBanner) {
                this._notificationBanner.destroy();
            }
            if (this._tileCounter) {
                this._tileCounter.destroy();
            }
        }
    });
});