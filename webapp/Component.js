sap.ui.define([
    "sap/ui/core/UIComponent",
    "sap/ui/Device",
    "com/sap/notifications/banner2/model/models",
    "com/sap/notifications/banner2/controller/NotificationBanner",
    "com/sap/notifications/banner2/controller/TileCounter",
    "sap/base/Log"
], function (UIComponent, Device, models, NotificationBanner, TileCounter, Log) {
    "use strict";

    return UIComponent.extend("com.sap.notifications.banner2.Component", {

        metadata: {
            manifest: "json"
        },

        /**
         * The component is initialized by UI5 automatically during the startup of the app and calls the init method once.
         * @public
         * @override
         */
        init: function () {
            var that = this;

            Log.info("[Component.js] ========== COMPONENT INIT START ==========");

            // call the base component's init function
            UIComponent.prototype.init.apply(this, arguments);

            // Check if mock server should be initialized (development mode)
            var oUriParams = new URLSearchParams(window.location.search);
            var bUseMockData = oUriParams.get("sap-ui-xx-mockserver") === "true" ||
                              window.location.hostname === "localhost" ||
                              window.location.hostname === "127.0.0.1";

            Log.info("[Component.js] Mock data mode: " + bUseMockData);

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
                }, function(_error) {
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

            Log.info("[Component.js] Initializing NotificationBanner...");

            // Create notification banner instance
            this._notificationBanner = new NotificationBanner();
            Log.info("[Component.js] NotificationBanner instance created");

            // Create tile counter instance
            this._tileCounter = new TileCounter();
            Log.info("[Component.js] TileCounter instance created");

            // Start polling for notifications every 30 seconds
            this._startNotificationPolling();

            // Listen for shell container ready event or attach immediately in standalone mode
            if (typeof sap !== "undefined" && sap.ushell && sap.ushell.Container) {
                Log.info("[Component.js] FLP mode detected");

                // Check if renderer is already created
                var oRenderer = sap.ushell.Container.getRenderer();
                if (oRenderer) {
                    Log.info("[Component.js] Shell renderer already exists - attaching banner immediately");
                    that._notificationBanner.attachToShell();
                    that._tileCounter.start();
                } else {
                    Log.info("[Component.js] Shell renderer not ready - waiting for RendererCreatedEvent");
                    // FLP mode - wait for shell to be ready
                    sap.ushell.Container.attachRendererCreatedEvent(function() {
                        Log.info("[Component.js] Shell renderer created - attaching banner");
                        that._notificationBanner.attachToShell();
                        that._tileCounter.start();
                    });
                }
            } else {
                Log.info("[Component.js] Standalone mode - attaching banner immediately");
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

            // Set up periodic polling and store interval ID for cleanup
            this._pollingInterval = setInterval(function() {
                that._notificationBanner.loadNotifications();
            }, 30000); // 30 seconds

            Log.info("[Component.js] Notification polling started (30s interval)");
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
            // Clear polling interval to prevent memory leaks
            if (this._pollingInterval) {
                clearInterval(this._pollingInterval);
                this._pollingInterval = null;
                Log.info("[Component.js] Notification polling stopped");
            }

            // Destroy notification banner
            if (this._notificationBanner) {
                this._notificationBanner.destroy();
                this._notificationBanner = null;
            }

            // Destroy tile counter
            if (this._tileCounter) {
                this._tileCounter.destroy();
                this._tileCounter = null;
            }

            Log.info("[Component.js] Component cleanup completed");
        }
    });
});