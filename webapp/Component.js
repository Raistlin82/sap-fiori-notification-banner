sap.ui.define([
    "sap/ui/core/UIComponent",
    "sap/ui/Device",
    "com/sap/notifications/banner2/model/models",
    "com/sap/notifications/banner2/controller/NotificationBanner",
    "sap/base/Log"
], function (UIComponent, Device, models, NotificationBanner, Log) {
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
                    // Initialize mock server for local development (wait until it is running)
                    mockserver.init().then(resolve, function(oError) {
                        Log.error("[Component.js] Mock server initialization failed: " + oError);
                        resolve();
                    });
                }, function() {
                    // Mockserver not available - continue without it (production mode)
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

            // Note: the dynamic FLP tile reads /stats directly via its
            // "Service URL" (see docs/FLP_CONFIGURATION.md), no client code needed

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
                } else {
                    Log.info("[Component.js] Shell renderer not ready - waiting for RendererCreatedEvent");
                    // FLP mode - wait for shell to be ready
                    sap.ushell.Container.attachRendererCreatedEvent(function() {
                        Log.info("[Component.js] Shell renderer created - attaching banner");
                        that._notificationBanner.attachToShell();
                    });
                }
            } else {
                Log.info("[Component.js] Standalone mode - attaching banner immediately");
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

            Log.info("[Component.js] Component cleanup completed");
        }
    });
});