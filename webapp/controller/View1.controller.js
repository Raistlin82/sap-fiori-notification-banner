sap.ui.define([
    "sap/ui/core/mvc/Controller",
    "sap/m/MessageToast"
], function (Controller, MessageToast) {
    "use strict";

    return Controller.extend("com.sap.notifications.banner2.controller.View1", {

        onInit: function () {
            // Controller initialization
        },

        /**
         * Test notification functionality
         */
        onTestNotification: function () {
            var oComponent = this.getOwnerComponent();
            var oNotificationBanner = oComponent.getNotificationBanner();

            if (oNotificationBanner) {
                // Trigger a test notification load
                oNotificationBanner.loadNotifications();
                MessageToast.show("Test notification triggered");
            } else {
                MessageToast.show("Notification banner not initialized");
            }
        }
    });
});