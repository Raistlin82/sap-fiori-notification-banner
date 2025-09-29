sap.ui.define([
    "sap/ui/model/json/JSONModel",
    "sap/ui/Device"
], function (JSONModel, Device) {
    "use strict";

    return {
        /**
         * Create device model
         * @returns {sap.ui.model.json.JSONModel} device model
         */
        createDeviceModel: function () {
            var oModel = new JSONModel(Device);
            oModel.setDefaultBindingMode("OneWay");
            return oModel;
        },

        /**
         * Create notification model
         * @returns {sap.ui.model.json.JSONModel} notification model
         */
        createNotificationModel: function () {
            var oModel = new JSONModel({
                notifications: [],
                currentIndex: 0,
                isLoading: false,
                lastUpdated: null
            });
            oModel.setDefaultBindingMode("TwoWay");
            return oModel;
        }
    };
});