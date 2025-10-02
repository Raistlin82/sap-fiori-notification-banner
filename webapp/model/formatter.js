sap.ui.define([], function () {
    "use strict";

    return {
        /**
         * Format severity to UI5 state
         * @param {string} severity - HIGH, MEDIUM, LOW
         * @returns {string} UI5 state (Error, Warning, Success)
         */
        formatSeverityState: function (severity) {
            switch (severity) {
                case "HIGH":
                    return "Error";
                case "MEDIUM":
                    return "Warning";
                case "LOW":
                    return "Success";
                default:
                    return "None";
            }
        },

        /**
         * Format severity to icon
         * @param {string} severity - HIGH, MEDIUM, LOW
         * @returns {string} SAP icon
         */
        formatSeverityIcon: function (severity) {
            switch (severity) {
                case "HIGH":
                    return "sap-icon://message-error";
                case "MEDIUM":
                    return "sap-icon://message-warning";
                case "LOW":
                    return "sap-icon://message-information";
                default:
                    return "sap-icon://message-information";
            }
        },

        /**
         * Format boolean to icon
         * @param {boolean} active - true/false
         * @returns {string} SAP icon
         */
        formatActiveIcon: function (active) {
            return active ? "sap-icon://accept" : "sap-icon://decline";
        },

        /**
         * Format boolean to state
         * @param {boolean} active - true/false
         * @returns {string} UI5 state
         */
        formatActiveState: function (active) {
            return active ? "Success" : "Error";
        }
    };
});
