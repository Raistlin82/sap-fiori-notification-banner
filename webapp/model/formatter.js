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
        },

        /**
         * Format ABAP DATS date (YYYYMMDD) to readable format
         * @param {string} abapDate - Date in ABAP DATS format (e.g., "20251003")
         * @returns {string} Formatted date (e.g., "03/10/2025")
         */
        formatAbapDate: function (abapDate) {
            if (!abapDate || abapDate.length !== 8) {
                return "";
            }
            // Parse YYYYMMDD
            var year = abapDate.substring(0, 4);
            var month = abapDate.substring(4, 6);
            var day = abapDate.substring(6, 8);

            return day + "/" + month + "/" + year;
        }
    };
});
