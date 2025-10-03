sap.ui.define([], function () {
    "use strict";

    /**
     * Private helper function to format a single date
     * @private
     */
    var _formatSingleDate = function (date) {
        if (!date) {
            return "";
        }

        // Convert to string if number
        var dateStr = String(date);

        var year, month, day;

        if (dateStr.length === 10 && dateStr.indexOf('-') !== -1) {
            // ISO format: "2025-10-03"
            var parts = dateStr.split('-');
            year = parts[0];
            month = parts[1];
            day = parts[2];
        } else if (dateStr.length === 8) {
            // ABAP DATS format: "20251003"
            year = dateStr.substring(0, 4);
            month = dateStr.substring(4, 6);
            day = dateStr.substring(6, 8);
        } else {
            return "";
        }

        return day + "/" + month + "/" + year;
    };

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
         * Format date to readable format
         * Handles both ISO format (YYYY-MM-DD) and ABAP DATS format (YYYYMMDD)
         * @param {string} date - Date in ISO or ABAP format
         * @returns {string} Formatted date (e.g., "03/10/2025")
         */
        formatAbapDate: function (date) {
            return _formatSingleDate(date);
        },

        /**
         * Format date range (start to end) in readable format
         * @param {string} startDate - Start date in ISO or ABAP format
         * @param {string} endDate - End date in ISO or ABAP format
         * @returns {string} Formatted date range (e.g., "01/10/2025 → 31/10/2025")
         */
        formatDateRange: function (startDate, endDate) {
            var start = _formatSingleDate(startDate);
            var end = _formatSingleDate(endDate);

            if (!start && !end) {
                return "";
            }

            if (!end) {
                return start;
            }

            if (!start) {
                return end;
            }

            return start + " → " + end;
        }
    };
});
