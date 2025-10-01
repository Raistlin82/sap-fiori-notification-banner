sap.ui.define([
    "sap/ui/base/Object",
    "sap/base/Log"
], function(BaseObject, Log) {
    "use strict";

    /**
     * TileCounter - Manages dynamic notification counter for FLP tile
     * Updates tile with active notification counts by severity
     * Format: "10 Active | 3H|5M|2L" (High, Medium, Low breakdown)
     */
    return BaseObject.extend("com.sap.notifications.banner.controller.TileCounter", {

        constructor: function() {
            BaseObject.apply(this, arguments);
            this._updateInterval = null;
            this._updateFrequency = 60000; // 60 seconds
            this._isRunning = false;
        },

        /**
         * Start periodic tile updates
         * @public
         */
        start: function() {
            if (this._isRunning) {
                return;
            }

            this._isRunning = true;

            // Initial update
            this._updateTile();

            // Schedule periodic updates
            var that = this;
            this._updateInterval = setInterval(function() {
                that._updateTile();
            }, this._updateFrequency);

            Log.info("TileCounter started - updating every " + (this._updateFrequency / 1000) + " seconds");
        },

        /**
         * Stop periodic tile updates
         * @public
         */
        stop: function() {
            if (this._updateInterval) {
                clearInterval(this._updateInterval);
                this._updateInterval = null;
            }
            this._isRunning = false;
            Log.info("TileCounter stopped");
        },

        /**
         * Update tile with current notification statistics
         * @private
         */
        _updateTile: function() {
            var that = this;

            jQuery.ajax({
                url: "/sap/bc/rest/zcl_notif_rest/stats",
                type: "GET",
                timeout: 5000,
                success: function(data) {
                    that._updateTileContent(data);
                },
                error: function(xhr, status, error) {
                    Log.error("Failed to fetch notification stats for tile: " + error);
                }
            });
        },

        /**
         * Update FLP tile content with statistics
         * @private
         * @param {object} stats - Statistics object with total, high_count, medium_count, low_count
         */
        _updateTileContent: function(stats) {
            try {
                // Check if running in FLP context
                if (typeof sap === "undefined" || !sap.ushell || !sap.ushell.Container) {
                    Log.warning("Not running in FLP context - tile update skipped");
                    return;
                }

                // Get the dynamic tile API
                var oTileAPI = this._getTileAPI();
                if (!oTileAPI) {
                    Log.warning("Could not access FLP tile API");
                    return;
                }

                // Prepare tile data
                var total = stats.total || 0;
                var highCount = stats.high_count || 0;
                var mediumCount = stats.medium_count || 0;
                var lowCount = stats.low_count || 0;

                // Format breakdown: "3H|5M|2L"
                var breakdown = this._formatBreakdown(highCount, mediumCount, lowCount);

                // Determine tile state based on HIGH severity count
                var infoState = this._getInfoState(highCount);

                // Update tile
                var tileData = {
                    number: total,
                    numberUnit: "Active",
                    info: breakdown,
                    infoState: infoState
                };

                oTileAPI.setTitle("System Notifications");
                oTileAPI.setNumber(tileData.number.toString());
                oTileAPI.setNumberUnit(tileData.numberUnit);
                oTileAPI.setInfo(tileData.info);
                oTileAPI.setInfoState(tileData.infoState);

                Log.info("Tile updated: " + total + " active notifications (" + breakdown + ")");

            } catch (e) {
                Log.error("Error updating tile content: " + e.message);
            }
        },

        /**
         * Get FLP tile API
         * @private
         * @returns {object|null} Tile API object or null
         */
        _getTileAPI: function() {
            try {
                // Access tile through component data
                var oComponentData = sap.ui.getCore().getComponent(this.getOwnerComponent().getId()).getComponentData();

                if (oComponentData && oComponentData.startupParameters && oComponentData.startupParameters.tileAPI) {
                    return oComponentData.startupParameters.tileAPI[0];
                }

                // Alternative: Try accessing through shell container
                if (sap.ushell && sap.ushell.Container) {
                    var oRenderer = sap.ushell.Container.getRenderer("fiori2");
                    if (oRenderer) {
                        return oRenderer.getDynamicPageTitle();
                    }
                }

                return null;

            } catch (e) {
                Log.error("Error accessing tile API: " + e.message);
                return null;
            }
        },

        /**
         * Format breakdown string: "3H|5M|2L"
         * @private
         * @param {number} high - High severity count
         * @param {number} medium - Medium severity count
         * @param {number} low - Low severity count
         * @returns {string} Formatted breakdown
         */
        _formatBreakdown: function(high, medium, low) {
            if (high === 0 && medium === 0 && low === 0) {
                return "No active notifications";
            }

            var parts = [];
            if (high > 0) {
                parts.push(high + "H");
            }
            if (medium > 0) {
                parts.push(medium + "M");
            }
            if (low > 0) {
                parts.push(low + "L");
            }

            return parts.join("|");
        },

        /**
         * Get tile info state based on HIGH severity count
         * @private
         * @param {number} highCount - Number of HIGH severity notifications
         * @returns {string} Info state (Error, Warning, Success)
         */
        _getInfoState: function(highCount) {
            if (highCount >= 3) {
                return "Error"; // RED - 3 or more HIGH severity
            } else if (highCount > 0) {
                return "Warning"; // ORANGE - 1-2 HIGH severity
            } else {
                return "Success"; // GREEN - No HIGH severity
            }
        },

        /**
         * Destroy and clean up
         * @public
         */
        destroy: function() {
            this.stop();
            BaseObject.prototype.destroy.apply(this, arguments);
        }
    });
});
