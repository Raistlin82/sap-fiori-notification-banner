sap.ui.define([
    "sap/ui/core/util/MockServer",
    "sap/base/Log"
], function (MockServer, Log) {
    "use strict";

    /**
     * Local mock of the ABAP REST service ZCL_NOTIFICATION_REST.
     * Mirrors the real endpoints and response formats:
     *   GET    /                      -> array of active notifications (all=X: all notifications)
     *   GET    /stats                 -> {d: {number, numberUnit, info, infoState}}
     *   GET    /valid-types?severity  -> {message_types: [], default_type}
     *   GET    /valid-modes?severity&message_type -> {display_modes: [], default_mode}
     *   GET    /log                   -> array of SILENT notifications
     *   POST   /                      -> create (single or recurring)
     *   POST   /acknowledge           -> record acknowledgment (409 if duplicate)
     *   PUT    /?message_id=          -> update
     *   DELETE /?message_id=          -> delete
     * Modifying requests require the CSRF token handed out on "X-CSRF-Token: Fetch".
     */

    var ROOT_URI = "/sap/bc/rest/zcl_notif_rest/";
    var MOCK_TOKEN = "mock-csrf-token";

    // Allowed combinations (subset of ZNOTIF_MATRIX): severity -> type -> [modes], first = default
    var MATRIX = {
        HIGH: {
            URGENT: ["BANNER", "BOTH"],
            WARNING: ["BANNER", "BOTH"],
            MAINT: ["BANNER", "BOTH"],
            INFO: ["BANNER", "BOTH", "SILENT"]
        },
        MEDIUM: {
            URGENT: ["BANNER", "BOTH"],
            WARNING: ["BANNER", "TOAST", "BOTH"],
            MAINT: ["BANNER", "TOAST", "BOTH", "SILENT"],
            INFO: ["TOAST", "BANNER", "BOTH", "SILENT"],
            TIP: ["TOAST", "BANNER", "SILENT"]
        },
        LOW: {
            WARNING: ["TOAST", "SILENT"],
            MAINT: ["TOAST", "SILENT"],
            INFO: ["TOAST", "SILENT"],
            TIP: ["TOAST", "SILENT"]
        }
    };

    var oMockServer;
    var aNotifications = [];
    var mAcknowledged = {};

    function _isoDate(oDate) {
        return oDate.toISOString().substring(0, 10);
    }

    function _addDays(oDate, iDays) {
        var oResult = new Date(oDate.getTime());
        oResult.setDate(oResult.getDate() + iDays);
        return oResult;
    }

    function _requiresAck(sSeverity, sType, sMode) {
        return (sMode === "BANNER" || sMode === "BOTH") && (sSeverity === "HIGH" || sType === "URGENT") ? "X" : "";
    }

    function _getParam(sUrl, sName) {
        var sQuery = sUrl.split("?")[1] || "";
        return new URLSearchParams(sQuery).get(sName);
    }

    function _respond(oXhr, iStatus, vBody, oHeaders) {
        oXhr.respond(iStatus, Object.assign({ "Content-Type": "application/json" }, oHeaders),
            JSON.stringify(vBody));
    }

    function _tokenHeaders(oXhr) {
        var sHeader = oXhr.requestHeaders["X-CSRF-Token"] || oXhr.requestHeaders["x-csrf-token"];
        return String(sHeader).toLowerCase() === "fetch" ? { "X-CSRF-Token": MOCK_TOKEN } : {};
    }

    function _hasValidToken(oXhr) {
        var sHeader = oXhr.requestHeaders["X-CSRF-Token"] || oXhr.requestHeaders["x-csrf-token"];
        return sHeader === MOCK_TOKEN;
    }

    function _isActiveToday(oNotification) {
        var sToday = _isoDate(new Date());
        return oNotification.active === "X" &&
            oNotification.start_date <= sToday && oNotification.end_date >= sToday;
    }

    function _toIsoDate(sValue) {
        // Accept YYYYMMDD (as sent by the admin UI) and YYYY-MM-DD
        if (/^\d{8}$/.test(sValue)) {
            return sValue.substring(0, 4) + "-" + sValue.substring(4, 6) + "-" + sValue.substring(6, 8);
        }
        return sValue;
    }

    function _buildNotification(oData) {
        var sNow = new Date().toISOString();
        var sMode = oData.display_mode || "BANNER";
        var aModes = (MATRIX[oData.severity] || {})[oData.message_type];
        if (!aModes || aModes.indexOf(sMode) === -1) {
            return null;
        }
        return {
            message_id: Math.random().toString(16).substring(2).toUpperCase().padEnd(32, "0").substring(0, 32),
            message_type: oData.message_type,
            severity: oData.severity,
            title: oData.title,
            message_text: oData.message_text,
            start_date: _toIsoDate(oData.start_date),
            end_date: _toIsoDate(oData.end_date),
            target_users: oData.target_users || "ALL",
            active: "X",
            display_mode: sMode,
            requires_ack: _requiresAck(oData.severity, oData.message_type, sMode),
            created_by: "MOCKUSER",
            created_at: sNow,
            changed_by: "MOCKUSER",
            changed_at: sNow
        };
    }

    function _handleGet(oXhr) {
        var sUrl = oXhr.url;
        var oHeaders = _tokenHeaders(oXhr);
        var aActive = aNotifications.filter(_isActiveToday);

        if (sUrl.indexOf("/stats") !== -1) {
            var mCount = { HIGH: 0, MEDIUM: 0, LOW: 0 };
            aActive.forEach(function (n) {
                mCount[n.severity] = (mCount[n.severity] || 0) + 1;
            });
            _respond(oXhr, 200, { d: {
                number: String(aActive.length),
                numberUnit: "Active",
                info: "🔴 " + mCount.HIGH + "  🟠 " + mCount.MEDIUM + "  🟢 " + mCount.LOW,
                infoState: mCount.HIGH > 0 ? "Error" : (mCount.MEDIUM > 0 ? "Warning" : "Success")
            } }, oHeaders);
        } else if (sUrl.indexOf("/valid-types") !== -1) {
            var aTypes = Object.keys(MATRIX[_getParam(sUrl, "severity")] || {});
            _respond(oXhr, 200, { message_types: aTypes, default_type: aTypes[0] || "" }, oHeaders);
        } else if (sUrl.indexOf("/valid-modes") !== -1) {
            var aModes = (MATRIX[_getParam(sUrl, "severity")] || {})[_getParam(sUrl, "message_type")] || [];
            _respond(oXhr, 200, { display_modes: aModes, default_mode: aModes[0] || "" }, oHeaders);
        } else if (sUrl.indexOf("/log") !== -1) {
            _respond(oXhr, 200, aActive.filter(function (n) {
                return n.display_mode === "SILENT";
            }), oHeaders);
        } else if (_getParam(sUrl, "all") === "X") {
            _respond(oXhr, 200, aNotifications, oHeaders);
        } else {
            _respond(oXhr, 200, aActive.filter(function (n) {
                return !(n.requires_ack === "X" && mAcknowledged[n.message_id]);
            }), oHeaders);
        }
        return true;
    }

    function _handlePost(oXhr) {
        if (!_hasValidToken(oXhr)) {
            _respond(oXhr, 403, { success: false, message: "CSRF token validation failed" },
                { "X-CSRF-Token": "Required" });
            return true;
        }

        var oData = JSON.parse(oXhr.requestBody || "{}");

        if (oXhr.url.indexOf("/acknowledge") !== -1) {
            if (mAcknowledged[oData.message_id]) {
                _respond(oXhr, 409, { success: false, error: "Already acknowledged" });
            } else {
                mAcknowledged[oData.message_id] = true;
                _respond(oXhr, 200, { success: true, timestamp: new Date().toISOString() });
            }
            return true;
        }

        var iOccurrences = oData.isRecurring ? (oData.occurrences || 1) : 1;
        var aCreated = [];
        for (var i = 0; i < iOccurrences; i++) {
            var oNotification = _buildNotification(oData);
            if (!oNotification) {
                _respond(oXhr, 400, { success: false,
                    message: "Combination not allowed by notification matrix" });
                return true;
            }
            if (i > 0) {
                var iStep = { D: 1, W: 7 }[oData.recurrenceType];
                var oStart = new Date(oNotification.start_date);
                var oEnd = new Date(oNotification.end_date);
                var iDuration = Math.round((oEnd - oStart) / 86400000);
                if (iStep) {
                    oStart = _addDays(oStart, iStep * i);
                } else {
                    oStart.setMonth(oStart.getMonth() + i);
                }
                oNotification.start_date = _isoDate(oStart);
                oNotification.end_date = _isoDate(_addDays(oStart, iDuration));
            }
            aCreated.push(oNotification);
        }
        aNotifications = aNotifications.concat(aCreated);

        if (oData.isRecurring) {
            _respond(oXhr, 201, { success: true, message: "Created " + aCreated.length + " recurring notifications",
                message_ids: aCreated.map(function (n) { return n.message_id; }) });
        } else {
            _respond(oXhr, 201, { success: true, message: "Notification created",
                message_id: aCreated[0].message_id });
        }
        return true;
    }

    function _handlePut(oXhr) {
        if (!_hasValidToken(oXhr)) {
            _respond(oXhr, 403, { success: false, message: "CSRF token validation failed" },
                { "X-CSRF-Token": "Required" });
            return true;
        }

        var sId = _getParam(oXhr.url, "message_id");
        var oData = JSON.parse(oXhr.requestBody || "{}");
        var oExisting = aNotifications.find(function (n) { return n.message_id === sId; });
        var oValid = _buildNotification(oData);

        if (!oExisting || !oValid) {
            _respond(oXhr, 400, { success: false, message: "Notification not found or combination not allowed" });
            return true;
        }

        ["message_type", "severity", "title", "message_text", "start_date", "end_date",
            "target_users", "display_mode", "requires_ack"].forEach(function (sKey) {
            oExisting[sKey] = oValid[sKey];
        });
        oExisting.active = oData.active === "X" || oData.active === true ? "X" : "";
        oExisting.changed_at = new Date().toISOString();

        _respond(oXhr, 200, { success: true, message: "Notification updated" });
        return true;
    }

    function _handleDelete(oXhr) {
        if (!_hasValidToken(oXhr)) {
            _respond(oXhr, 403, { success: false, message: "CSRF token validation failed" },
                { "X-CSRF-Token": "Required" });
            return true;
        }

        var sId = _getParam(oXhr.url, "message_id");
        var iBefore = aNotifications.length;
        aNotifications = aNotifications.filter(function (n) { return n.message_id !== sId; });
        delete mAcknowledged[sId];

        if (aNotifications.length < iBefore) {
            _respond(oXhr, 200, { success: true, message: "Notification permanently deleted" });
        } else {
            _respond(oXhr, 404, { success: false, message: "Notification not found" });
        }
        return true;
    }

    return {

        /**
         * Initialize the mock server
         * @public
         * @returns {Promise} Promise that resolves when mock server is ready
         */
        init: function () {
            return new Promise(function (resolve, reject) {
                var sDataUrl = sap.ui.require.toUrl(
                    "com/sap/notifications/banner2/localService/mockdata/notifications.json");

                jQuery.ajax({
                    url: sDataUrl,
                    dataType: "json",
                    success: function (aData) {
                        // Shift validity so that active mock notifications are always current
                        var oToday = new Date();
                        aNotifications = aData.map(function (n) {
                            return Object.assign({}, n, {
                                start_date: _isoDate(_addDays(oToday, -1)),
                                end_date: _isoDate(_addDays(oToday, 7))
                            });
                        });

                        oMockServer = new MockServer({
                            rootUri: ROOT_URI,
                            requests: [
                                { method: "GET", path: /.*/, response: _handleGet },
                                { method: "POST", path: /.*/, response: _handlePost },
                                { method: "PUT", path: /.*/, response: _handlePut },
                                { method: "DELETE", path: /.*/, response: _handleDelete }
                            ]
                        });

                        MockServer.config({
                            autoRespond: true,
                            autoRespondAfter: 300 // Simulate network delay
                        });

                        oMockServer.start();
                        Log.info("MockServer: Running at " + ROOT_URI + " with " + aNotifications.length +
                            " notifications");
                        resolve();
                    },
                    error: function (xhr, status, error) {
                        reject(error);
                    }
                });
            });
        },

        /**
         * Stop the mock server
         * @public
         */
        stop: function () {
            if (oMockServer) {
                oMockServer.stop();
                Log.info("MockServer: Stopped");
            }
        },

        /**
         * Get mock server instance
         * @public
         * @returns {sap.ui.core.util.MockServer} mock server instance
         */
        getMockServer: function () {
            return oMockServer;
        }
    };
});
