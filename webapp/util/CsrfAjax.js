sap.ui.define([
    "sap/base/Log"
], function (Log) {
    "use strict";

    /**
     * CsrfAjax - jQuery.ajax wrapper for modifying requests (POST/PUT/DELETE)
     * against the notification REST service.
     *
     * Implements the standard SAP CSRF handshake:
     * 1. GET with header "X-CSRF-Token: Fetch" returns a token in the response header
     * 2. Modifying requests send the token in the "X-CSRF-Token" header
     * 3. If the backend answers 403 with "X-CSRF-Token: Required" (token expired,
     *    e.g. after session timeout) the token is fetched again and the request
     *    is retried once
     *
     * Usage is identical to jQuery.ajax with success/error callbacks:
     *   CsrfAjax.ajax({ url: ..., type: "POST", data: ..., success: fn, error: fn });
     */

    // Cheap GET endpoint used only to obtain the token
    var TOKEN_URL = "/sap/bc/rest/zcl_notif_rest/stats";

    var _sToken = null;
    var _oTokenPromise = null;

    /**
     * Fetch (or reuse) the CSRF token
     * @private
     * @param {boolean} bForce - Ignore the cached token
     * @returns {Promise<string>} Token
     */
    function _fetchToken(bForce) {
        if (_sToken && !bForce) {
            return Promise.resolve(_sToken);
        }
        if (_oTokenPromise && !bForce) {
            return _oTokenPromise;
        }

        _oTokenPromise = new Promise(function (resolve, reject) {
            jQuery.ajax({
                url: TOKEN_URL,
                type: "GET",
                headers: { "X-CSRF-Token": "Fetch" },
                timeout: 10000,
                success: function (data, textStatus, xhr) {
                    _sToken = xhr.getResponseHeader("X-CSRF-Token");
                    _oTokenPromise = null;
                    if (!_sToken) {
                        Log.warning("CsrfAjax: backend did not return a CSRF token");
                    }
                    resolve(_sToken);
                },
                error: function (xhr, textStatus, error) {
                    _oTokenPromise = null;
                    reject({ xhr: xhr, textStatus: textStatus, error: error });
                }
            });
        });

        return _oTokenPromise;
    }

    /**
     * Send request with token, retry once on token expiry
     * @private
     * @param {object} oSettings - jQuery.ajax settings without callbacks
     * @param {boolean} bRetried - Whether this is already the retry
     * @returns {Promise<object>} Resolves with {data, textStatus, xhr}
     */
    function _send(oSettings, bRetried) {
        return _fetchToken(bRetried).then(function (sToken) {
            return new Promise(function (resolve, reject) {
                var oHeaders = Object.assign({}, oSettings.headers);
                if (sToken) {
                    oHeaders["X-CSRF-Token"] = sToken;
                }

                jQuery.ajax(Object.assign({}, oSettings, {
                    headers: oHeaders,
                    success: function (data, textStatus, xhr) {
                        resolve({ data: data, textStatus: textStatus, xhr: xhr });
                    },
                    error: function (xhr, textStatus, error) {
                        var sTokenHeader = String(xhr.getResponseHeader("X-CSRF-Token") || "");
                        if (!bRetried && xhr.status === 403 && sTokenHeader.toLowerCase() === "required") {
                            Log.info("CsrfAjax: CSRF token expired, fetching a new one and retrying");
                            _sToken = null;
                            resolve(_send(oSettings, true));
                            return;
                        }
                        reject({ xhr: xhr, textStatus: textStatus, error: error });
                    }
                }));
            });
        });
    }

    return {
        /**
         * jQuery.ajax replacement that adds the CSRF token
         * @public
         * @param {object} oSettings - jQuery.ajax settings incl. success/error callbacks
         */
        ajax: function (oSettings) {
            var fnSuccess = oSettings.success;
            var fnError = oSettings.error;
            var oRequest = Object.assign({}, oSettings);
            delete oRequest.success;
            delete oRequest.error;

            _send(oRequest, false).then(function (oResult) {
                if (fnSuccess) {
                    fnSuccess(oResult.data, oResult.textStatus, oResult.xhr);
                }
            }, function (oFailure) {
                if (fnError) {
                    fnError(oFailure.xhr, oFailure.textStatus, oFailure.error);
                }
            });
        }
    };
});
