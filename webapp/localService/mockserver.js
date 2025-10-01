sap.ui.define([
    "sap/ui/core/util/MockServer",
    "sap/base/Log"
], function (MockServer, Log) {
    "use strict";

    var oMockServer;

    return {

        /**
         * Initialize the mock server
         * @public
         * @returns {Promise} Promise that resolves when mock server is ready
         */
        init: function () {
            return new Promise(function (resolve) {
                var sJsonFilesPath = sap.ui.require.toUrl("com/sap/notifications/banner2/localService/mockdata");

                // Create mock server instance
                oMockServer = new MockServer({
                    rootUri: "/sap/bc/rest/zcl_notif_rest/"
                });

                // Configure mock server
                MockServer.config({
                    autoRespond: true,
                    autoRespondAfter: 500 // Simulate network delay
                });

                // Simulate GET request
                oMockServer.simulate(sJsonFilesPath + "/notifications.json", {
                    sMockdataBaseUrl: sJsonFilesPath,
                    bGenerateMissingMockData: false
                });

                // Custom request handlers
                var aRequests = oMockServer.getRequests();

                // GET /notifications
                aRequests.push({
                    method: "GET",
                    path: new RegExp(".*"),
                    response: function (oXhr) {
                        Log.info("MockServer: GET notifications request");
                        oXhr.respondJSON(200, {}, JSON.stringify({
                            notifications: [
                                {
                                    message_id: "550e8400-e29b-41d4-a716-446655440001",
                                    message_type: "URGENT",
                                    severity: "HIGH",
                                    title: "Sistema in Manutenzione Programmata",
                                    message_text: "Il sistema SAP sarà offline dalle 22:00 alle 24:00 per manutenzione straordinaria.",
                                    start_date: "2025-09-30",
                                    end_date: "2025-10-01",
                                    active: true,
                                    changed_at: "2025-09-30T08:00:00Z"
                                },
                                {
                                    message_id: "550e8400-e29b-41d4-a716-446655440002",
                                    message_type: "INFO",
                                    severity: "MEDIUM",
                                    title: "Nuovo Aggiornamento Disponibile",
                                    message_text: "È disponibile la versione 1.2.0 con nuove funzionalità.",
                                    start_date: "2025-09-30",
                                    end_date: "2025-10-15",
                                    active: true,
                                    changed_at: "2025-09-30T07:30:00Z"
                                },
                                {
                                    message_id: "550e8400-e29b-41d4-a716-446655440003",
                                    message_type: "TIP",
                                    severity: "LOW",
                                    title: "Suggerimento del Giorno",
                                    message_text: "Usa Ctrl+F per cercare rapidamente nelle tabelle.",
                                    start_date: "2025-09-30",
                                    end_date: "2025-10-30",
                                    active: true,
                                    changed_at: "2025-09-30T06:00:00Z"
                                }
                            ]
                        }));
                        return true;
                    }
                });

                oMockServer.setRequests(aRequests);

                // Start mock server
                oMockServer.start();

                Log.info("MockServer: Running with notifications mock data at " + oMockServer.getRootUri());
                resolve();
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