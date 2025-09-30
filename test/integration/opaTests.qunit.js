/* global QUnit */
QUnit.config.autostart = false;

sap.ui.getCore().attachInit(function () {
    "use strict";

    sap.ui.require([
        "com/sap/notifications/banner/test/integration/ApplicationJourney"
    ], function () {
        QUnit.start();
    });
});