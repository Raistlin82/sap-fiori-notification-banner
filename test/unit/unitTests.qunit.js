/* global QUnit */
QUnit.config.autostart = false;

sap.ui.getCore().attachInit(function () {
    "use strict";

    sap.ui.require([
        "com/sap/notifications/banner/test/unit/controller/NotificationBanner",
        "com/sap/notifications/banner/test/unit/model/models"
    ], function () {
        QUnit.start();
    });
});