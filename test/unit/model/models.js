/*global QUnit*/

sap.ui.define([
    "com/sap/notifications/banner/model/models"
], function (models) {
    "use strict";

    QUnit.module("models");

    QUnit.test("Should create device model", function (assert) {
        var oModel = models.createDeviceModel();

        assert.ok(oModel, "Device model created");
        assert.ok(oModel.getData(), "Model has data");
        assert.ok(oModel.getData().system, "System information available");
        assert.ok(typeof oModel.getData().system.phone !== "undefined", "Phone property exists");
        assert.ok(typeof oModel.getData().system.tablet !== "undefined", "Tablet property exists");
        assert.ok(typeof oModel.getData().system.desktop !== "undefined", "Desktop property exists");
    });

    QUnit.test("Device model should be read-only", function (assert) {
        var oModel = models.createDeviceModel();

        assert.strictEqual(oModel.getDefaultBindingMode(), "OneWay", "Model is read-only");
    });
});