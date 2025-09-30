/*global QUnit*/

sap.ui.define([
    "sap/ui/test/opaQunit",
    "sap/ui/test/Opa5",
    "sap/ui/test/actions/Press",
    "sap/ui/test/matchers/PropertyStrictEquals"
], function (opaTest, Opa5, Press, PropertyStrictEquals) {
    "use strict";

    QUnit.module("Notification Banner Application");

    Opa5.extendConfig({
        viewNamespace: "com.sap.notifications.banner.view.",
        autoWait: true
    });

    opaTest("Should load the application", function (Given, When, Then) {
        // Arrangements
        Given.iStartMyUIComponent({
            componentConfig: {
                name: "com.sap.notifications.banner"
            }
        });

        // Assertions
        Then.waitFor({
            id: "page",
            viewName: "View1",
            success: function (oPage) {
                Opa5.assert.ok(oPage, "The View1 page is displayed");
            },
            errorMessage: "Did not find the View1 page"
        });
    });

    opaTest("Should display welcome message", function (Given, When, Then) {
        // Assertions
        Then.waitFor({
            controlType: "sap.m.MessageStrip",
            viewName: "View1",
            success: function (aMessageStrips) {
                Opa5.assert.ok(aMessageStrips.length > 0, "Welcome message strip is displayed");
                Opa5.assert.ok(aMessageStrips[0].getText().length > 0, "Message strip has text");
            },
            errorMessage: "Did not find the welcome message"
        });
    });

    opaTest("Should display notification panel", function (Given, When, Then) {
        // Assertions
        Then.waitFor({
            controlType: "sap.m.Panel",
            viewName: "View1",
            success: function (aPanels) {
                Opa5.assert.ok(aPanels.length > 0, "Notification panel is displayed");
            },
            errorMessage: "Did not find the notification panel"
        });
    });

    opaTest("Should have a test notification button", function (Given, When, Then) {
        // Assertions
        Then.waitFor({
            controlType: "sap.m.Button",
            viewName: "View1",
            matchers: new PropertyStrictEquals({
                name: "type",
                value: "Emphasized"
            }),
            success: function (aButtons) {
                Opa5.assert.ok(aButtons.length > 0, "Test notification button exists");
            },
            errorMessage: "Did not find the test notification button"
        });
    });

    opaTest("Should trigger notification load on button press", function (Given, When, Then) {
        // Actions
        When.waitFor({
            controlType: "sap.m.Button",
            viewName: "View1",
            matchers: new PropertyStrictEquals({
                name: "type",
                value: "Emphasized"
            }),
            actions: new Press(),
            errorMessage: "Did not find the test button to press"
        });

        // Assertions
        Then.waitFor({
            controlType: "sap.m.MessageToast",
            check: function () {
                return sap.ui.test.Opa5.getJQuery()(".sapMMessageToast").length > 0;
            },
            success: function () {
                Opa5.assert.ok(true, "Message toast appeared after button press");
            },
            errorMessage: "No message toast appeared"
        });

        // Cleanup
        Then.iTeardownMyUIComponent();
    });
});