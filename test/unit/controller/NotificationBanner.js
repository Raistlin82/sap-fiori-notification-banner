/*global QUnit, sinon*/

sap.ui.define([
    "com/sap/notifications/banner/controller/NotificationBanner",
    "sap/base/Log"
], function (NotificationBanner, Log) {
    "use strict";

    QUnit.module("NotificationBanner", {
        beforeEach: function () {
            this.oNotificationBanner = new NotificationBanner();
            this.server = sinon.fakeServer.create();
            this.server.autoRespond = true;
        },
        afterEach: function () {
            this.oNotificationBanner.destroy();
            this.server.restore();
        }
    });

    QUnit.test("Should initialize correctly", function (assert) {
        assert.ok(this.oNotificationBanner, "NotificationBanner instance created");
        assert.strictEqual(this.oNotificationBanner._retryCount, 0, "Retry count initialized to 0");
        assert.strictEqual(this.oNotificationBanner._consecutiveErrors, 0, "Consecutive errors initialized to 0");
        assert.strictEqual(this.oNotificationBanner._isCircuitOpen, false, "Circuit breaker is closed");
    });

    QUnit.test("Should handle successful notification load", function (assert) {
        var done = assert.async();

        this.server.respondWith("GET", /zcl_notification_rest/, [
            200,
            { "Content-Type": "application/json" },
            JSON.stringify({
                notifications: [
                    {
                        message_id: "1",
                        severity: "HIGH",
                        title: "Test",
                        message_text: "Test message",
                        changed_at: "2025-09-30T10:00:00Z"
                    }
                ]
            })
        ]);

        this.oNotificationBanner.attachToShell();
        this.oNotificationBanner.loadNotifications();

        setTimeout(function () {
            assert.strictEqual(this.oNotificationBanner._retryCount, 0, "Retry count reset after success");
            assert.strictEqual(this.oNotificationBanner._consecutiveErrors, 0, "Error count reset after success");
            done();
        }.bind(this), 100);
    });

    QUnit.test("Should retry on error with exponential backoff", function (assert) {
        var done = assert.async();
        var callCount = 0;

        this.server.respondWith("GET", /zcl_notification_rest/, function (xhr) {
            callCount++;
            xhr.respond(500, { "Content-Type": "text/plain" }, "Internal Server Error");
        });

        var clock = sinon.useFakeTimers();

        this.oNotificationBanner.loadNotifications();

        // First call
        this.server.respond();
        assert.strictEqual(callCount, 1, "Initial call made");

        // First retry after 1 second
        clock.tick(1000);
        this.server.respond();
        assert.strictEqual(callCount, 2, "First retry after 1s");

        // Second retry after 2 seconds
        clock.tick(2000);
        this.server.respond();
        assert.strictEqual(callCount, 3, "Second retry after 2s");

        // Third retry after 4 seconds
        clock.tick(4000);
        this.server.respond();
        assert.strictEqual(callCount, 4, "Third retry after 4s");

        clock.restore();
        done();
    });

    QUnit.test("Should open circuit breaker after consecutive errors", function (assert) {
        var done = assert.async();

        this.server.respondWith("GET", /zcl_notification_rest/, [
            500,
            { "Content-Type": "text/plain" },
            "Internal Server Error"
        ]);

        // Simulate 5 consecutive errors
        for (var i = 0; i < 5; i++) {
            this.oNotificationBanner._consecutiveErrors = i;
            this.oNotificationBanner.loadNotifications();
            this.server.respond();
        }

        setTimeout(function () {
            assert.strictEqual(this.oNotificationBanner._isCircuitOpen, true, "Circuit breaker opened after 5 errors");
            done();
        }.bind(this), 100);
    });

    QUnit.test("Should handle user ID safely in standalone mode", function (assert) {
        var done = assert.async();

        // Mock no FLP environment
        var originalUshell = sap.ushell;
        delete sap.ushell;

        this.server.respondWith("GET", /zcl_notification_rest\?user_id=ANONYMOUS/, [
            200,
            { "Content-Type": "application/json" },
            JSON.stringify({ notifications: [] })
        ]);

        this.oNotificationBanner.loadNotifications();

        setTimeout(function () {
            assert.ok(true, "No error thrown in standalone mode");
            sap.ushell = originalUshell;
            done();
        }, 100);
    });

    QUnit.test("Should show error banner on failure", function (assert) {
        var done = assert.async();

        this.server.respondWith("GET", /zcl_notification_rest/, [
            503,
            { "Content-Type": "text/plain" },
            "Service Unavailable"
        ]);

        this.oNotificationBanner.attachToShell();
        this.oNotificationBanner._maxRetries = 0; // Skip retries for this test

        this.oNotificationBanner.loadNotifications();

        setTimeout(function () {
            assert.ok(this.oNotificationBanner._currentBanner, "Error banner displayed");
            done();
        }.bind(this), 100);
    });

    QUnit.test("Should reset circuit breaker after timeout", function (assert) {
        var done = assert.async();
        var clock = sinon.useFakeTimers();

        // Open circuit breaker
        this.oNotificationBanner._consecutiveErrors = 5;
        this.oNotificationBanner._openCircuitBreaker();

        assert.strictEqual(this.oNotificationBanner._isCircuitOpen, true, "Circuit breaker opened");

        // Fast-forward 60 seconds
        clock.tick(60000);

        setTimeout(function () {
            assert.strictEqual(this.oNotificationBanner._isCircuitOpen, false, "Circuit breaker reset after timeout");
            clock.restore();
            done();
        }.bind(this), 100);
    });
});