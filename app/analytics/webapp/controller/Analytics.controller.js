sap.ui.define([
    "sap/ui/core/mvc/Controller",
    "sap/ui/model/json/JSONModel",
    "sap/m/MessageToast",
    "sap/m/MessageBox",
    "sap/ui/model/Filter",
    "sap/ui/model/FilterOperator",
    "sap/ui/core/format/DateFormat"
], function (Controller, JSONModel, MessageToast, MessageBox, Filter, FilterOperator, DateFormat) {
    "use strict";

    return Controller.extend("com.sap.notifications.analytics.controller.Analytics", {

        onInit: function () {
            // Create analytics model
            var oModel = new JSONModel({
                notifications: [],
                selectedNotificationId: null,
                selectedNotification: null,
                users: [],
                chartData: [],
                busy: false
            });
            this.getView().setModel(oModel, "analytics");

            // Load notifications list
            this.loadNotifications();
        },

        /**
         * Load all notifications requiring acknowledgment
         */
        loadNotifications: function () {
            var that = this;
            var oModel = this.getView().getModel("analytics");

            oModel.setProperty("/busy", true);

            jQuery.ajax({
                url: "/sap/bc/rest/zcl_notif_analytics/notifications",
                type: "GET",
                success: function (data) {
                    oModel.setProperty("/notifications", data.notifications || []);
                    oModel.setProperty("/busy", false);
                },
                error: function (xhr, status, error) {
                    MessageBox.error("Failed to load notifications: " + error);
                    oModel.setProperty("/busy", false);
                }
            });
        },

        /**
         * Handle notification selection change
         */
        onNotificationChange: function (oEvent) {
            var sSelectedKey = oEvent.getParameter("selectedItem").getKey();
            this.loadAnalytics(sSelectedKey);
        },

        /**
         * Load analytics data for selected notification
         */
        loadAnalytics: function (sMessageId) {
            var that = this;
            var oModel = this.getView().getModel("analytics");

            if (!sMessageId) {
                return;
            }

            oModel.setProperty("/busy", true);

            jQuery.ajax({
                url: "/sap/bc/rest/zcl_notif_analytics/analytics/" + sMessageId,
                type: "GET",
                success: function (data) {
                    // Set selected notification data
                    oModel.setProperty("/selectedNotification", data);
                    oModel.setProperty("/users", data.users || []);

                    // Prepare chart data
                    var aChartData = [
                        {
                            status: that.getView().getModel("i18n").getResourceBundle().getText("acknowledgedSegment"),
                            count: data.acknowledged
                        },
                        {
                            status: that.getView().getModel("i18n").getResourceBundle().getText("pendingSegment"),
                            count: data.pending
                        }
                    ];
                    oModel.setProperty("/chartData", aChartData);

                    // Configure pie chart colors
                    that._configureChartColors();

                    oModel.setProperty("/busy", false);
                },
                error: function (xhr, status, error) {
                    MessageBox.error("Failed to load analytics: " + error);
                    oModel.setProperty("/busy", false);
                }
            });
        },

        /**
         * Configure pie chart colors (green for acknowledged, orange for pending)
         */
        _configureChartColors: function () {
            var oVizFrame = this.byId("pieChart");
            if (oVizFrame) {
                oVizFrame.setVizProperties({
                    plotArea: {
                        colorPalette: ["#5CB85C", "#F0AD4E"]  // Green, Orange
                    },
                    title: {
                        visible: false
                    },
                    legend: {
                        visible: true
                    }
                });
            }
        },

        /**
         * Handle chart segment selection
         */
        onChartSelect: function (oEvent) {
            var aData = oEvent.getParameter("data");
            if (aData && aData.length > 0) {
                var sStatus = aData[0].data.Status;
                var oBundle = this.getView().getModel("i18n").getResourceBundle();

                // Filter table based on chart selection
                if (sStatus === oBundle.getText("acknowledgedSegment")) {
                    this.byId("statusFilter").setSelectedKey("ACK");
                } else {
                    this.byId("statusFilter").setSelectedKey("PENDING");
                }

                this.onFilterChange();
            }
        },

        /**
         * Handle status filter change
         */
        onFilterChange: function () {
            var sFilterKey = this.byId("statusFilter").getSelectedKey();
            var oTable = this.byId("userTable");
            var oBinding = oTable.getBinding("items");

            var aFilters = [];

            if (sFilterKey === "ACK") {
                aFilters.push(new Filter("acknowledged", FilterOperator.EQ, true));
            } else if (sFilterKey === "PENDING") {
                aFilters.push(new Filter("acknowledged", FilterOperator.EQ, false));
            }

            oBinding.filter(aFilters);
        },

        /**
         * Handle search in user table
         */
        onSearch: function (oEvent) {
            var sQuery = oEvent.getParameter("query");
            var oTable = this.byId("userTable");
            var oBinding = oTable.getBinding("items");

            var aFilters = [];

            if (sQuery && sQuery.length > 0) {
                aFilters.push(new Filter({
                    filters: [
                        new Filter("userid", FilterOperator.Contains, sQuery),
                        new Filter("user_name", FilterOperator.Contains, sQuery)
                    ],
                    and: false  // OR condition
                }));
            }

            // Combine with status filter if active
            var sFilterKey = this.byId("statusFilter").getSelectedKey();
            if (sFilterKey === "ACK") {
                aFilters.push(new Filter("acknowledged", FilterOperator.EQ, true));
            } else if (sFilterKey === "PENDING") {
                aFilters.push(new Filter("acknowledged", FilterOperator.EQ, false));
            }

            oBinding.filter(aFilters);
        },

        /**
         * Handle refresh button
         */
        onRefresh: function () {
            this.loadNotifications();

            var oModel = this.getView().getModel("analytics");
            var sMessageId = oModel.getProperty("/selectedNotificationId");

            if (sMessageId) {
                this.loadAnalytics(sMessageId);
            }

            MessageToast.show("Data refreshed");
        },

        /**
         * Export table data to Excel
         */
        onExport: function () {
            var oTable = this.byId("userTable");
            var oModel = this.getView().getModel("analytics");
            var aUsers = oModel.getProperty("/users");

            if (!aUsers || aUsers.length === 0) {
                MessageBox.warning("No data to export");
                return;
            }

            // Use sap.ui.export library for Excel export
            sap.ui.require(["sap/ui/export/Spreadsheet"], function (Spreadsheet) {
                var oBundle = this.getView().getModel("i18n").getResourceBundle();

                var aCols = [
                    {
                        label: oBundle.getText("columnUserId"),
                        property: "userid",
                        type: "string"
                    },
                    {
                        label: oBundle.getText("columnUserName"),
                        property: "user_name",
                        type: "string"
                    },
                    {
                        label: oBundle.getText("columnStatus"),
                        property: "acknowledged",
                        type: "boolean"
                    },
                    {
                        label: oBundle.getText("columnAckDate"),
                        property: "ack_timestamp",
                        type: "string"
                    },
                    {
                        label: oBundle.getText("columnDaysPending"),
                        property: "days_pending",
                        type: "number"
                    },
                    {
                        label: oBundle.getText("columnClientInfo"),
                        property: "client_info",
                        type: "string"
                    }
                ];

                var oSettings = {
                    workbook: {
                        columns: aCols
                    },
                    dataSource: aUsers,
                    fileName: "notification_acknowledgments.xlsx"
                };

                var oSpreadsheet = new Spreadsheet(oSettings);
                oSpreadsheet.build()
                    .then(function () {
                        MessageToast.show("Export successful");
                    })
                    .catch(function (oError) {
                        MessageBox.error("Export failed: " + oError.message);
                    });

            }.bind(this));
        },

        /**
         * Format timestamp for display
         * @param {string} sTimestamp - ABAP timestamp (YYYYMMDDHHMMSS.mmm)
         * @returns {string} Formatted date/time
         */
        formatTimestamp: function (sTimestamp) {
            if (!sTimestamp) {
                return "-";
            }

            try {
                // Parse ABAP timestamp format: 20251010123045.123
                var sDate = sTimestamp.substring(0, 8);    // YYYYMMDD
                var sTime = sTimestamp.substring(8, 14);   // HHMMSS

                var iYear = parseInt(sDate.substring(0, 4));
                var iMonth = parseInt(sDate.substring(4, 6)) - 1;  // Month is 0-based
                var iDay = parseInt(sDate.substring(6, 8));
                var iHour = parseInt(sTime.substring(0, 2));
                var iMin = parseInt(sTime.substring(2, 4));
                var iSec = parseInt(sTime.substring(4, 6));

                var oDate = new Date(iYear, iMonth, iDay, iHour, iMin, iSec);

                var oDateFormat = DateFormat.getDateTimeInstance({
                    pattern: "dd/MM/yyyy HH:mm"
                });

                return oDateFormat.format(oDate);

            } catch (e) {
                return sTimestamp;
            }
        }

    });
});
