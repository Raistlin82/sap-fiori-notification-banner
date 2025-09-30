sap.ui.define([
    "sap/ui/core/mvc/Controller",
    "sap/ui/model/json/JSONModel",
    "sap/m/MessageToast",
    "sap/m/MessageBox",
    "sap/ui/core/Fragment",
    "sap/base/Log"
], function (Controller, JSONModel, MessageToast, MessageBox, Fragment, Log) {
    "use strict";

    return Controller.extend("com.sap.notifications.admin.controller.NotificationAdmin", {

        onInit: function () {
            // Initialize models
            this._initializeModels();

            // Load initial data
            this._loadNotifications();
            this._loadStatistics();

            // Set up refresh timer
            this._setupRefreshTimer();
        },

        /**
         * Initialize data models
         * @private
         */
        _initializeModels: function() {
            var oModel = new JSONModel({
                notifications: [],
                statistics: {
                    active: 0,
                    highPriority: 0,
                    total: 0
                }
            });

            this.getView().setModel(oModel);
        },

        /**
         * Load notifications from backend
         * @private
         */
        _loadNotifications: function() {
            var that = this;

            jQuery.ajax({
                url: "/sap/bc/rest/zcl_notification_rest/",
                type: "GET",
                success: function(data) {
                    that.getView().getModel().setProperty("/notifications", data);
                    that._calculateStatistics();
                },
                error: function(xhr, status, error) {
                    Log.error("Failed to load notifications: " + error);
                    MessageToast.show("Error loading notifications");
                }
            });
        },

        /**
         * Calculate statistics from notifications data
         * @private
         */
        _calculateStatistics: function() {
            var aNotifications = this.getView().getModel().getProperty("/notifications");
            var oStats = {
                active: 0,
                highPriority: 0,
                total: aNotifications.length
            };

            aNotifications.forEach(function(notification) {
                if (notification.active === 'X') {
                    oStats.active++;
                }
                if (notification.severity === 'HIGH') {
                    oStats.highPriority++;
                }
            });

            this.getView().getModel().setProperty("/statistics", oStats);
        },

        /**
         * Load statistics
         * @private
         */
        _loadStatistics: function() {
            // Statistics are calculated from notifications data
            // Could be extended with additional backend calls if needed
        },

        /**
         * Set up automatic refresh timer
         * @private
         */
        _setupRefreshTimer: function() {
            var that = this;
            setInterval(function() {
                that._loadNotifications();
            }, 60000); // Refresh every minute
        },

        /**
         * Handle refresh button press
         */
        onRefresh: function() {
            this._loadNotifications();
            MessageToast.show("Data refreshed");
        },

        /**
         * Handle create notification button press
         */
        onCreateNotification: function() {
            this._openNotificationDialog(null);
        },

        /**
         * Handle edit notification button press
         */
        onEditNotification: function(oEvent) {
            var oContext = oEvent.getSource().getBindingContext();
            var oNotification = oContext.getObject();
            this._openNotificationDialog(oNotification);
        },

        /**
         * Handle delete notification button press
         */
        onDeleteNotification: function(oEvent) {
            var that = this;
            var oContext = oEvent.getSource().getBindingContext();
            var oNotification = oContext.getObject();

            MessageBox.confirm(
                "Are you sure you want to delete this notification?",
                {
                    onAction: function(sAction) {
                        if (sAction === MessageBox.Action.OK) {
                            that._deleteNotification(oNotification.message_id);
                        }
                    }
                }
            );
        },

        /**
         * Handle close early notification
         */
        onCloseEarly: function(oEvent) {
            var that = this;
            var oContext = oEvent.getSource().getBindingContext();
            var oNotification = oContext.getObject();

            MessageBox.confirm(
                "Are you sure you want to close this notification early? This will set the end date to today and deactivate it.",
                {
                    title: "Close Notification Early",
                    onAction: function(sAction) {
                        if (sAction === MessageBox.Action.OK) {
                            that._closeNotificationEarly(oNotification);
                        }
                    }
                }
            );
        },

        /**
         * Handle toggle notification status
         */
        onToggleNotification: function(oEvent) {
            var oContext = oEvent.getSource().getBindingContext();
            var oNotification = oContext.getObject();

            // Toggle active status
            oNotification.active = oNotification.active === 'X' ? ' ' : 'X';

            this._updateNotification(oNotification);
        },

        /**
         * Handle search
         */
        onSearch: function(oEvent) {
            var sQuery = oEvent.getParameter("newValue");
            var oTable = this.byId("notificationsTable");
            var oBinding = oTable.getBinding("items");

            if (sQuery && sQuery.length > 0) {
                var aFilters = [
                    new sap.ui.model.Filter("title", sap.ui.model.FilterOperator.Contains, sQuery),
                    new sap.ui.model.Filter("message_text", sap.ui.model.FilterOperator.Contains, sQuery),
                    new sap.ui.model.Filter("message_type", sap.ui.model.FilterOperator.Contains, sQuery)
                ];
                var oFilter = new sap.ui.model.Filter({
                    filters: aFilters,
                    and: false
                });
                oBinding.filter(oFilter);
            } else {
                oBinding.filter([]);
            }
        },

        /**
         * Handle item press
         */
        onItemPress: function(oEvent) {
            var oContext = oEvent.getSource().getBindingContext();
            var oNotification = oContext.getObject();
            this._openNotificationDialog(oNotification);
        },

        /**
         * Open notification dialog
         * @private
         */
        _openNotificationDialog: function(oNotification) {
            var that = this;

            if (!this._pDialog) {
                this._pDialog = Fragment.load({
                    id: this.getView().getId(),
                    name: "com.sap.notifications.admin.fragment.NotificationDialog",
                    controller: this
                }).then(function (oDialog) {
                    that.getView().addDependent(oDialog);
                    return oDialog;
                });
            }

            this._pDialog.then(function(oDialog) {
                // Set dialog model
                var oDialogModel = new JSONModel();

                if (oNotification) {
                    // Edit mode
                    oDialogModel.setData({
                        notification: JSON.parse(JSON.stringify(oNotification)),
                        isEdit: true,
                        dialogTitle: "Edit Notification"
                    });
                } else {
                    // Create mode
                    var oNewNotification = {
                        message_id: "",
                        message_type: "INFO",
                        severity: "LOW",
                        title: "",
                        message_text: "",
                        start_date: new Date(),
                        end_date: new Date(),
                        target_users: "ALL",
                        active: "X"
                    };

                    oDialogModel.setData({
                        notification: oNewNotification,
                        isEdit: false,
                        dialogTitle: "Create Notification"
                    });
                }

                oDialog.setModel(oDialogModel, "dialog");
                oDialog.open();
            });
        },

        /**
         * Handle dialog save
         */
        onDialogSave: function() {
            var oDialog = this.byId("notificationDialog");
            var oDialogModel = oDialog.getModel("dialog");
            var oNotificationData = oDialogModel.getProperty("/notification");
            var bIsEdit = oDialogModel.getProperty("/isEdit");

            // Validate data
            if (!this._validateNotificationData(oNotificationData)) {
                return;
            }

            if (bIsEdit) {
                this._updateNotification(oNotificationData);
            } else {
                this._createNotification(oNotificationData);
            }

            oDialog.close();
        },

        /**
         * Handle dialog cancel
         */
        onDialogCancel: function() {
            this.byId("notificationDialog").close();
        },

        /**
         * Validate notification data
         * @private
         */
        _validateNotificationData: function(oData) {
            if (!oData.title || oData.title.trim() === "") {
                MessageToast.show("Title is required");
                return false;
            }

            if (!oData.message_text || oData.message_text.trim() === "") {
                MessageToast.show("Message text is required");
                return false;
            }

            if (oData.end_date <= oData.start_date) {
                MessageToast.show("End date must be after start date");
                return false;
            }

            return true;
        },

        /**
         * Create new notification
         * @private
         */
        _createNotification: function(oNotificationData) {
            var that = this;

            jQuery.ajax({
                url: "/sap/bc/rest/zcl_notification_rest/",
                type: "POST",
                contentType: "application/json",
                data: JSON.stringify(oNotificationData),
                success: function(data) {
                    MessageToast.show("Notification created successfully");
                    that._loadNotifications();
                },
                error: function(xhr, status, error) {
                    Log.error("Failed to create notification: " + error);
                    MessageToast.show("Error creating notification");
                }
            });
        },

        /**
         * Update existing notification
         * @private
         */
        _updateNotification: function(oNotificationData) {
            var that = this;

            jQuery.ajax({
                url: "/sap/bc/rest/zcl_notification_rest/?message_id=" + oNotificationData.message_id,
                type: "PUT",
                contentType: "application/json",
                data: JSON.stringify(oNotificationData),
                success: function(data) {
                    MessageToast.show("Notification updated successfully");
                    that._loadNotifications();
                },
                error: function(xhr, status, error) {
                    Log.error("Failed to update notification: " + error);
                    MessageToast.show("Error updating notification");
                }
            });
        },

        /**
         * Delete notification
         * @private
         */
        _deleteNotification: function(sMessageId) {
            var that = this;

            jQuery.ajax({
                url: "/sap/bc/rest/zcl_notification_rest/?message_id=" + sMessageId,
                type: "DELETE",
                success: function(data) {
                    MessageToast.show("Notification deleted successfully");
                    that._loadNotifications();
                },
                error: function(xhr, status, error) {
                    Log.error("Failed to delete notification: " + error);
                    MessageToast.show("Error deleting notification");
                }
            });
        },

        /**
         * Close notification early by setting end_date to today and deactivating
         * @private
         */
        _closeNotificationEarly: function(oNotification) {
            var that = this;

            // Create a copy of the notification
            var oUpdatedNotification = JSON.parse(JSON.stringify(oNotification));

            // Set end_date to today
            var today = new Date();
            var sToday = today.getFullYear() +
                         ('0' + (today.getMonth() + 1)).slice(-2) +
                         ('0' + today.getDate()).slice(-2);

            oUpdatedNotification.end_date = sToday;
            oUpdatedNotification.active = ' '; // Deactivate

            jQuery.ajax({
                url: "/sap/bc/rest/zcl_notification_rest/?message_id=" + oUpdatedNotification.message_id,
                type: "PUT",
                contentType: "application/json",
                data: JSON.stringify(oUpdatedNotification),
                success: function(data) {
                    MessageToast.show("Notification closed early successfully");
                    that._loadNotifications();
                },
                error: function(xhr, status, error) {
                    Log.error("Failed to close notification early: " + error);
                    MessageToast.show("Error closing notification early");
                }
            });
        },

        /**
         * Format severity state for ObjectStatus
         */
        formatSeverityState: function(sSeverity) {
            switch (sSeverity) {
                case "HIGH":
                    return "Error";
                case "MEDIUM":
                    return "Warning";
                case "LOW":
                    return "Information";
                default:
                    return "None";
            }
        },

        /**
         * Format active status text
         */
        formatActiveStatus: function(sActive) {
            return sActive === 'X' ? "Active" : "Inactive";
        },

        /**
         * Format active status state
         */
        formatActiveState: function(sActive) {
            return sActive === 'X' ? "Success" : "Error";
        },

        /**
         * Format toggle button icon
         */
        formatToggleIcon: function(sActive) {
            return sActive === 'X' ? "sap-icon://hide" : "sap-icon://show";
        },

        /**
         * Format toggle button tooltip
         */
        formatToggleTooltip: function(sActive) {
            return sActive === 'X' ? "Deactivate" : "Activate";
        },

        /**
         * Format close early button visibility
         */
        formatCloseEarlyVisible: function(sActive) {
            // Show "Close Early" button only for active notifications
            return sActive === 'X';
        }
    });
});