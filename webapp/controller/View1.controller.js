sap.ui.define([
    "sap/ui/core/mvc/Controller",
    "sap/ui/model/json/JSONModel",
    "sap/m/MessageToast",
    "sap/m/MessageBox",
    "com/sap/notifications/banner2/model/formatter"
], function (Controller, JSONModel, MessageToast, MessageBox, formatter) {
    "use strict";

    return Controller.extend("com.sap.notifications.banner2.controller.View1", {

        formatter: formatter,

        /**
         * Controller initialization
         */
        onInit: function () {
            // Create local models
            var oViewModel = new JSONModel({
                messages: [],
                busy: false
            });
            this.getView().setModel(oViewModel);

            // Model for edit dialog
            var oEditModel = new JSONModel({
                dialogTitle: "",
                message_id: "",
                title: "",
                message_text: "",
                severity: "MEDIUM",
                message_type: "INFO",
                display_mode: "BANNER",
                target_audience: "ALL",
                start_date: this._getFormattedDate(new Date()),
                end_date: this._getFormattedDate(new Date(Date.now() + 30 * 24 * 60 * 60 * 1000)), // +30 days
                active: true
            });
            this.getView().setModel(oEditModel, "editMode");

            // Load initial data
            this.onRefresh();
        },

        /**
         * Refresh notifications from backend
         */
        onRefresh: function () {
            var that = this;
            var oModel = this.getView().getModel();

            oModel.setProperty("/busy", true);

            jQuery.ajax({
                url: "/sap/bc/rest/zcl_notif_rest/",
                type: "GET",
                data: {
                    all: "X" // Get all messages, not just active for current user
                },
                timeout: 10000,
                success: function (data) {
                    oModel.setProperty("/busy", false);

                    // Backend returns array directly or in wrapper
                    var messages = Array.isArray(data) ? data : (data.d || data);

                    // Convert active from 'X'/'' to boolean
                    messages.forEach(function(msg) {
                        // Handle ABAP 'X', boolean true, or string 'true'
                        // Convert anything else (empty string, space, null) to false
                        msg.active = !!(msg.active === 'X' || msg.active === true || msg.active === 'true');
                    });

                    oModel.setProperty("/messages", messages);
                    MessageToast.show("Loaded " + messages.length + " notifications");
                },
                error: function (xhr, status, error) {
                    oModel.setProperty("/busy", false);
                    MessageBox.error(that.getView().getModel("i18n").getProperty("msgLoadError") +
                                   "\nStatus: " + xhr.status + "\n" + error);
                }
            });
        },

        /**
         * Open create dialog
         */
        onCreate: function () {
            var oEditModel = this.getView().getModel("editMode");
            var oI18n = this.getView().getModel("i18n");

            // Reset edit model with defaults
            oEditModel.setData({
                dialogTitle: oI18n.getProperty("buttonCreate"),
                message_id: "", // Empty = new message
                title: "",
                message_text: "",
                severity: "MEDIUM",
                message_type: "INFO",
                display_mode: "BANNER",
                target_audience: "ALL",
                start_date: this._getFormattedDate(new Date()),
                end_date: this._getFormattedDate(new Date(Date.now() + 30 * 24 * 60 * 60 * 1000)),
                active: true
            });

            this._getDialog().open();
        },

        /**
         * Open edit dialog
         * @param {sap.ui.base.Event} oEvent - Button press event
         */
        onEdit: function (oEvent) {
            var oContext = oEvent.getSource().getBindingContext();
            var oMessage = oContext.getObject();
            var oEditModel = this.getView().getModel("editMode");
            var oI18n = this.getView().getModel("i18n");

            // Load message data into edit model
            oEditModel.setData({
                dialogTitle: oI18n.getProperty("buttonEdit"),
                message_id: oMessage.message_id,
                title: oMessage.title,
                message_text: oMessage.message_text,
                severity: oMessage.severity,
                message_type: oMessage.message_type,
                display_mode: oMessage.display_mode,
                target_audience: oMessage.target_audience,
                start_date: oMessage.start_date,
                end_date: oMessage.end_date,
                active: oMessage.active
            });

            this._getDialog().open();
        },

        /**
         * Save message (create or update)
         */
        onSave: function () {
            var that = this;
            var oEditModel = this.getView().getModel("editMode").getData();
            var oI18n = this.getView().getModel("i18n");

            // Validation
            if (!this._validateMessage(oEditModel)) {
                MessageBox.error(oI18n.getProperty("messageValidationError"));
                return;
            }

            // Prepare data for backend
            var oData = {
                message_id: oEditModel.message_id || this._generateUUID(),
                title: oEditModel.title,
                message_text: oEditModel.message_text,
                severity: oEditModel.severity,
                message_type: oEditModel.message_type,
                display_mode: oEditModel.display_mode,
                target_users: oEditModel.target_audience,  // ABAP expects target_users not target_audience
                start_date: this._formatDateForABAP(oEditModel.start_date),  // Convert to YYYYMMDD
                end_date: this._formatDateForABAP(oEditModel.end_date),      // Convert to YYYYMMDD
                active: oEditModel.active ? 'X' : ''
            };

            var isUpdate = !!oEditModel.message_id && oEditModel.message_id.length > 0;
            var method = isUpdate ? "PUT" : "POST";
            var url = "/sap/bc/rest/zcl_notif_rest/";
            if (isUpdate) {
                url += "?message_id=" + encodeURIComponent(oData.message_id);
            }

            jQuery.ajax({
                url: url,
                type: method,
                contentType: "application/json",
                data: JSON.stringify(oData),
                timeout: 10000,
                success: function () {
                    that._getDialog().close();
                    MessageToast.show(isUpdate ?
                        oI18n.getProperty("messageUpdateSuccess") :
                        oI18n.getProperty("messageCreateSuccess"));
                    that.onRefresh();
                },
                error: function (xhr, status, error) {
                    MessageBox.error(oI18n.getProperty("msgSaveError") +
                                   "\nStatus: " + xhr.status + "\n" + error);
                }
            });
        },

        /**
         * Delete message
         * @param {sap.ui.base.Event} oEvent - Button press event
         */
        onDelete: function (oEvent) {
            var that = this;
            var oContext = oEvent.getSource().getBindingContext();
            var oMessage = oContext.getObject();
            var oI18n = this.getView().getModel("i18n");

            MessageBox.confirm(oI18n.getProperty("msgDeleteConfirm"), {
                title: oI18n.getProperty("msgDeleteConfirmTitle"),
                onClose: function (sAction) {
                    if (sAction === MessageBox.Action.OK) {
                        jQuery.ajax({
                            url: "/sap/bc/rest/zcl_notif_rest/?message_id=" + encodeURIComponent(oMessage.message_id),
                            type: "DELETE",
                            timeout: 10000,
                            success: function () {
                                MessageToast.show(oI18n.getProperty("messageDeleteSuccess"));
                                that.onRefresh();
                            },
                            error: function (xhr, status, error) {
                                MessageBox.error(oI18n.getProperty("msgDeleteError") +
                                               "\nStatus: " + xhr.status + "\n" + error);
                            }
                        });
                    }
                }
            });
        },

        /**
         * Toggle active status
         * @param {sap.ui.base.Event} oEvent - Switch change event
         */
        onToggleActive: function (oEvent) {
            var that = this;
            var bNewState = oEvent.getParameter("state");
            var oContext = oEvent.getSource().getBindingContext();
            var oMessage = oContext.getObject();
            var oI18n = this.getView().getModel("i18n");

            // Prepare data for backend (ABAP expects 'X' or '' and YYYYMMDD dates)
            var oData = Object.assign({}, oMessage);
            oData.active = bNewState ? 'X' : '';
            // Convert dates from ISO to ABAP format if needed
            oData.start_date = this._formatDateForABAP(oData.start_date);
            oData.end_date = this._formatDateForABAP(oData.end_date);

            jQuery.ajax({
                url: "/sap/bc/rest/zcl_notif_rest/?message_id=" + encodeURIComponent(oMessage.message_id),
                type: "PUT",
                contentType: "application/json",
                data: JSON.stringify(oData),
                timeout: 10000,
                success: function () {
                    // Update model with boolean value (not string)
                    oMessage.active = bNewState;
                    MessageToast.show(bNewState ?
                        oI18n.getProperty("msgActivateSuccess") :
                        oI18n.getProperty("msgDeactivateSuccess"));
                    that.onRefresh();
                },
                error: function (xhr, status, error) {
                    MessageBox.error(oI18n.getProperty("msgSaveError") +
                                   "\nStatus: " + xhr.status + "\n" + error);
                    // Revert switch
                    oEvent.getSource().setState(!bNewState);
                }
            });
        },

        /**
         * Search in table
         * @param {sap.ui.base.Event} oEvent - Search event
         */
        onSearch: function (oEvent) {
            var sQuery = oEvent.getParameter("query") || oEvent.getParameter("newValue");
            var oTable = this.byId("messagesTable");
            var oBinding = oTable.getBinding("items");

            if (oBinding) {
                var aFilters = [];
                if (sQuery && sQuery.length > 0) {
                    aFilters.push(new sap.ui.model.Filter({
                        filters: [
                            new sap.ui.model.Filter("title", sap.ui.model.FilterOperator.Contains, sQuery),
                            new sap.ui.model.Filter("message_text", sap.ui.model.FilterOperator.Contains, sQuery)
                        ],
                        and: false
                    }));
                }
                oBinding.filter(aFilters);
            }
        },

        /**
         * Apply filters
         */
        onFilterChange: function () {
            var oTable = this.byId("messagesTable");
            var oBinding = oTable.getBinding("items");
            var sActiveFilter = this.byId("filterActive").getSelectedKey();
            var sSeverity = this.byId("filterSeverity").getSelectedKey();

            if (oBinding) {
                var aFilters = [];

                // Active filter with 3 states: "" (all), "active" (true only), "inactive" (false only)
                if (sActiveFilter === "active") {
                    aFilters.push(new sap.ui.model.Filter("active", sap.ui.model.FilterOperator.EQ, true));
                } else if (sActiveFilter === "inactive") {
                    aFilters.push(new sap.ui.model.Filter("active", sap.ui.model.FilterOperator.EQ, false));
                }
                // If sActiveFilter === "" (All), no filter applied

                // Severity filter
                if (sSeverity) {
                    aFilters.push(new sap.ui.model.Filter("severity", sap.ui.model.FilterOperator.EQ, sSeverity));
                }

                oBinding.filter(aFilters);
            }
        },

        /**
         * Cancel dialog
         */
        onCancelDialog: function () {
            this._getDialog().close();
        },

        /**
         * Date change handler for validation
         */
        onDateChange: function () {
            var oEditModel = this.getView().getModel("editMode").getData();
            var startDate = new Date(oEditModel.start_date);
            var endDate = new Date(oEditModel.end_date);

            if (endDate < startDate) {
                MessageToast.show(this.getView().getModel("i18n").getProperty("validationEndDateInvalid"));
            }
        },

        /* =========================================================== */
        /* Internal helper methods                                     */
        /* =========================================================== */

        /**
         * Get or create dialog
         * @returns {sap.m.Dialog} Dialog instance
         * @private
         */
        _getDialog: function () {
            if (!this._oDialog) {
                this._oDialog = this.byId("messageDialog");
            }
            return this._oDialog;
        },

        /**
         * Validate message data
         * @param {object} oData - Message data
         * @returns {boolean} True if valid
         * @private
         */
        _validateMessage: function (oData) {
            if (!oData.title || oData.title.trim().length === 0) {
                return false;
            }
            if (!oData.message_text || oData.message_text.trim().length === 0) {
                return false;
            }
            if (!oData.start_date) {
                return false;
            }
            if (!oData.end_date) {
                return false;
            }
            var startDate = new Date(oData.start_date);
            var endDate = new Date(oData.end_date);
            if (endDate < startDate) {
                return false;
            }
            return true;
        },

        /**
         * Generate UUID v4 for ABAP char32 format (without hyphens)
         * @returns {string} UUID (32 characters, no hyphens)
         * @private
         */
        _generateUUID: function () {
            // Generate UUID and remove hyphens to fit ABAP char32 (32 characters max)
            var uuid = 'xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'.replace(/[xy]/g, function(c) {
                var r = Math.random() * 16 | 0;
                var v = c === 'x' ? r : (r & 0x3 | 0x8);
                return v.toString(16);
            });
            // Remove hyphens: "fc4b1a3e-fa30-442b-956e-52a0aafa67d5" → "fc4b1a3efa30442b956e52a0aafa67d5"
            return uuid.replace(/-/g, '');
        },

        /**
         * Format date to yyyy-MM-dd
         * @param {Date} date - Date object
         * @returns {string} Formatted date
         * @private
         */
        _getFormattedDate: function (date) {
            var year = date.getFullYear();
            var month = String(date.getMonth() + 1).padStart(2, '0');
            var day = String(date.getDate()).padStart(2, '0');
            return year + '-' + month + '-' + day;
        },

        /**
         * Format date to ABAP DATS format (YYYYMMDD)
         * @param {string} dateString - Date string in ISO format (YYYY-MM-DD)
         * @returns {string} Date in ABAP format (YYYYMMDD)
         * @private
         */
        _formatDateForABAP: function (dateString) {
            if (!dateString) {
                return '';
            }
            // Remove hyphens from ISO date format: "2025-10-03" -> "20251003"
            return dateString.replace(/-/g, '');
        }
    });
});
