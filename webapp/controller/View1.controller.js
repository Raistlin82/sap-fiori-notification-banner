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
                severity: "",
                message_type: "",
                display_mode: "",
                target_audience: "ALL",
                start_date: this._getFormattedDate(new Date()),
                end_date: this._getFormattedDate(new Date(Date.now() + 30 * 24 * 60 * 60 * 1000)), // +30 days
                active: true,
                isViewMode: false,
                isCreateMode: false,
                // Step-by-step wizard control
                currentStep: 1,  // 1=Title/Message, 2=Severity, 3=MessageType, 4=DisplayMode, 5=Details
                showBackButton: false,
                showForwardButton: false,
                showSaveButton: true,
                // Recurring fields
                isRecurring: false,
                recurrenceType: "W",
                occurrences: 4,
                recurringPreview: "",
                canEnableRecurring: false,
                // Available options from matrix
                availableMessageTypes: [],
                availableDisplayModes: []
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
                        // Trim spaces to handle ABAP space padding
                        var activeValue = typeof msg.active === 'string' ? msg.active.trim() : msg.active;
                        msg.active = !!(activeValue === 'X' || activeValue === true || activeValue === 'true');
                    });

                    oModel.setProperty("/messages", messages);
                    MessageToast.show("Loaded " + messages.length + " notifications");

                    // Reapply current filter after refresh
                    that.onFilterChange();
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

            // Reset edit model with wizard starting at step 1
            oEditModel.setData({
                dialogTitle: oI18n.getProperty("buttonCreate"),
                message_id: "", // Empty = new message
                title: "",
                message_text: "",
                severity: "",
                message_type: "",
                display_mode: "",
                target_audience: "ALL",
                start_date: this._getFormattedDate(new Date()),
                end_date: this._getFormattedDate(new Date(Date.now() + 30 * 24 * 60 * 60 * 1000)),
                active: true,
                isViewMode: false,
                isCreateMode: true,  // Enable wizard for new notifications
                // Wizard step control
                currentStep: 1,  // Start at step 1 (Title + Message)
                showBackButton: false,
                showForwardButton: true,
                showSaveButton: false,
                // Recurring fields
                isRecurring: false,
                recurrenceType: "W",
                occurrences: 4,
                recurringPreview: "",
                canEnableRecurring: false,
                // Available options from matrix
                availableMessageTypes: [],
                availableDisplayModes: []
            });

            // Update button visibility
            this._updateButtonVisibility();

            this._getDialog().open();
        },

        /**
         * Navigate to next step in wizard
         */
        onNextStep: function() {
            var that = this;
            var oEditModel = this.getView().getModel("editMode");
            var currentStep = oEditModel.getProperty("/currentStep");

            switch(currentStep) {
                case 1: // Title + Message → Severity
                    var title = oEditModel.getProperty("/title");
                    var message = oEditModel.getProperty("/message_text");
                    if (!title || !title.trim() || !message || !message.trim()) {
                        MessageBox.error("Please fill in both Title and Message");
                        return;
                    }
                    oEditModel.setProperty("/currentStep", 2);
                    this._updateButtonVisibility();
                    break;

                case 2: // Severity → Message Type (carica valori validi)
                    var severity = oEditModel.getProperty("/severity");
                    if (!severity) {
                        MessageBox.error("Please select a Severity");
                        return;
                    }
                    // Carica message types validi per questa severity
                    this._loadValidMessageTypesAndProceed();
                    // _updateButtonVisibility will be called after loading completes
                    break;

                case 3: // Message Type → Display Mode (carica valori validi)
                    var messageType = oEditModel.getProperty("/message_type");
                    if (!messageType) {
                        MessageBox.error("Please select a Message Type");
                        return;
                    }
                    // Carica display modes validi per severity + message type
                    this._loadValidDisplayModesAndProceed();
                    // _updateButtonVisibility will be called after loading completes
                    break;

                case 4: // Display Mode → Dettagli
                    var displayMode = oEditModel.getProperty("/display_mode");
                    if (!displayMode) {
                        MessageBox.error("Please select a Display Mode");
                        return;
                    }
                    oEditModel.setProperty("/currentStep", 5);
                    this._updateButtonVisibility();
                    // Enable recurring if all required fields are filled
                    this._checkRecurringEnabledment();
                    break;

                default:
                    break;
            }
        },

        /**
         * Navigate to previous step in wizard
         */
        onPreviousStep: function() {
            var oEditModel = this.getView().getModel("editMode");
            var currentStep = oEditModel.getProperty("/currentStep");

            if (currentStep > 1) {
                oEditModel.setProperty("/currentStep", currentStep - 1);
                this._updateButtonVisibility();
            }
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
                target_audience: oMessage.target_users || oMessage.target_audience || "ALL",  // ABAP returns target_users
                start_date: oMessage.start_date,
                end_date: oMessage.end_date,
                active: oMessage.active,
                isViewMode: false,
                isCreateMode: false,  // Edit mode - no wizard
                // Wizard - skip to final step for edit mode (message_id exists)
                currentStep: 5,  // Show all fields immediately
                // Recurring fields - cannot edit recurring for existing notifications
                isRecurring: false,
                recurrenceType: "W",
                occurrences: 4,
                recurringPreview: "",
                canEnableRecurring: false,
                // Available options from matrix - will be loaded dynamically
                availableMessageTypes: [],
                availableDisplayModes: []
            });

            // Load valid options from matrix for this notification's severity/message type
            this._loadValidMessageTypes();
            this._loadValidDisplayModes();

            // Update button visibility
            this._updateButtonVisibility();

            this._getDialog().open();
        },

        /**
         * Open view dialog (read-only)
         * @param {sap.ui.base.Event} oEvent - Button press event
         */
        onView: function (oEvent) {
            var oContext = oEvent.getSource().getBindingContext();
            var oMessage = oContext.getObject();
            var oEditModel = this.getView().getModel("editMode");

            // Load message data into edit model in view mode
            oEditModel.setData({
                dialogTitle: "View Notification",
                message_id: oMessage.message_id,
                title: oMessage.title,
                message_text: oMessage.message_text,
                severity: oMessage.severity,
                message_type: oMessage.message_type,
                display_mode: oMessage.display_mode,
                target_audience: oMessage.target_users || oMessage.target_audience || "ALL",  // ABAP returns target_users
                start_date: oMessage.start_date,
                end_date: oMessage.end_date,
                active: oMessage.active,
                isViewMode: true,
                isCreateMode: false,  // View mode - no wizard
                // Wizard - show all fields for view mode
                currentStep: 5,
                // Recurring fields - not available in view mode
                isRecurring: false,
                recurrenceType: "W",
                occurrences: 4,
                recurringPreview: "",
                canEnableRecurring: false,
                // Display modes - show current values only
                availableMessageTypes: [oMessage.message_type],
                availableDisplayModes: [oMessage.display_mode]
            });

            // Update button visibility
            this._updateButtonVisibility();

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

            // Matrix validation - check if combination is valid before saving
            this._validateMatrixCombination(oEditModel, function(isValid, errorMessage) {
                if (!isValid) {
                    // Show clear error popup
                    MessageBox.error(
                        "The selected combination is not valid according to the notification matrix:\n\n" +
                        "Severity: " + oEditModel.severity + "\n" +
                        "Message Type: " + oEditModel.message_type + "\n" +
                        "Display Mode: " + oEditModel.display_mode + "\n\n" +
                        errorMessage,
                        {
                            title: "Invalid Notification Combination",
                            styleClass: "sapUiSizeCompact"
                        }
                    );
                    return;
                }

                // Matrix validation passed - proceed with save
                that._performSave(oEditModel, oI18n);
            });
        },

        /**
         * Perform the actual save operation
         * @private
         */
        _performSave: function(oEditModel, oI18n) {
            var that = this;

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

            // Add recurring parameters if enabled
            if (oEditModel.isRecurring && !oEditModel.message_id) {
                oData.isRecurring = true;
                oData.recurrenceType = oEditModel.recurrenceType;
                oData.occurrences = parseInt(oEditModel.occurrences, 10);
            }

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
                success: function (response) {
                    that._getDialog().close();

                    // Handle different response formats
                    if (oData.isRecurring && response && response.message_ids) {
                        MessageToast.show("Successfully created " + response.message_ids.length + " recurring notifications");
                    } else {
                        MessageToast.show(isUpdate ?
                            oI18n.getProperty("messageUpdateSuccess") :
                            oI18n.getProperty("messageCreateSuccess"));
                    }

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
         * Date change handler for validation and recurring enablement
         */
        onDateChange: function () {
            var oEditModel = this.getView().getModel("editMode");
            var oData = oEditModel.getData();
            var startDate = new Date(oData.start_date);
            var endDate = new Date(oData.end_date);

            // Validate: end date must be >= start date
            if (endDate < startDate) {
                MessageToast.show(this.getView().getModel("i18n").getProperty("validationEndDateInvalid"));
            }

            // Check if all required fields are filled to enable recurring option
            var title = oEditModel.getProperty("/title");
            var message = oEditModel.getProperty("/message_text");
            var severity = oEditModel.getProperty("/severity");

            var canEnable = !!(title && message && oData.start_date && oData.end_date && severity);
            oEditModel.setProperty("/canEnableRecurring", canEnable);

            // Update preview if recurring is enabled
            if (oEditModel.getProperty("/isRecurring")) {
                this._updateRecurringPreview();
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
        },

        /**
         * Copy notification - opens create dialog with pre-populated fields
         * @param {sap.ui.base.Event} oEvent - Button press event
         */
        onCopy: function (oEvent) {
            var oContext = oEvent.getSource().getBindingContext();
            var oMessage = oContext.getObject();
            var oEditModel = this.getView().getModel("editMode");
            var oI18n = this.getView().getModel("i18n");

            // Load message data but set as new (no message_id)
            oEditModel.setData({
                dialogTitle: oI18n.getProperty("buttonCreate") + " (Copy)",
                message_id: "", // Empty = new message
                title: oMessage.title,
                message_text: oMessage.message_text,
                severity: oMessage.severity,
                message_type: oMessage.message_type,
                display_mode: oMessage.display_mode,
                target_audience: oMessage.target_users || oMessage.target_audience || "ALL",  // ABAP returns target_users
                start_date: oMessage.start_date,
                end_date: oMessage.end_date,
                active: oMessage.active,
                isViewMode: false,
                isCreateMode: false,  // Copy mode - skip wizard, show all fields
                // Wizard - skip to final step for copy (all fields pre-filled)
                currentStep: 5,
                // Recurring fields - can be enabled for copy
                isRecurring: false,
                recurrenceType: "W",
                occurrences: 4,
                recurringPreview: "",
                canEnableRecurring: true,  // Fields are filled, so recurring can be enabled
                // Available options from matrix - will be loaded dynamically
                availableMessageTypes: [],
                availableDisplayModes: []
            });

            MessageToast.show("Notification copied. Modify as needed and save.");

            // Load valid options from matrix for copied notification's severity/message type
            this._loadValidMessageTypes();
            this._loadValidDisplayModes();

            // Update button visibility
            this._updateButtonVisibility();

            this._getDialog().open();
        },

        /**
         * Handle recurring checkbox toggle
         */
        onRecurringToggle: function () {
            var oEditModel = this.getView().getModel("editMode");
            var isRecurring = oEditModel.getProperty("/isRecurring");

            if (isRecurring) {
                // Initialize with defaults
                oEditModel.setProperty("/recurrenceType", "W");
                oEditModel.setProperty("/occurrences", 4);
                this._updateRecurringPreview();
            } else {
                // Clear preview
                oEditModel.setProperty("/recurringPreview", "");
            }
        },

        /**
         * Handle recurrence type or occurrences change
         */
        onRecurringChange: function () {
            this._updateRecurringPreview();
        },

        /**
         * Handle severity change - reload valid display modes
         */
        onSeverityChange: function () {
            var oEditModel = this.getView().getModel("editMode");
            var severity = oEditModel.getProperty("/severity");

            if (!severity) {
                // Severity cleared - disable all subsequent fields
                oEditModel.setProperty("/canSelectMessageType", false);
                oEditModel.setProperty("/canSelectDisplayMode", false);
                oEditModel.setProperty("/canFillDetails", false);
                oEditModel.setProperty("/message_type", "");
                oEditModel.setProperty("/display_mode", "");
                oEditModel.setProperty("/availableMessageTypes", []);
                oEditModel.setProperty("/availableDisplayModes", []);
                return;
            }

            // Step 2: Load valid message types for this severity
            this._loadValidMessageTypes();
        },

        /**
         * Handle message type change - reload valid display modes
         */
        onMessageTypeChange: function () {
            var oEditModel = this.getView().getModel("editMode");
            var messageType = oEditModel.getProperty("/message_type");

            if (!messageType) {
                // Message type cleared - disable subsequent fields
                oEditModel.setProperty("/canSelectDisplayMode", false);
                oEditModel.setProperty("/canFillDetails", false);
                oEditModel.setProperty("/display_mode", "");
                oEditModel.setProperty("/availableDisplayModes", []);
                return;
            }

            // Step 3: Load valid display modes for this severity + message type
            this._loadValidDisplayModes();
        },

        /**
         * Handle display mode change - enable detail fields
         */
        onDisplayModeChange: function () {
            var oEditModel = this.getView().getModel("editMode");
            var displayMode = oEditModel.getProperty("/display_mode");

            if (!displayMode) {
                // Display mode cleared - disable detail fields
                oEditModel.setProperty("/canFillDetails", false);
                oEditModel.setProperty("/canEnableRecurring", false);
                return;
            }

            // Step 4: Enable all detail fields (title, message, dates, etc.)
            oEditModel.setProperty("/canFillDetails", true);

            // Check if can enable recurring
            this._checkRecurringEnabledment();
        },

        /**
         * Load valid message types based on current severity
         * @private
         */
        _loadValidMessageTypes: function () {
            var that = this;
            var oEditModel = this.getView().getModel("editMode");
            var severity = oEditModel.getProperty("/severity");

            if (!severity) {
                return;
            }

            jQuery.ajax({
                url: "/sap/bc/rest/zcl_notif_rest/valid-types",
                type: "GET",
                data: {
                    severity: severity
                },
                timeout: 5000,
                success: function (data) {
                    // Store available message types
                    oEditModel.setProperty("/availableMessageTypes", data.message_types || []);

                    // Filter Select items to show only valid values
                    that._filterMessageTypeItems();

                    // Enable message type selection
                    oEditModel.setProperty("/canSelectMessageType", true);

                    // Auto-select default type
                    if (data.default_type) {
                        oEditModel.setProperty("/message_type", data.default_type);
                        // Force Select to update selectedKey after items are filtered
                        var oSelect = that.byId("selectMessageType");
                        if (oSelect) {
                            oSelect.setSelectedKey(data.default_type);
                        }
                        // Trigger next step
                        that.onMessageTypeChange();
                    }
                },
                error: function (xhr, status, error) {
                    console.error("Failed to load valid message types:", error);
                    MessageToast.show("Error loading valid message types");
                    // Fallback to all types
                    oEditModel.setProperty("/availableMessageTypes", ["INFO", "WARNING", "ERROR", "SUCCESS", "URGENT"]);
                    that._filterMessageTypeItems();
                    oEditModel.setProperty("/canSelectMessageType", true);
                }
            });
        },

        /**
         * Load valid display modes based on current severity and message type
         * @private
         */
        _loadValidDisplayModes: function () {
            var that = this;
            var oEditModel = this.getView().getModel("editMode");
            var severity = oEditModel.getProperty("/severity");
            var messageType = oEditModel.getProperty("/message_type");

            // Need both severity and message type
            if (!severity || !messageType) {
                return;
            }

            jQuery.ajax({
                url: "/sap/bc/rest/zcl_notif_rest/valid-modes",
                type: "GET",
                data: {
                    severity: severity,
                    message_type: messageType
                },
                timeout: 5000,
                success: function (data) {
                    // Store available display modes
                    oEditModel.setProperty("/availableDisplayModes", data.display_modes || []);

                    // Filter Select items to show only valid values
                    that._filterDisplayModeItems();

                    // Enable display mode selection
                    oEditModel.setProperty("/canSelectDisplayMode", true);

                    // Auto-select default mode
                    if (data.default_mode) {
                        oEditModel.setProperty("/display_mode", data.default_mode);
                        // Force Select to update selectedKey after items are filtered
                        var oSelect = that.byId("selectDisplayMode");
                        if (oSelect) {
                            oSelect.setSelectedKey(data.default_mode);
                        }
                        // Trigger next step
                        that.onDisplayModeChange();
                    }
                },
                error: function (xhr, status, error) {
                    console.error("Failed to load valid display modes:", error);
                    // Fallback to all modes
                    oEditModel.setProperty("/availableDisplayModes", ["BANNER", "TOAST", "BOTH", "SILENT"]);
                    that._filterDisplayModeItems();
                }
            });
        },

        /**
         * Validate notification combination against matrix
         * @param {object} oData - Notification data
         * @param {function} callback - Callback function(isValid, errorMessage)
         * @private
         */
        _validateMatrixCombination: function(oData, callback) {
            if (!oData.severity || !oData.message_type || !oData.display_mode) {
                callback(false, "Severity, Message Type, and Display Mode are required");
                return;
            }

            jQuery.ajax({
                url: "/sap/bc/rest/zcl_notif_rest/valid-modes",
                type: "GET",
                data: {
                    severity: oData.severity,
                    message_type: oData.message_type
                },
                timeout: 5000,
                success: function (data) {
                    var validModes = data.display_modes || [];
                    if (validModes.indexOf(oData.display_mode) > -1) {
                        // Valid combination
                        callback(true, "");
                    } else {
                        // Invalid combination
                        var errorMsg = "This combination is not allowed. Valid display modes for " +
                                     oData.severity + " / " + oData.message_type + " are: " +
                                     validModes.join(", ");
                        callback(false, errorMsg);
                    }
                },
                error: function (xhr, status, error) {
                    console.error("Failed to validate matrix combination:", error);
                    // On error, allow the save to proceed (fail-open)
                    callback(true, "");
                }
            });
        },

        /**
         * Update recurring preview text
         * @private
         */
        _updateRecurringPreview: function () {
            var oEditModel = this.getView().getModel("editMode");
            var type = oEditModel.getProperty("/recurrenceType");
            var count = oEditModel.getProperty("/occurrences");
            var startDate = oEditModel.getProperty("/start_date");

            if (!type || !count || !startDate) {
                oEditModel.setProperty("/recurringPreview", "");
                return;
            }

            var typeText = type === 'D' ? 'daily' : (type === 'W' ? 'weekly' : 'monthly');
            var preview = "Will create " + count + " " + typeText + " notifications starting from " + startDate;

            oEditModel.setProperty("/recurringPreview", preview);
        },

        /**
         * Load valid message types from matrix and proceed to step 3
         * @private
         */
        _loadValidMessageTypesAndProceed: function () {
            var that = this;
            var oEditModel = this.getView().getModel("editMode");
            var severity = oEditModel.getProperty("/severity");

            if (!severity) {
                return;
            }

            jQuery.ajax({
                url: "/sap/bc/rest/zcl_notif_rest/valid-types",
                type: "GET",
                data: {
                    severity: severity
                },
                timeout: 5000,
                success: function (data) {
                    // Store available message types from matrix
                    oEditModel.setProperty("/availableMessageTypes", data.message_types || []);

                    // Filter Select items to show only valid values
                    that._filterMessageTypeItems();

                    // Auto-select default type from matrix (is_default = X)
                    if (data.default_type) {
                        oEditModel.setProperty("/message_type", data.default_type);
                        // Force Select to update selectedKey after items are filtered
                        var oSelect = that.byId("selectMessageType");
                        if (oSelect) {
                            oSelect.setSelectedKey(data.default_type);
                        }
                    }

                    // Proceed to step 3
                    oEditModel.setProperty("/currentStep", 3);
                    that._updateButtonVisibility();
                },
                error: function (xhr, status, error) {
                    console.error("Failed to load valid message types:", error);
                    MessageBox.error("Error loading valid message types from notification matrix");
                }
            });
        },

        /**
         * Load valid display modes from matrix and proceed to step 4
         * @private
         */
        _loadValidDisplayModesAndProceed: function () {
            var that = this;
            var oEditModel = this.getView().getModel("editMode");
            var severity = oEditModel.getProperty("/severity");
            var messageType = oEditModel.getProperty("/message_type");

            // Need both severity and message type
            if (!severity || !messageType) {
                return;
            }

            jQuery.ajax({
                url: "/sap/bc/rest/zcl_notif_rest/valid-modes",
                type: "GET",
                data: {
                    severity: severity,
                    message_type: messageType
                },
                timeout: 5000,
                success: function (data) {
                    // Store available display modes from matrix
                    oEditModel.setProperty("/availableDisplayModes", data.display_modes || []);

                    // Filter Select items to show only valid values
                    that._filterDisplayModeItems();

                    // Auto-select default mode from matrix (is_default = X)
                    if (data.default_mode) {
                        oEditModel.setProperty("/display_mode", data.default_mode);
                        // Force Select to update selectedKey after items are filtered
                        var oSelect = that.byId("selectDisplayMode");
                        if (oSelect) {
                            oSelect.setSelectedKey(data.default_mode);
                        }
                    }

                    // Proceed to step 4
                    oEditModel.setProperty("/currentStep", 4);
                    that._updateButtonVisibility();
                },
                error: function (xhr, status, error) {
                    console.error("Failed to load valid display modes:", error);
                    MessageBox.error("Error loading valid display modes from notification matrix");
                }
            });
        },

        /**
         * Check if all required fields are filled to enable recurring option
         * @private
         */
        _checkRecurringEnabledment: function () {
            var oEditModel = this.getView().getModel("editMode");
            var title = oEditModel.getProperty("/title");
            var message = oEditModel.getProperty("/message_text");
            var severity = oEditModel.getProperty("/severity");
            var messageType = oEditModel.getProperty("/message_type");
            var displayMode = oEditModel.getProperty("/display_mode");
            var startDate = oEditModel.getProperty("/start_date");
            var endDate = oEditModel.getProperty("/end_date");

            // Enable recurring only if all required fields are filled
            var canEnable = !!(title && message && severity && messageType && displayMode && startDate && endDate);
            oEditModel.setProperty("/canEnableRecurring", canEnable);
        },

        /**
         * Update button visibility based on current wizard state
         * @private
         */
        _updateButtonVisibility: function() {
            var oEditModel = this.getView().getModel("editMode");
            var isCreateMode = oEditModel.getProperty("/isCreateMode");
            var currentStep = oEditModel.getProperty("/currentStep");
            var isViewMode = oEditModel.getProperty("/isViewMode");

            if (isCreateMode) {
                // Wizard mode - show Forward/Back based on step
                oEditModel.setProperty("/showBackButton", currentStep > 1);
                oEditModel.setProperty("/showForwardButton", currentStep < 5);
                oEditModel.setProperty("/showSaveButton", currentStep === 5);
            } else {
                // Edit/Copy/View mode - no wizard, only Save
                oEditModel.setProperty("/showBackButton", false);
                oEditModel.setProperty("/showForwardButton", false);
                oEditModel.setProperty("/showSaveButton", !isViewMode);
            }
        },

        /**
         * Filter message type select items to show only valid values from matrix
         * @private
         */
        _filterMessageTypeItems: function() {
            var oEditModel = this.getView().getModel("editMode");
            var availableTypes = oEditModel.getProperty("/availableMessageTypes") || [];
            var oSelect = this.byId("selectMessageType");
            var oI18n = this.getView().getModel("i18n").getResourceBundle();

            if (!oSelect) {
                return;
            }

            // Remove all existing items
            oSelect.removeAllItems();

            // Add only valid items from matrix
            var itemConfigs = [
                {key: "URGENT", textKey: "messageTypeUrgent"},
                {key: "INFO", textKey: "messageTypeInfo"},
                {key: "TIP", textKey: "messageTypeTip"},
                {key: "WARNING", textKey: "messageTypeWarning"},
                {key: "MAINT", textKey: "messageTypeMaint"}
            ];

            itemConfigs.forEach(function(config) {
                if (availableTypes.indexOf(config.key) > -1) {
                    oSelect.addItem(new sap.ui.core.Item({
                        key: config.key,
                        text: oI18n.getText(config.textKey)
                    }));
                }
            });
        },

        /**
         * Filter display mode select items to show only valid values from matrix
         * @private
         */
        _filterDisplayModeItems: function() {
            var oEditModel = this.getView().getModel("editMode");
            var availableModes = oEditModel.getProperty("/availableDisplayModes") || [];
            var oSelect = this.byId("selectDisplayMode");
            var oI18n = this.getView().getModel("i18n").getResourceBundle();

            if (!oSelect) {
                return;
            }

            // Remove all existing items
            oSelect.removeAllItems();

            // Add only valid items from matrix
            var itemConfigs = [
                {key: "BANNER", textKey: "displayModeBanner"},
                {key: "TOAST", textKey: "displayModeToast"},
                {key: "BOTH", textKey: "displayModeBoth"},
                {key: "SILENT", textKey: "displayModeSilent"}
            ];

            itemConfigs.forEach(function(config) {
                if (availableModes.indexOf(config.key) > -1) {
                    oSelect.addItem(new sap.ui.core.Item({
                        key: config.key,
                        text: oI18n.getText(config.textKey)
                    }));
                }
            });
        }
    });
});
