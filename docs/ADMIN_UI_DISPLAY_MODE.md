# Admin UI - Display Mode Selector Implementation Guide

## 📋 Table of Contents

1. [Overview](#overview)
2. [Display Modes](#display-modes)
3. [Option 1: SM30 Table Maintenance (Simplest)](#option-1-sm30-table-maintenance-simplest)
4. [Option 2: Custom SAP Fiori Admin App](#option-2-custom-sap-fiori-admin-app)
5. [Option 3: Web Dynpro ABAP](#option-3-web-dynpro-abap)
6. [Recommendations by Message Type](#recommendations-by-message-type)
7. [Preview Functionality (Optional Enhancement)](#preview-functionality-optional-enhancement)
8. [Validation Rules](#validation-rules)
9. [REST API Usage](#rest-api-usage)
10. [Migration from v1.0.0](#migration-from-v100)
11. [Testing Checklist](#testing-checklist)
12. [See Also](#see-also)

---

## Overview

This guide explains how to add the **Display Mode** selector to your notification admin application (e.g., SM30, custom Fiori app, or Web Dynpro).

With v1.1.0, administrators can choose how each notification is displayed to users.

## Display Modes

| Mode | Description | Use Case |
|------|-------------|----------|
| **BANNER** | Fixed top banner, user must close | Critical messages requiring acknowledgment |
| **TOAST** | Bottom-right toast, auto-dismiss 5s | Non-intrusive updates, confirmations |
| **BOTH** | Shows both banner and toast | Important announcements needing maximum visibility |
| **SILENT** | No UI display, logged only | Audit trail, compliance, background monitoring |

## Option 1: SM30 Table Maintenance (Simplest)

If using SM30 for table maintenance, the F4 help is **automatic** thanks to the custom domain:

### Steps:
1. Transaction: **SM30**
2. Table: **ZTNOTIFY_MSGS**
3. Click **Maintain**
4. On any row, navigate to **DISPLAY_MODE** field
5. Press **F4** → Dropdown shows: BANNER, TOAST, BOTH, SILENT
6. Select desired mode
7. Save

**Note**: F4 help is automatically provided by `ZDOMAIN_DISPLAY_MODE` fixed values.

## Option 2: Custom SAP Fiori Admin App

If you're building a custom Fiori admin app, add a `sap.m.Select` control with the display modes.

### View XML (Fragment or View)

```xml
<VBox>
    <Label text="{i18n>fieldDisplayMode}" labelFor="displayModeSelect" required="true"/>
    <Select
        id="displayModeSelect"
        selectedKey="{notificationModel>/display_mode}"
        items="{
            path: 'displayModes>/',
            sorter: { path: 'key' }
        }"
        change="onDisplayModeChange">
        <core:Item key="{displayModes>key}" text="{displayModes>text}"/>
    </Select>
    <Text
        text="{displayModes>description}"
        class="sapUiTinyMarginTop sapUiSmallMarginBottom"
        textAlign="Begin"/>
</VBox>
```

### Controller JavaScript

```javascript
onInit: function() {
    // Create display modes model
    var displayModesData = [
        {
            key: "BANNER",
            text: this.getText("displayModeBanner"),
            description: this.getText("displayModeBannerDesc")
        },
        {
            key: "TOAST",
            text: this.getText("displayModeToast"),
            description: this.getText("displayModeToastDesc")
        },
        {
            key: "BOTH",
            text: this.getText("displayModeBoth"),
            description: this.getText("displayModeBothDesc")
        },
        {
            key: "SILENT",
            text: this.getText("displayModeSilent"),
            description: this.getText("displayModeSilentDesc")
        }
    ];

    var displayModesModel = new JSONModel(displayModesData);
    this.getView().setModel(displayModesModel, "displayModes");

    // Set default value for new notifications
    var notificationModel = this.getView().getModel("notificationModel");
    if (!notificationModel.getProperty("/display_mode")) {
        notificationModel.setProperty("/display_mode", "BANNER");
    }
},

onDisplayModeChange: function(oEvent) {
    var selectedKey = oEvent.getParameter("selectedItem").getKey();

    // Update model
    this.getView().getModel("notificationModel").setProperty("/display_mode", selectedKey);

    // Optional: Show preview
    this._updatePreview();
},

getText: function(sKey) {
    return this.getView().getModel("i18n").getResourceBundle().getText(sKey);
}
```

### i18n Keys (already added in v1.1.0)

```properties
# Display mode labels
displayModeBanner=Banner
displayModeToast=Toast Notification
displayModeBoth=Banner + Toast
displayModeSilent=Silent (Log Only)

# Display mode descriptions
displayModeBannerDesc=Fixed banner at top, user must close
displayModeToastDesc=Bottom-right toast, auto-dismiss 5s
displayModeBothDesc=Shows both banner and toast
displayModeSilentDesc=No UI, logged only

# Field label
fieldDisplayMode=Display Mode
```

## Option 3: Web Dynpro ABAP

If using Web Dynpro for admin UI:

### Context Attribute
- **Attribute Name**: `DISPLAY_MODE`
- **Type**: `ZNOTIFY_DISP_MODE`
- **Input Help**: Automatic (from domain fixed values)

### Dropdown by Key (DDLB)
```abap
METHOD wddoinit.
  DATA: lt_values TYPE wdr_context_attr_value_list,
        ls_value TYPE wdr_context_attr_value.

  " BANNER
  ls_value-value = 'BANNER'.
  ls_value-text = 'Banner'.
  APPEND ls_value TO lt_values.

  " TOAST
  ls_value-value = 'TOAST'.
  ls_value-text = 'Toast Notification'.
  APPEND ls_value TO lt_values.

  " BOTH
  ls_value-value = 'BOTH'.
  ls_value-text = 'Banner + Toast'.
  APPEND ls_value TO lt_values.

  " SILENT
  ls_value-value = 'SILENT'.
  ls_value-text = 'Silent (Log Only)'.
  APPEND ls_value TO lt_values.

  " Bind to context attribute
  wd_context->node_notification->get_attribute( 'DISPLAY_MODE' )->set_attribute_value_set( lt_values ).
ENDMETHOD.
```

## Recommendations by Message Type

Guide administrators with these recommendations:

| Message Type | Severity | Recommended Display Mode |
|--------------|----------|--------------------------|
| URGENT | HIGH | BANNER or BOTH |
| WARNING | MEDIUM | BANNER or TOAST |
| MAINT | MEDIUM | TOAST |
| INFO | LOW | TOAST |
| TIP | LOW | TOAST |
| SUCCESS | LOW | TOAST or SILENT |

## Preview Functionality (Optional Enhancement)

Add a preview button that shows how the notification will appear:

```javascript
onPreview: function() {
    var notification = this.getView().getModel("notificationModel").getData();
    var displayMode = notification.display_mode || "BANNER";

    switch(displayMode.toUpperCase()) {
        case "BANNER":
            this._showPreviewBanner(notification);
            break;
        case "TOAST":
            this._showPreviewToast(notification);
            break;
        case "BOTH":
            this._showPreviewBanner(notification);
            this._showPreviewToast(notification);
            break;
        case "SILENT":
            MessageBox.information("This notification will not be displayed in the UI. It will only be logged.");
            break;
    }
},

_showPreviewBanner: function(notification) {
    var messageType = this._getMessageType(notification.severity);

    MessageBox.show(notification.title + ": " + notification.message_text, {
        icon: messageType,
        title: this.getText("messagePreviewTitle"),
        actions: [MessageBox.Action.CLOSE]
    });
},

_showPreviewToast: function(notification) {
    MessageToast.show(notification.title + ": " + notification.message_text, {
        duration: 5000,
        width: "25em"
    });
},

_getMessageType: function(severity) {
    switch(severity.toUpperCase()) {
        case "HIGH": return MessageBox.Icon.ERROR;
        case "MEDIUM": return MessageBox.Icon.WARNING;
        case "LOW": return MessageBox.Icon.INFORMATION;
        default: return MessageBox.Icon.INFORMATION;
    }
}
```

## Validation Rules

Implement these validation rules in your admin app:

```javascript
validateNotification: function(notification) {
    var errors = [];

    // Required fields
    if (!notification.title) {
        errors.push("Title is required");
    }
    if (!notification.message_text) {
        errors.push("Message text is required");
    }
    if (!notification.severity) {
        errors.push("Severity is required");
    }
    if (!notification.message_type) {
        errors.push("Message type is required");
    }

    // Display mode validation
    if (!notification.display_mode) {
        errors.push("Display mode is required");
    } else {
        var validModes = ["BANNER", "TOAST", "BOTH", "SILENT"];
        if (validModes.indexOf(notification.display_mode.toUpperCase()) === -1) {
            errors.push("Invalid display mode. Must be: BANNER, TOAST, BOTH, or SILENT");
        }
    }

    // Date validation
    if (notification.start_date && notification.end_date) {
        if (notification.end_date < notification.start_date) {
            errors.push("End date must be after start date");
        }
    }

    // Target users validation (optional)
    if (notification.target_users && notification.target_users.length > 255) {
        errors.push("Target users exceeds maximum length (255 characters)");
    }

    return errors;
}
```

## REST API Usage

When creating/updating notifications via REST API, include the `display_mode` field:

```javascript
// Create notification via REST
jQuery.ajax({
    url: "/sap/bc/rest/zcl_notification_rest/",
    type: "POST",
    contentType: "application/json",
    data: JSON.stringify({
        title: "System Maintenance",
        message_text: "Scheduled maintenance on Sunday 2AM-4AM",
        message_type: "MAINT",
        severity: "MEDIUM",
        start_date: "20250401",
        end_date: "20250430",
        target_users: "ALL",
        active: "X",
        display_mode: "TOAST"  // ← v1.1.0 field
    }),
    success: function(response) {
        MessageToast.show("Notification created successfully");
    }
});
```

## Migration from v1.0.0

Existing notifications without `display_mode` will:
1. Be automatically set to `"BANNER"` by the backend (default value)
2. Continue working exactly as before
3. Can be updated to use new display modes at any time

Run the migration script to update existing records:
```abap
REPORT z_migrate_notify_v11.
* See abap/z_migrate_notify_v11.abap for full script
```

## Testing Checklist

Test all display modes in admin UI:

- [ ] BANNER mode shows fixed top banner
- [ ] TOAST mode shows bottom-right toast (5s auto-dismiss)
- [ ] BOTH mode shows both banner and toast
- [ ] SILENT mode has no UI display (check logs)
- [ ] F4 help works in SM30
- [ ] F4 help works in custom Fiori app (if applicable)
- [ ] Default value is BANNER for new notifications
- [ ] Validation rejects invalid display mode values
- [ ] Preview functionality works (if implemented)

## See Also

- [Enhancement Plan](../ENHANCEMENT_PLAN.md) - Full v1.1.0 feature specification
- [Custom Domains README](../abap/domains/README.md) - Domain and data element details
- [i18n Properties](../webapp/i18n/i18n.properties) - All text keys for v1.1.0
