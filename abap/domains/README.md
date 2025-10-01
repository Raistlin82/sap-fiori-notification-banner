# Custom Domains and Data Elements

## Overview

This directory contains custom Z domains and data elements for the notification system. Using custom domains with **fixed value ranges** provides automatic validation and F4 help.

## Benefits of Custom Domains with Fixed Values

### 1. **Automatic Input Validation**
- Values are validated at **database level** (not just UI)
- Invalid values are rejected automatically in SE11, SM30, and via ABAP code
- No need for manual validation logic in every program

### 2. **Automatic F4 Help**
- Users get dropdown list of valid values automatically
- No need to create separate search helps
- Consistent UX across all transactions (SE16, SM30, custom apps)

### 3. **Data Integrity**
- Prevents typos (e.g., "HIHG" instead of "HIGH")
- Ensures consistent casing (HIGH vs high vs High)
- Database-level constraint enforcement

### 4. **Documentation**
- Fixed values are self-documenting
- Visible in SE11 domain definition
- Easier for new developers to understand valid values

### 5. **Maintenance**
- Adding new values requires domain change + transport
- All dependent tables automatically get new values
- Change history tracked in transport system

## Structure

```
domains/
├── zdomain_msg_type.se11       → Domain for MESSAGE_TYPE
├── zdomain_severity.se11       → Domain for SEVERITY
├── zdomain_display_mode.se11   → Domain for DISPLAY_MODE (v1.1.0)
└── zdomain_target_users.se11   → Domain for TARGET_USERS

data_elements/
├── znotify_msg_type.se11       → Data Element based on ZDOMAIN_MSG_TYPE
├── znotify_severity.se11       → Data Element based on ZDOMAIN_SEVERITY
├── znotify_disp_mode.se11      → Data Element based on ZDOMAIN_DISPLAY_MODE
└── znotify_target_users.se11   → Data Element based on ZDOMAIN_TARGET_USERS
```

## Creation Order (IMPORTANT!)

**You must create objects in this order:**

1. **Domains** (SE11 → Domain)
   - ZDOMAIN_MSG_TYPE
   - ZDOMAIN_SEVERITY
   - ZDOMAIN_DISPLAY_MODE
   - ZDOMAIN_TARGET_USERS

2. **Data Elements** (SE11 → Data Type)
   - ZNOTIFY_MSG_TYPE (references ZDOMAIN_MSG_TYPE)
   - ZNOTIFY_SEVERITY (references ZDOMAIN_SEVERITY)
   - ZNOTIFY_DISP_MODE (references ZDOMAIN_DISPLAY_MODE)
   - ZNOTIFY_TARGET_USERS (references ZDOMAIN_TARGET_USERS)

3. **Table** (SE11 → Database Table)
   - ZTNOTIFY_MSGS (uses the data elements above)

## Fixed Values

### MESSAGE_TYPE (ZDOMAIN_MSG_TYPE)
- `URGENT` - Urgent System Message
- `INFO` - Informational Message
- `TIP` - Helpful Tip
- `SUCCESS` - Success Notification
- `MAINT` - Scheduled Maintenance
- `WARNING` - Warning Message

### SEVERITY (ZDOMAIN_SEVERITY)
- `HIGH` - High Priority (Critical/Error) → Red UI
- `MEDIUM` - Medium Priority (Warning) → Orange UI
- `LOW` - Low Priority (Info) → Blue UI

### DISPLAY_MODE (ZDOMAIN_DISPLAY_MODE) - v1.1.0
- `BANNER` - Fixed Top Banner (default)
- `TOAST` - Toast Notification (5s auto-dismiss)
- `BOTH` - Banner + Toast simultaneously
- `SILENT` - Silent (Log Only, no UI)

### TARGET_USERS (ZDOMAIN_TARGET_USERS)
**Role-based fixed values** (CHAR 10) with F4 help - SAP Standard Roles Only

Fixed Values:
- `ALL` - All Users
- `ADMIN` - Administrators (SAP_ALL role - exact match)
- `DEVELOPER` - Developers (SAP_BR_DEVELOPER role - exact match)

**Implementation**:
- Role-based authorization via AGR_USERS table (PFCG integration)
- Uses exact role name matching (no LIKE patterns)
- SAP standard roles only (no custom Z_* roles)

**Business Logic**: See `zcl_notification_manager=>check_target_audience()` method

## Testing F4 Help

After creating domains and data elements, test the F4 help:

1. **SM30**: Table Maintenance
   ```
   Transaction: SM30
   Table: ZTNOTIFY_MSGS
   Click on MESSAGE_TYPE field
   Press F4 → Should show dropdown with fixed values
   ```

2. **SE16**: Data Browser
   ```
   Transaction: SE16
   Table: ZTNOTIFY_MSGS
   Selection Screen → MESSAGE_TYPE field
   Press F4 → Should show fixed values
   ```

3. **ABAP Code**: Validation
   ```abap
   DATA ls_notification TYPE ztnotify_msgs.

   ls_notification-message_type = 'INVALID'. " ❌ Will fail validation
   ls_notification-message_type = 'URGENT'.  " ✅ Valid

   INSERT ztnotify_msgs FROM ls_notification.
   " First one raises error, second succeeds
   ```

## Migration Considerations

If you already have data in ZTNOTIFY_MSGS table with `CHAR10` fields:

1. **Before changing to custom domains**:
   - Export existing data (SE16 → Download)
   - Verify all existing values match the new fixed values
   - Update any non-conforming values

2. **Apply changes**:
   - Adjust table structure (SE14)
   - Test that existing data still loads correctly

3. **Run migration script**:
   - Execute `z_migrate_notify_v11.abap` to set DISPLAY_MODE defaults

## Version History

- **v1.0.0**: Initial domains for MESSAGE_TYPE and SEVERITY
- **v1.1.0**: Added DISPLAY_MODE domain for toast notifications
