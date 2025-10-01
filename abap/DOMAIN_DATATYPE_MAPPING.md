# Domain → Data Element → Table Field Mapping

## Complete Type Hierarchy

This document shows the complete mapping from custom domains to data elements to table fields in the SAP Fiori Notification Banner system.

## Structure Overview

```
DOMAIN (SE11) → DATA ELEMENT (SE11) → TABLE FIELD (ZTNOTIFY_MSGS)
     ↓                   ↓                        ↓
Fixed Values      Field Labels              Database Column
  + Type          + Search Help            (uses Data Element)
```

---

## 1. MESSAGE_TYPE Field

### Domain: ZDOMAIN_MSG_TYPE
```
Type: CHAR
Length: 12
Fixed Values:
  - URGENT    | Urgent System Message
  - INFO      | Informational Message
  - TIP       | Helpful Tip
  - SUCCESS   | Success Notification
  - MAINT     | Scheduled Maintenance
  - WARNING   | Warning Message
```

### Data Element: ZNOTIFY_MSG_TYPE
```
Domain: ZDOMAIN_MSG_TYPE
Labels:
  - Short: Msg Type
  - Medium: Message Type
  - Long: Notification Message Type
Search Help: Automatic from domain fixed values
```

### Table Field: ZTNOTIFY_MSGS.MESSAGE_TYPE
```
Data Element: ZNOTIFY_MSG_TYPE
Inherited Type: CHAR 12
F4 Help: ✅ Automatic (shows 6 values)
Validation: ✅ Database-level (only allows fixed values)
```

---

## 2. SEVERITY Field

### Domain: ZDOMAIN_SEVERITY
```
Type: CHAR
Length: 8
Fixed Values:
  - HIGH      | High Priority (Critical/Error)
  - MEDIUM    | Medium Priority (Warning)
  - LOW       | Low Priority (Info)
```

### Data Element: ZNOTIFY_SEVERITY
```
Domain: ZDOMAIN_SEVERITY
Labels:
  - Short: Severity
  - Medium: Severity Level
  - Long: Notification Severity Level
Search Help: Automatic from domain fixed values
```

### Table Field: ZTNOTIFY_MSGS.SEVERITY
```
Data Element: ZNOTIFY_SEVERITY
Inherited Type: CHAR 8
F4 Help: ✅ Automatic (shows 3 values)
Validation: ✅ Database-level
UI Mapping:
  - HIGH   → Red (Error)
  - MEDIUM → Orange (Warning)
  - LOW    → Blue (Information)
```

---

## 3. DISPLAY_MODE Field (v1.1.0)

### Domain: ZDOMAIN_DISPLAY_MODE
```
Type: CHAR
Length: 10
Fixed Values:
  - BANNER    | Fixed Top Banner
  - TOAST     | Toast Notification (5s)
  - BOTH      | Banner + Toast
  - SILENT    | Silent (Log Only)
```

### Data Element: ZNOTIFY_DISP_MODE
```
Domain: ZDOMAIN_DISPLAY_MODE
Labels:
  - Short: Display
  - Medium: Display Mode
  - Long: Notification Display Mode
Search Help: Automatic from domain fixed values
```

### Table Field: ZTNOTIFY_MSGS.DISPLAY_MODE
```
Data Element: ZNOTIFY_DISP_MODE
Inherited Type: CHAR 10
F4 Help: ✅ Automatic (shows 4 values)
Validation: ✅ Database-level
Default: BANNER (set in ABAP code)
```

---

## 4. TARGET_USERS Field

### Domain: ZDOMAIN_TARGET_USERS
```
Type: CHAR
Length: 10
Fixed Values: 3 predefined values (SAP Standard Roles Only)
Values:
  - ALL       | All Users
  - ADMIN     | Administrators (SAP_ALL role - exact match)
  - DEVELOPER | Developers (SAP_BR_DEVELOPER role - exact match)
```

### Data Element: ZNOTIFY_TARGET_USERS
```
Domain: ZDOMAIN_TARGET_USERS
Labels:
  - Short: Target
  - Medium: Target Users
  - Long: Target Audience Filter
Search Help: Automatic from domain fixed values (F4 help enabled)
```

### Table Field: ZTNOTIFY_MSGS.TARGET_USERS
```
Data Element: ZNOTIFY_TARGET_USERS
Inherited Type: CHAR 10
F4 Help: ✅ Automatic dropdown in SM30/SE16 (3 values)
Validation: Domain fixed values + exact role matching via AGR_USERS table
Implementation: zcl_notification_manager=>check_target_audience() method
Role Matching: Exact match only (agr_name = 'SAP_ALL' or 'SAP_BR_DEVELOPER')
Note: No LIKE patterns used for security and performance
```

---

## ABAP Code Type Mapping

### Internal Structure (zcl_notification_manager.clas.abap)

```abap
TYPES: BEGIN OF ty_notification,
         message_id   TYPE string,           " ← Generic string (OK for internal use)
         message_type TYPE string,           " ← Maps to ZNOTIFY_MSG_TYPE in DB
         severity     TYPE string,           " ← Maps to ZNOTIFY_SEVERITY in DB
         title        TYPE string,           " ← CHAR255 in DB
         message_text TYPE string,           " ← DSTRING in DB
         start_date   TYPE dats,             " ← DATS in DB
         end_date     TYPE dats,             " ← DATS in DB
         target_users TYPE string,           " ← Maps to ZNOTIFY_TARGET_USERS in DB
         active       TYPE char1,            " ← CHAR1 in DB
         display_mode TYPE char10,           " ← Maps to ZNOTIFY_DISP_MODE in DB
         created_by   TYPE syuname,          " ← SYUNAME in DB
         created_at   TYPE timestampl,       " ← TIMESTAMPL in DB
         changed_by   TYPE syuname,          " ← SYUNAME in DB
         changed_at   TYPE timestampl,       " ← TIMESTAMPL in DB
       END OF ty_notification.
```

**Why generic types in internal structure?**
- ✅ Flexibility for JSON serialization/deserialization
- ✅ Avoids tight coupling to database types
- ✅ ABAP auto-converts when reading/writing to database
- ✅ Internal structure doesn't need F4 help

### Database Table (ZTNOTIFY_MSGS)

```sql
Field Name       Data Element         Why This Type?
--------------------------------------------------------------------------------
MESSAGE_TYPE     ZNOTIFY_MSG_TYPE     → F4 help + validation (6 fixed values)
SEVERITY         ZNOTIFY_SEVERITY     → F4 help + validation (3 fixed values)
TARGET_USERS     ZNOTIFY_TARGET_USERS → Consistent type definition
DISPLAY_MODE     ZNOTIFY_DISP_MODE    → F4 help + validation (4 fixed values)
```

---

## Type Conversion Flow

### When Creating Notification (POST)

```
1. JSON Request
   ↓
2. Deserialize to ty_notification (internal structure)
   message_type: "URGENT" (TYPE string)
   ↓
3. Map to ztnotify_msgs (database table)
   message_type: "URGENT" (Data Element: ZNOTIFY_MSG_TYPE)
   ↓
4. Domain Validation
   ZDOMAIN_MSG_TYPE checks if "URGENT" is in fixed values
   ✅ Valid → INSERT
   ❌ Invalid → Reject
```

### When Reading Notification (GET)

```
1. SELECT from ztnotify_msgs
   message_type: ZNOTIFY_MSG_TYPE (CHAR 12)
   ↓
2. Convert to ty_notification
   message_type: TYPE string (auto-converted by ABAP)
   ↓
3. Serialize to JSON
   "message_type": "URGENT"
```

---

## Benefits of This Architecture

### 1. **Separation of Concerns**
- **Internal Structure**: Generic types for flexibility
- **Database**: Strict types for data integrity

### 2. **Automatic Validation**
- Domain fixed values enforce correctness
- No manual validation needed in ABAP code
- Works in SE11, SM30, and custom apps

### 3. **Automatic F4 Help**
- SM30: Dropdown automatically available
- Custom Fiori apps: Can use value help
- Web Dynpro: Automatic dropdown

### 4. **Maintainability**
- Add new MESSAGE_TYPE? Just update domain
- Change validation rules? Update domain, not code
- All dependent objects auto-update

### 5. **Self-Documentation**
- Fixed values visible in SE11
- Field labels defined in data elements
- No need to read code to understand values

---

## Testing the Mapping

### Test 1: Domain Validation

```sql
-- Transaction: SE16 or SM30
-- Try to insert invalid value
INSERT INTO ZTNOTIFY_MSGS VALUES (
  message_type = 'INVALID'  -- ❌ Will fail: not in ZDOMAIN_MSG_TYPE
);

INSERT INTO ZTNOTIFY_MSGS VALUES (
  message_type = 'URGENT'   -- ✅ Will succeed: valid fixed value
);
```

### Test 2: F4 Help

```
Transaction: SM30
Table: ZTNOTIFY_MSGS
1. Click on MESSAGE_TYPE field
2. Press F4
3. Expected: Dropdown with 6 values (URGENT, INFO, TIP, SUCCESS, MAINT, WARNING)
```

### Test 3: ABAP Code

```abap
DATA: ls_notification TYPE ztnotify_msgs.

" This works - valid value
ls_notification-message_type = 'URGENT'.
INSERT ztnotify_msgs FROM ls_notification.
" → sy-subrc = 0 ✅

" This fails - invalid value
ls_notification-message_type = 'INVALID'.
INSERT ztnotify_msgs FROM ls_notification.
" → sy-subrc <> 0 ❌ Domain validation error
```

---

## Migration Considerations

### Existing Data

If you already have data in ZTNOTIFY_MSGS:

```abap
" Check for invalid values before adding domains
SELECT message_type, COUNT(*) as cnt
FROM ztnotify_msgs
GROUP BY message_type
HAVING message_type NOT IN ('URGENT', 'INFO', 'TIP', 'SUCCESS', 'MAINT', 'WARNING').
```

If found, update them:
```abap
UPDATE ztnotify_msgs
SET message_type = 'INFO'
WHERE message_type NOT IN ('URGENT', 'INFO', 'TIP', 'SUCCESS', 'MAINT', 'WARNING').
```

Then apply the domain changes via SE14 table adjustment.

---

## Summary Table

| Field | Domain | Data Element | Type | Length | Fixed Values | F4 Help |
|-------|--------|--------------|------|--------|--------------|---------|
| MESSAGE_TYPE | ZDOMAIN_MSG_TYPE | ZNOTIFY_MSG_TYPE | CHAR | 12 | 6 values | ✅ Yes |
| SEVERITY | ZDOMAIN_SEVERITY | ZNOTIFY_SEVERITY | CHAR | 8 | 3 values | ✅ Yes |
| DISPLAY_MODE | ZDOMAIN_DISPLAY_MODE | ZNOTIFY_DISP_MODE | CHAR | 10 | 4 values | ✅ Yes |
| TARGET_USERS | ZDOMAIN_TARGET_USERS | ZNOTIFY_TARGET_USERS | CHAR | 255 | None | ⚠️ Optional |

---

**Version**: v1.1.0
**Last Updated**: 2025-01-30
