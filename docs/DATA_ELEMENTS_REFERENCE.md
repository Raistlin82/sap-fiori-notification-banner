# Data Elements and Data Types Reference

**Version**: 1.1.0
**Last Updated**: January 30, 2025

This document lists ALL data elements and data types used in the SAP Fiori Global Notification Banner application, distinguishing between SAP standard elements (pre-existing) and custom elements (must be created).

---

## Table of Contents

1. [SAP Standard Data Elements](#sap-standard-data-elements)
2. [Custom Data Elements](#custom-data-elements)
3. [Field Mapping in ZTNOTIFY_MSGS Table](#field-mapping-in-ztnotify_msgs-table)
4. [Verification Steps](#verification-steps)

---

## SAP Standard Data Elements

These are **pre-existing SAP standard data elements**. They do NOT need to be created - they already exist in all SAP systems.

### MANDT (Client)

**Data Element**: MANDT
**Domain**: MANDT
**Type**: CLNT, Length: 3
**Purpose**: Client field for multi-client systems
**SAP Note**: Automatically added as first key field in all application tables
**Used In**: ZTNOTIFY_MSGS.CLIENT

---

### CHAR32 (Character String Length 32)

**Data Element**: CHAR32
**Domain**: CHAR32
**Type**: CHAR, Length: 32
**Purpose**: Generic 32-character string (used for UUIDs)
**SAP Note**: Pre-defined ABAP Dictionary type
**Used In**: ZTNOTIFY_MSGS.MESSAGE_ID

---

### CHAR255 (Character String Length 255)

**Data Element**: CHAR255
**Domain**: CHAR255
**Type**: CHAR, Length: 255
**Purpose**: Generic 255-character string
**SAP Note**: Pre-defined ABAP Dictionary type
**Used In**: ZTNOTIFY_MSGS.TITLE

---

### CHAR1 (Character String Length 1)

**Data Element**: CHAR1
**Domain**: CHAR1
**Type**: CHAR, Length: 1
**Purpose**: Single character field (flags, switches)
**SAP Note**: Pre-defined ABAP Dictionary type
**Used In**: ZTNOTIFY_MSGS.ACTIVE

**Common Values**:
- `X` = Active/True
- ` ` (space) = Inactive/False

---

### DSTRING (Dynamic String)

**Data Element**: DSTRING
**Domain**: DSTRING
**Type**: STRING, Length: 0 (dynamic)
**Purpose**: Variable-length string without fixed limit
**SAP Note**: Built-in ABAP Dictionary type for long texts
**Used In**: ZTNOTIFY_MSGS.MESSAGE_TEXT

**Characteristics**:
- No fixed length limit
- Efficiently stored in database (only uses required space)
- Automatically handled by ABAP runtime
- Ideal for long message texts
- Recommended max: 2000 characters for UI performance

---

### DATS (Date Field)

**Data Element**: DATS
**Domain**: DATS
**Type**: DATS, Length: 8
**Purpose**: Date storage in YYYYMMDD format
**SAP Note**: Standard SAP date type
**Used In**: ZTNOTIFY_MSGS.START_DATE, END_DATE

**Format**: YYYYMMDD (e.g., 20250130)

---

### SYUNAME (User Name)

**Data Element**: SYUNAME
**Domain**: XUBNAME
**Type**: CHAR, Length: 12
**Purpose**: SAP user ID
**SAP Note**: Standard system field for user names
**Used In**: ZTNOTIFY_MSGS.CREATED_BY, CHANGED_BY

**Characteristics**:
- Uppercase only
- Maximum 12 characters
- Same as SY-UNAME system field

---

### TIMESTAMPL (Long Timestamp)

**Data Element**: TIMESTAMPL
**Domain**: TIMESTAMPL
**Type**: DEC, Length: 21, Decimals: 7
**Purpose**: Precise timestamp with microseconds
**SAP Note**: SAP standard timestamp type (UTC)
**Used In**: ZTNOTIFY_MSGS.CREATED_AT, CHANGED_AT

**Format**: YYYYMMDDHHMMSSmmmuuun (21 digits, 7 decimal places)
**Example**: 20250130143045123456 (2025-01-30 14:30:45.123456)

---

## Custom Data Elements

These are **custom data elements** that MUST be created following the deployment guide.

**Creation Steps**: See DEPLOYMENT_GUIDE.md Step 2: Create Data Elements

### ZNOTIFY_MSG_TYPE

**Data Element**: ZNOTIFY_MSG_TYPE
**Domain**: ZDOMAIN_MSG_TYPE (custom)
**Type**: CHAR, Length: 12
**Purpose**: Notification message type classification
**Creation Required**: YES - See DEPLOYMENT_GUIDE.md Step 2
**Used In**: ZTNOTIFY_MSGS.MESSAGE_TYPE

**Field Labels**:
```
Short:   Msg Type
Medium:  Message Type
Long:    Notification Message Type
Heading: Msg Type
```

**Fixed Values** (from domain):
- `URGENT` - Urgent System Message
- `INFO` - Informational Message
- `TIP` - Helpful Tip
- `SUCCESS` - Success Notification
- `MAINT` - Scheduled Maintenance
- `WARNING` - Warning Message

**F4 Help**: Automatic dropdown from ZDOMAIN_MSG_TYPE

---

### ZNOTIFY_SEVERITY

**Data Element**: ZNOTIFY_SEVERITY
**Domain**: ZDOMAIN_SEVERITY (custom)
**Type**: CHAR, Length: 8
**Purpose**: Notification severity/priority level
**Creation Required**: YES - See DEPLOYMENT_GUIDE.md Step 2
**Used In**: ZTNOTIFY_MSGS.SEVERITY

**Field Labels**:
```
Short:   Severity
Medium:  Severity Level
Long:    Notification Severity Level
Heading: Severity
```

**Fixed Values** (from domain):
- `HIGH` - High Priority (Critical/Error)
- `MEDIUM` - Medium Priority (Warning)
- `LOW` - Low Priority (Info)

**F4 Help**: Automatic dropdown from ZDOMAIN_SEVERITY

**UI Color Mapping**:
- HIGH → Red
- MEDIUM → Orange
- LOW → Blue/Green

---

### ZNOTIFY_DISP_MODE

**Data Element**: ZNOTIFY_DISP_MODE
**Domain**: ZDOMAIN_DISPLAY_MODE (custom)
**Type**: CHAR, Length: 10
**Purpose**: Notification display mode control
**Creation Required**: YES - See DEPLOYMENT_GUIDE.md Step 2
**Used In**: ZTNOTIFY_MSGS.DISPLAY_MODE

**Field Labels**:
```
Short:   Display
Medium:  Display Mode
Long:    Notification Display Mode
Heading: Display
```

**Fixed Values** (from domain):
- `BANNER` - Fixed Top Banner
- `TOAST` - Toast Notification (5s auto-dismiss)
- `BOTH` - Banner + Toast simultaneously
- `SILENT` - Silent (Log Only, no UI display)

**F4 Help**: Automatic dropdown from ZDOMAIN_DISPLAY_MODE

**Default Value**: BANNER (for backward compatibility)

---

### ZNOTIFY_TARGET_USERS

**Data Element**: ZNOTIFY_TARGET_USERS
**Domain**: ZDOMAIN_TARGET_USERS (custom)
**Type**: CHAR, Length: 255
**Purpose**: Target audience filtering pattern
**Creation Required**: YES - See DEPLOYMENT_GUIDE.md Step 2
**Used In**: ZTNOTIFY_MSGS.TARGET_USERS

**Field Labels**:
```
Short:   Target
Medium:  Target Users
Long:    Target Audience Filter
Heading: Target
```

**No Fixed Values** - Free text field with pattern matching

**Supported Patterns**:
- `ALL` - All users
- `USER:username` - Specific user (e.g., USER:SMITHJ)
- `ROLE:rolename` - Users with role (e.g., ROLE:SAP_ALL)
- `DEPT:deptcode` - Department members (e.g., DEPT:FIN)
- Comma-separated: `ROLE:SAP_ALL,DEPT:IT,USER:ADMIN` (OR logic)

---

## Field Mapping in ZTNOTIFY_MSGS Table

Complete mapping of all fields in ZTNOTIFY_MSGS table:

| Field Name       | Data Element         | Type    | Length | Decimals | Origin     | F4 Help |
|------------------|----------------------|---------|--------|----------|------------|---------|
| CLIENT           | MANDT                | CLNT    | 3      | 0        | SAP Std    | No      |
| MESSAGE_ID       | CHAR32               | CHAR    | 32     | 0        | SAP Std    | No      |
| MESSAGE_TYPE     | ZNOTIFY_MSG_TYPE     | CHAR    | 12     | 0        | Custom     | ✅ Yes  |
| SEVERITY         | ZNOTIFY_SEVERITY     | CHAR    | 8      | 0        | Custom     | ✅ Yes  |
| TITLE            | CHAR255              | CHAR    | 255    | 0        | SAP Std    | No      |
| MESSAGE_TEXT     | DSTRING              | STRING  | 0      | 0        | SAP Std    | No      |
| START_DATE       | DATS                 | DATS    | 8      | 0        | SAP Std    | No      |
| END_DATE         | DATS                 | DATS    | 8      | 0        | SAP Std    | No      |
| TARGET_USERS     | ZNOTIFY_TARGET_USERS | CHAR    | 255    | 0        | Custom     | No      |
| ACTIVE           | CHAR1                | CHAR    | 1      | 0        | SAP Std    | No      |
| DISPLAY_MODE     | ZNOTIFY_DISP_MODE    | CHAR    | 10     | 0        | Custom     | ✅ Yes  |
| CREATED_BY       | SYUNAME              | CHAR    | 12     | 0        | SAP Std    | No      |
| CREATED_AT       | TIMESTAMPL           | DEC     | 21     | 7        | SAP Std    | No      |
| CHANGED_BY       | SYUNAME              | CHAR    | 12     | 0        | SAP Std    | No      |
| CHANGED_AT       | TIMESTAMPL           | DEC     | 21     | 7        | SAP Std    | No      |

**Summary**:
- **Total Fields**: 15
- **SAP Standard Data Elements**: 11 (73%)
- **Custom Data Elements**: 4 (27%)
- **F4 Help Enabled**: 3 fields (MESSAGE_TYPE, SEVERITY, DISPLAY_MODE)

---

## Verification Steps

### Step 1: Verify SAP Standard Data Elements Exist

**Transaction**: SE11

```
1. SE11 → Data Type → Enter "MANDT" → Display
   Expected: ✅ Active, Type CLNT, Length 3

2. SE11 → Data Type → Enter "CHAR32" → Display
   Expected: ✅ Active, Type CHAR, Length 32

3. SE11 → Data Type → Enter "CHAR255" → Display
   Expected: ✅ Active, Type CHAR, Length 255

4. SE11 → Data Type → Enter "CHAR1" → Display
   Expected: ✅ Active, Type CHAR, Length 1

5. SE11 → Data Type → Enter "DSTRING" → Display
   Expected: ✅ Active, Type STRING

6. SE11 → Data Type → Enter "DATS" → Display
   Expected: ✅ Active, Type DATS, Length 8

7. SE11 → Data Type → Enter "SYUNAME" → Display
   Expected: ✅ Active, Type CHAR, Length 12, Domain XUBNAME

8. SE11 → Data Type → Enter "TIMESTAMPL" → Display
   Expected: ✅ Active, Type DEC, Length 21, Decimals 7
```

**Result**: All SAP standard data elements should exist and be active.

---

### Step 2: Verify Custom Data Elements Created

**Transaction**: SE11

```
1. SE11 → Data Type → Enter "ZNOTIFY_MSG_TYPE" → Display
   Expected: ✅ Active, Domain ZDOMAIN_MSG_TYPE
   Field Labels: Msg Type, Message Type, Notification Message Type

2. SE11 → Data Type → Enter "ZNOTIFY_SEVERITY" → Display
   Expected: ✅ Active, Domain ZDOMAIN_SEVERITY
   Field Labels: Severity, Severity Level, Notification Severity Level

3. SE11 → Data Type → Enter "ZNOTIFY_DISP_MODE" → Display
   Expected: ✅ Active, Domain ZDOMAIN_DISPLAY_MODE
   Field Labels: Display, Display Mode, Notification Display Mode

4. SE11 → Data Type → Enter "ZNOTIFY_TARGET_USERS" → Display
   Expected: ✅ Active, Domain ZDOMAIN_TARGET_USERS
   Field Labels: Target, Target Users, Target Audience Filter
```

**Result**: All 4 custom data elements should exist and be active.

---

### Step 3: Verify F4 Help Works

**Transaction**: SM30

```
1. SM30 → Table/View: ZTNOTIFY_MSGS → Maintain
2. Click "New Entries"
3. Position cursor on MESSAGE_TYPE field → Press F4
   Expected: ✅ Dropdown with 6 values (URGENT, INFO, TIP, SUCCESS, MAINT, WARNING)

4. Position cursor on SEVERITY field → Press F4
   Expected: ✅ Dropdown with 3 values (HIGH, MEDIUM, LOW)

5. Position cursor on DISPLAY_MODE field → Press F4
   Expected: ✅ Dropdown with 4 values (BANNER, TOAST, BOTH, SILENT)

6. Position cursor on TARGET_USERS field → Press F4
   Expected: ❌ No dropdown (free text field, no fixed values)
```

**Result**: F4 help should work for MESSAGE_TYPE, SEVERITY, and DISPLAY_MODE.

---

### Step 4: Verify Data Element Usage in Table

**Transaction**: SE11

```
1. SE11 → Database Table → ZTNOTIFY_MSGS → Display
2. Click "Fields" tab
3. For each field, check "Data element" column matches the mapping table above
4. Expected: All 15 fields use correct data elements
```

**Result**: Table should use exactly the data elements listed in the mapping table.

---

### Step 5: Test Data Entry

**Transaction**: SM30

```
1. SM30 → ZTNOTIFY_MSGS → Create new entry
2. Fill in all fields:
   MESSAGE_TYPE: URGENT (select from F4)
   SEVERITY: HIGH (select from F4)
   TITLE: Test Notification (free text, max 255 chars)
   MESSAGE_TEXT: Long message text (free text, unlimited)
   START_DATE: 20250101 (date picker)
   END_DATE: 20251231 (date picker)
   TARGET_USERS: ALL (free text)
   ACTIVE: X (checkbox)
   DISPLAY_MODE: BANNER (select from F4)

3. Save entry
4. Expected: ✅ Entry saved successfully
5. SE16 → ZTNOTIFY_MSGS → Display
   Expected: ✅ Entry visible with correct values
   CREATED_BY: [Your username]
   CREATED_AT: [Current timestamp]
```

**Result**: All data elements should work correctly for data entry and storage.

---

## Related Documentation

- **Deployment Guide**: [DEPLOYMENT_GUIDE.md](./DEPLOYMENT_GUIDE.md)
  - Step 1: Create Custom Domains
  - Step 2: Create Data Elements
  - Step 3: Create Database Table

- **Domain Reference**: [domains/README.md](../abap/domains/README.md)
  - ZDOMAIN_MSG_TYPE
  - ZDOMAIN_SEVERITY
  - ZDOMAIN_DISPLAY_MODE
  - ZDOMAIN_TARGET_USERS

- **Type Hierarchy**: [DOMAIN_DATATYPE_MAPPING.md](../abap/DOMAIN_DATATYPE_MAPPING.md)
  - Domain → Data Element → Table Field mapping
  - Internal types vs. database types

---

**Last Updated**: January 30, 2025
**Version**: 1.1.0
