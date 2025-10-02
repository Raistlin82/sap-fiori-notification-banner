# 🚀 Complete Deployment Guide - SAP Fiori Global Notification Banner

**Version**: 1.1.1
**Last Updated**: January 2025

---

## 🆕 Latest Updates (v1.1.1)

**Application Name Changed**: `ZNOTIFY_BANNER` → `ZNOTIFY_BANNER2`
**Namespace**: `com.sap.notifications.banner2`

**Critical Fixes Applied**:
- ✅ Page height CSS issue resolved (inline styles added)
- ✅ Mockserver error handling fixed (graceful fallback)
- ✅ BSP structure confirmed correct per SAP official documentation

📄 **See**: [`docs/RELEASE_NOTES.md`](./RELEASE_NOTES.md) for complete change log.

---

## 🏗️ Architecture: Single App Design

**CRITICAL**: This solution uses **ONE SINGLE SAP Fiori Application** (`ZNOTIFY_BANNER2`) with dual functionality:

1. **Admin Interface** (Visible Dynamic Tile):
   - CRUD operations for notification management
   - Visible only to admin users (role: `Z_NOTIF_ADMIN` or `SAP_ALL`)
   - One dynamic tile showing live statistics

2. **Global Notification Banner** (Background Component):
   - Displays notifications to ALL users in Fiori Launchpad
   - Loads automatically via hidden tile or FLP plugin configuration
   - No separate application or tile needed

**Key Points**:
- ✅ **Single BSP Application**: ZNOTIFY_BANNER2
- ✅ **Single Dynamic Tile**: "System Notifications" (for admins only)
- ✅ **No multiple apps**: Admin UI + Global Banner are in ONE app
- ✅ **Deployment**: Deploy once, configure FLP for admin tile + global banner activation

---

## 📋 Table of Contents
1. [Pre-Deployment Verification](#pre-deployment-verification)
2. [Prerequisites](#prerequisites)
3. [System Requirements](#system-requirements)
4. [Backend Deployment (ABAP)](#backend-deployment-abap)
   - [Step 1: Create Custom Domains](#step-1-create-custom-domains)
   - [Step 2: Create Data Elements](#step-2-create-data-elements)
   - [Step 3: Create Database Table](#step-3-create-database-table)
   - [Step 4: Create CDS View](#step-4-create-cds-view)
   - [Step 5: Create ABAP Classes](#step-5-create-abap-classes)
   - [Step 6: Configure REST Service](#step-6-configure-rest-service)
   - [Step 7: Create Authorization Object (Optional)](#step-7-create-authorization-object-optional)
5. [Frontend Deployment (UI5)](#frontend-deployment-ui5)
6. [Fiori Launchpad Configuration](#fiori-launchpad-configuration)
7. [Testing](#testing)
8. [Troubleshooting](#troubleshooting)
9. [Maintenance](#maintenance)

---

## ✅ Pre-Deployment Verification

Before starting the deployment process, verify that the project is ready for production.

### Build Verification
Run the following commands to ensure the build process works correctly:

⚠️ **IMPORTANT**: Use `build:sap` for SAP BSP deployment (not `build`)

```bash
# Build SAP-compatible deployment package
npm run build:sap

# Verify output folders
ls -lh deploy-sap/
# Expected: 10 files (no hyphens in names)

ls deploy-sap/controller/
# Expected: 3 files (NotificationBanner.js, TileCounter.js, View1.controller.js)
```

**Expected Results:**
- ✅ Build completes in ~20-30 seconds without errors
- ✅ `deploy-sap/` folder created with 10 SAP-compatible files
- ✅ No files with hyphens (`-`) in filenames
- ✅ All critical files present (Component.js, index.html, manifest.json, etc.)

### Code Quality Verification

**ABAP Files:**
- ✅ `abap/domains/` - 4 custom domains with fixed values
- ✅ `abap/data_elements/` - 4 data elements for type safety
- ✅ `abap/ztnotify_msgs.se11` - Table definition with custom data elements
- ✅ `abap/ztnotify_messages.ddls` - CDS view syntax valid
- ✅ `abap/zcl_notification_manager.clas.abap` - Business logic class
- ✅ `abap/zcl_notification_rest.clas.abap` - REST service class

**Frontend Files:**
- ✅ `webapp/Component.js` - FLP compatibility with safe checks
- ✅ `webapp/controller/NotificationBanner.js` - Banner and toast display logic
- ✅ `webapp/controller/TileCounter.js` - Dynamic tile counter
- ✅ `webapp/manifest.json` - Valid JSON with correct dataSources
- ✅ `webapp/i18n/i18n.properties` - 90+ translation keys
- ✅ `ui5.yaml` - Build configuration with zipper task

### Production Readiness Features

Verify these features are implemented:

- ✅ **Display Modes**: BANNER, TOAST, BOTH, SILENT support
- ✅ **Tile Counter**: Real-time statistics with color coding
- ✅ **Error Handling**: Exponential backoff retry (3 attempts: 1s → 2s → 4s delays)
- ✅ **Circuit Breaker**: Opens after 5 consecutive errors, resets after 60s
- ✅ **FLP Compatibility**: Safe checks for `sap.ushell.Container` existence
- ✅ **Standalone Mode**: Works in both FLP and standalone environments
- ✅ **Timeout Handling**: 10s AJAX timeout configured
- ✅ **User Context**: Safe user ID retrieval with ANONYMOUS fallback

### GO/NO-GO Decision

**✅ GO - Ready for Production** if:
- All build verification steps pass
- All ABAP files have correct syntax
- Frontend files validated
- Error handling features confirmed

**❌ NO-GO - Fix Issues** if:
- Build fails or produces errors
- ABAP syntax errors exist
- Missing Component-preload.js
- manifest.json validation fails

---

## 📊 Deployment Overview

```mermaid
graph TB
    Start([🚀 Start Deployment]) --> Prerequisites{Prerequisites<br/>Met?}
    Prerequisites -->|No| SetupEnv[Setup Environment<br/>Install Node.js, UI5 Tools<br/>Get SAP Access]
    SetupEnv --> Prerequisites
    Prerequisites -->|Yes| Backend[🗄️ Backend Deployment]

    Backend --> Domains[Step 1: Custom Domains<br/>SE11: Create 4 Domains]
    Domains --> DataElements[Step 2: Data Elements<br/>SE11: Create 4 Data Elements]
    DataElements --> DB[Step 3: Database Table<br/>SE11: ZTNOTIFY_MSGS]
    DB --> CDS[Step 4: CDS View<br/>SE80: ZTNOTIFY_MESSAGES]
    CDS --> Classes[Step 5: ABAP Classes<br/>SE80: Manager + REST]
    Classes --> REST[Step 6: REST Service<br/>SICF: Configure endpoint]
    REST --> Auth[Step 7: Authorization<br/>SU21/PFCG: Optional]

    Auth --> Frontend[💻 Frontend Deployment]
    Frontend --> Install[Install Dependencies<br/>npm install]
    Install --> Configure[Configure<br/>ui5.yaml, manifest.json]
    Configure --> Build[Build<br/>npm run build]
    Build --> Deploy[Deploy to SAP<br/>BSP Application]
    Deploy --> FLP[FLP Configuration<br/>/UI2/FLPD_CUST]

    FLP --> Testing[🧪 Testing Phase]
    Testing --> DisplayModes[Test Display Modes<br/>BANNER/TOAST/BOTH/SILENT]
    Testing --> TileCounter[Test Tile Counter<br/>Statistics/Color Coding]
    Testing --> BackendTest[Backend Tests<br/>REST API/Database]

    DisplayModes --> Verification{All Tests<br/>Pass?}
    TileCounter --> Verification
    BackendTest --> Verification
    Verification -->|No| Debug[🔧 Troubleshooting]
    Debug --> Testing
    Verification -->|Yes| Production[✅ Production Ready]

    Production --> Monitor[📊 Monitoring & Maintenance]

    style Start fill:#c8e6c9,stroke:#388e3c,stroke-width:2px
    style Backend fill:#bbdefb,stroke:#1976d2,stroke-width:2px
    style Frontend fill:#ffccbc,stroke:#d84315,stroke-width:2px
    style Testing fill:#fff9c4,stroke:#f57c00,stroke-width:2px
    style Production fill:#c8e6c9,stroke:#388e3c,stroke-width:3px
    style Verification fill:#ffecb3,stroke:#f57c00,stroke-width:2px
```

**Estimated Total Time**: 4-6 hours for experienced SAP developers

---

## 🔧 Prerequisites

### System Access Required
- ✅ **ABAP Development** access (SE11, SE80, SICF)
- ✅ **Authorization Management** access (SU01, PFCG, SU21)
- ✅ **Fiori Launchpad** configuration access
- ✅ **Node.js** environment (v16+ recommended)
- ✅ **UI5 Tooling** setup

### Required Knowledge
- SAP ABAP development
- UI5/Fiori development basics
- SAP authorization concepts
- REST API configuration

---

## 🖥️ System Requirements

### Backend (S/4HANA)
- **Version**: S/4HANA PCE 2023 or higher
- **Components**: SAP_UI, SAP_GWFND, SAP_ABA
- **Memory**: Minimum 512MB for notification service
- **Database**: SAP HANA or compatible

### Frontend
- **UI5 Version**: 1.60.0 or higher
- **Node.js**: v16.x - v20.x
- **npm**: v8.x or higher
- **Browsers**: Chrome 90+, Edge 90+, Firefox 88+, Safari 14+

---

## 🏗️ Backend Deployment (ABAP)

### Step 1: Create Custom Domains

Custom domains provide automatic F4 help and database-level validation for notification fields.

**Transaction**: SE11 → Domain

#### Domain 1: ZDOMAIN_MSG_TYPE

```
Domain Name: ZDOMAIN_MSG_TYPE
Short Description: Message Type Domain
Data Type: CHAR
Length: 12
Output Length: 12
```

**Value Range** (Fixed Values tab):
```
Value       Short Description
----------- ------------------------------------
URGENT      Urgent System Message
INFO        Informational Message
TIP         Helpful Tip
SUCCESS     Success Notification
MAINT       Scheduled Maintenance
WARNING     Warning Message
```

**Actions**:
1. SE11 → Enter "ZDOMAIN_MSG_TYPE" → Create
2. Enter data type: CHAR, Length: 12
3. Go to "Value Range" tab
4. Add 6 fixed values as shown above
5. **Save** → **Activate**

#### Domain 2: ZDOMAIN_SEVERITY

```
Domain Name: ZDOMAIN_SEVERITY
Short Description: Severity Level Domain
Data Type: CHAR
Length: 8
Output Length: 8
```

**Value Range** (Fixed Values tab):
```
Value       Short Description
----------- ------------------------------------
HIGH        High Priority (Critical/Error)
MEDIUM      Medium Priority (Warning)
LOW         Low Priority (Info)
```

**Actions**:
1. SE11 → Enter "ZDOMAIN_SEVERITY" → Create
2. Enter data type: CHAR, Length: 8
3. Go to "Value Range" tab
4. Add 3 fixed values as shown above
5. **Save** → **Activate**

#### Domain 3: ZDOMAIN_DISPLAY_MODE

```
Domain Name: ZDOMAIN_DISPLAY_MODE
Short Description: Display Mode Domain
Data Type: CHAR
Length: 10
Output Length: 10
```

**Value Range** (Fixed Values tab):
```
Value       Short Description
----------- ------------------------------------
BANNER      Fixed Top Banner
TOAST       Toast Notification (5s)
BOTH        Banner + Toast
SILENT      Silent (Log Only)
```

**Actions**:
1. SE11 → Enter "ZDOMAIN_DISPLAY_MODE" → Create
2. Enter data type: CHAR, Length: 10
3. Go to "Value Range" tab
4. Add 4 fixed values as shown above
5. **Save** → **Activate**

#### Domain 4: ZDOMAIN_TARGET_USERS

```
Domain Name: ZDOMAIN_TARGET_USERS
Short Description: Target Audience Domain
Data Type: CHAR
Length: 10
Output Length: 10
```

**Value Range** (Fixed Values tab - SAP Standard Roles Only):
```
Value       Short Description
----------- ------------------------------------
ALL         All Users
ADMIN       Administrators (SAP_ALL role)
DEVELOPER   Developers (SAP_BR_DEVELOPER role)
```

**Actions**:
1. SE11 → Enter "ZDOMAIN_TARGET_USERS" → Create
2. Enter data type: CHAR, Length: 10
3. Go to "Value Range" tab
4. Add 3 fixed values as shown above
5. **Save** → **Activate**

**Note**: Uses SAP standard roles only (SAP_ALL, SAP_BR_DEVELOPER). Custom Z_* roles removed for simplicity and security.

**✅ Verification**: All 4 domains should show "Active" status in SE11.

---

### Step 2: Create Data Elements

Data elements connect domains to table fields and provide field labels for UI display.

**Transaction**: SE11 → Data Type

#### Data Element 1: ZNOTIFY_MSG_TYPE

```
Data Element: ZNOTIFY_MSG_TYPE
Short Description: Notification Message Type
Domain: ZDOMAIN_MSG_TYPE
```

**Field Labels** (Field Label tab):
```
Short:  Msg Type
Medium: Message Type
Long:   Notification Message Type
Heading: Msg Type
```

**Actions**:
1. SE11 → Data Type → Enter "ZNOTIFY_MSG_TYPE" → Create
2. Select "Data Element" → Continue
3. Enter short description
4. Domain: ZDOMAIN_MSG_TYPE
5. Go to "Field Label" tab → Enter labels
6. **Save** → **Activate**

#### Data Element 2: ZNOTIFY_SEVERITY

```
Data Element: ZNOTIFY_SEVERITY
Short Description: Notification Severity Level
Domain: ZDOMAIN_SEVERITY
```

**Field Labels**:
```
Short:  Severity
Medium: Severity Level
Long:   Notification Severity Level
Heading: Severity
```

**Actions**:
1. SE11 → Data Type → Enter "ZNOTIFY_SEVERITY" → Create
2. Select "Data Element" → Continue
3. Enter short description
4. Domain: ZDOMAIN_SEVERITY
5. Go to "Field Label" tab → Enter labels
6. **Save** → **Activate**

#### Data Element 3: ZNOTIFY_DISP_MODE

```
Data Element: ZNOTIFY_DISP_MODE
Short Description: Notification Display Mode
Domain: ZDOMAIN_DISPLAY_MODE
```

**Field Labels**:
```
Short:  Display
Medium: Display Mode
Long:   Notification Display Mode
Heading: Display
```

**Actions**:
1. SE11 → Data Type → Enter "ZNOTIFY_DISP_MODE" → Create
2. Select "Data Element" → Continue
3. Enter short description
4. Domain: ZDOMAIN_DISPLAY_MODE
5. Go to "Field Label" tab → Enter labels
6. **Save** → **Activate**

#### Data Element 4: ZNOTIFY_TARGET_USERS

```
Data Element: ZNOTIFY_TARGET_USERS
Short Description: Target Audience Filter
Domain: ZDOMAIN_TARGET_USERS
```

**Field Labels**:
```
Short:  Target
Medium: Target Users
Long:   Target Audience Filter
Heading: Target
```

**Actions**:
1. SE11 → Data Type → Enter "ZNOTIFY_TARGET_USERS" → Create
2. Select "Data Element" → Continue
3. Enter short description
4. Domain: ZDOMAIN_TARGET_USERS
5. Go to "Field Label" tab → Enter labels
6. **Save** → **Activate**

**✅ Verification**: All 4 data elements should show "Active" status in SE11.

---

### Step 3: Create Database Table

**Transaction**: SE11 → Database Table

**File Reference**: `abap/ztnotify_msgs.se11`

Create table `ZTNOTIFY_MSGS` with the following structure:

```
Table Name: ZTNOTIFY_MSGS
Short Description: Global Notification Messages
Delivery Class: A (Application Table)
```

**Fields** (using custom data elements):

| Field Name      | Key | Data Element           | Type      | Length | Description                  |
|-----------------|-----|------------------------|-----------|--------|------------------------------|
| CLIENT          | ✅  | MANDT                  | CLNT      | 3      | Client                       |
| MESSAGE_ID      | ✅  | CHAR32                 | CHAR      | 32     | Message ID (UUID)            |
| MESSAGE_TYPE    |     | ZNOTIFY_MSG_TYPE       | CHAR      | 12     | Message Type (F4 help)       |
| SEVERITY        |     | ZNOTIFY_SEVERITY       | CHAR      | 8      | Severity Level (F4 help)     |
| TITLE           |     | CHAR255                | CHAR      | 255    | Notification Title           |
| MESSAGE_TEXT    |     | CHAR255                | CHAR      | 255    | Message Text                 |
| START_DATE      |     | DATS                   | DATS      | 8      | Valid From Date              |
| END_DATE        |     | DATS                   | DATS      | 8      | Valid To Date                |
| TARGET_USERS    |     | ZNOTIFY_TARGET_USERS   | CHAR      | 10     | Target Audience (F4 help)    |
| ACTIVE          |     | CHAR1                  | CHAR      | 1      | Active Flag (X/blank)        |
| DISPLAY_MODE    |     | ZNOTIFY_DISP_MODE      | CHAR      | 10     | Display Mode (F4 help)       |
| CREATED_BY      |     | SYUNAME                | CHAR      | 12     | Created By User              |
| CREATED_AT      |     | TIMESTAMPL             | DEC       | 21     | Created Timestamp            |
| CHANGED_BY      |     | SYUNAME                | CHAR      | 12     | Changed By User              |
| CHANGED_AT      |     | TIMESTAMPL             | DEC       | 21     | Changed Timestamp            |

**Actions**:
1. SE11 → Database Table → Enter "ZTNOTIFY_MSGS" → Create
2. Short Description: "Global Notification Messages"
3. Delivery Class: A (Application Table)
4. **Add all fields as shown above** (use custom data elements, not generic CHAR types)
5. Technical Settings:
   - Data Class: APPL0 (Master Data)
   - Size Category: 1 (0-15,999 rows expected)
6. **Save** → **Check** → **Activate**

**🎯 Key Points**:
- MESSAGE_TYPE uses ZNOTIFY_MSG_TYPE → Automatic F4 help with 6 values
- SEVERITY uses ZNOTIFY_SEVERITY → Automatic F4 help with 3 values
- DISPLAY_MODE uses ZNOTIFY_DISP_MODE → Automatic F4 help with 4 values
- TARGET_USERS uses ZNOTIFY_TARGET_USERS → Automatic F4 help with 3 values (SAP standard roles only)
- MESSAGE_TEXT uses CHAR255 (fixed length 255 chars, SM30 compatible)

**✅ Verification**:
- SE11 → Display ZTNOTIFY_MSGS → Check all fields exist
- SM30 → ZTNOTIFY_MSGS → Test F4 help on MESSAGE_TYPE (should show 6 values)
- SM30 → ZTNOTIFY_MSGS → Test F4 help on TARGET_USERS (should show 3 values: ALL, ADMIN, DEVELOPER)

---

### Step 4: Create CDS View

**Tool Required**: Eclipse ADT (ABAP Development Tools) or SAP HANA Studio

**Alternative**: Use transaction SE38 to run program `RUTDDLSACT` for activation

**File Reference**: `abap/ztnotify_messages.ddls`

> **⚠️ Important**: CDS views **CANNOT** be created in SE80 or SE11 for SAP S/4HANA On-Premise.
> You must use Eclipse ADT (recommended) or create the DDL source code manually and activate it via SE38.

```sql
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Global Notification Messages'

define view entity ztnotify_messages
  as select from ztnotify_msgs
{
  key message_id,
      message_type,
      severity,
      title,
      message_text,
      start_date,
      end_date,
      target_users,
      active,
      display_mode,
      created_by,
      created_at,
      changed_by,
      changed_at
}
where active = 'X'
  and start_date <= $session.system_date
  and end_date >= $session.system_date
```

**Key Features**:
- **define view entity** → Modern CDS syntax (replaces obsolete DDIC-based views)
- **@AccessControl.authorizationCheck: #NOT_REQUIRED** → Public access for all users
- **WHERE clause** → Only active notifications within date range
- **All fields** → Includes display_mode and audit fields

**Important Notes**:
- Uses modern `view entity` syntax (not obsolete `define view`)
- No SQL view name annotation needed (modern approach)
- For OData exposure, use RAP or create separate service definition

**Actions (Eclipse ADT Method - Recommended)**:
1. Open Eclipse with ABAP Development Tools (ADT)
2. Right-click on package → New → Other ABAP Repository Object
3. Select "Data Definition" (DDL Source)
4. Enter name: `ztnotify_messages` (lowercase, must match entity name)
5. Copy the CDS view code above into the editor
6. **Save** (Ctrl+S) → **Activate** (Ctrl+F3)

**Actions (Manual Method - Alternative)**:
1. Create file `ztnotify_messages.ddls` with the DDL source code above
2. Upload via SE38 → Program `MASS_UPLOAD` or similar
3. Activate via SE38 → Program `RUTDDLSACT`
4. Enter CDS view name: `ztnotify_messages`

**Important**: DDL source name and view entity name must be identical (case-sensitive)

**✅ Verification**:
- Eclipse ADT → Check "Active" status for `ztnotify_messages`
- SE11 → Display table `ZTNOTIFY_MSGS` (base table)
- Note: Modern view entities don't create SQL views automatically

---

### Step 5: Create ABAP Classes

#### Class 1: ZCL_NOTIFICATION_MANAGER

**Transaction**: SE80 → Class Builder

**File Reference**: `abap/zcl_notification_manager.clas.abap`

This class contains the core business logic for notification management.

**Class Structure**:
```
Class Name: ZCL_NOTIFICATION_MANAGER
Description: Notification Manager - Business Logic
Instantiation: Public
```

**Static Methods**:
- `get_active_notifications` - Retrieve active notifications for a user
- `create_notification` - Create a new notification
- `update_notification` - Update an existing notification
- `deactivate_notification` - Deactivate a notification (sets active = ' ')
- `check_user_authorization` - Check if user is authorized

**Key Type Definition**:
```abap
TYPES: BEGIN OF ty_notification,
         message_id   TYPE char32,      " UUID in text format
         message_type TYPE char12,      " MESSAGE_TYPE domain
         severity     TYPE char8,       " SEVERITY domain
         title        TYPE char255,     " Notification title
         message_text TYPE char255,     " Notification text
         start_date   TYPE dats,        " Valid from date
         end_date     TYPE dats,        " Valid to date
         target_users TYPE char10,      " TARGET_USERS domain
         active       TYPE char1,       " Active flag (X/' ')
         display_mode TYPE char10,      " DISPLAY_MODE domain
         created_by   TYPE syuname,     " Created by user
         created_at   TYPE timestampl,  " Created timestamp
         changed_by   TYPE syuname,     " Changed by user
         changed_at   TYPE timestampl,  " Changed timestamp
       END OF ty_notification.

TYPES: tt_notifications TYPE STANDARD TABLE OF ty_notification WITH DEFAULT KEY.
```

**Important - Modern ABAP Compliance**:
- ✅ All string types replaced with fixed CHAR types (fully-typed parameters)
- ✅ Table type uses STANDARD TABLE WITH DEFAULT KEY (required for RETURNING)
- ✅ System variables escaped with @ in SQL: `@sy-datum`, `@sy-uname`
- ✅ Method parameters escaped with @ in SQL: `@iv_message_id`, `@lv_timestamp`

**Actions**:
1. SE80 → Class Builder → Create class `ZCL_NOTIFICATION_MANAGER`
2. Copy definition and implementation from `abap/zcl_notification_manager.clas.abap`
3. Verify all 5 public static methods are present
4. **Save** → **Check** → **Activate**
5. **Important**: Ensure Eclipse ADT is used for activation (modern syntax required)

**🎯 Key Features**:
- Role-based target audience filtering (3 fixed values: ALL, ADMIN, DEVELOPER - SAP standard roles only)
- Display mode support (BANNER, TOAST, BOTH, SILENT)
- Audit trail (automatically sets created_by, created_at, changed_by, changed_at)
- Statistics calculation for tile counter

#### Class 2: ZCL_NOTIFICATION_REST

**Transaction**: SE80 → Class Builder (or Eclipse ADT)

**File Reference**: `abap/zcl_notification_rest.clas.abap`

This class provides the REST API interface for SICF HTTP service handler.

**Class Structure**:
```
Class Name: ZCL_NOTIFICATION_REST
Description: Notification REST Service Handler
Interfaces: IF_HTTP_EXTENSION (required by SICF)
```

**REST Endpoints**:
- `GET /` - Get all active notifications for current user
- `GET /stats` - Get statistics (total, high_count, medium_count, low_count)
- `GET /log` - Get silent notifications (display_mode = SILENT)
- `POST /` - Create new notification
- `PUT /?message_id=xxx` - Update existing notification
- `DELETE /?message_id=xxx` - Delete (deactivate) notification

**Architecture**:
```abap
PUBLIC SECTION.
  INTERFACES: if_http_extension.

PRIVATE SECTION.
  DATA: mo_server TYPE REF TO if_http_server.

  METHODS: handle_get_notifications,
           handle_create_notification,
           handle_update_notification,
           handle_delete_notification,
           handle_get_stats,
           handle_get_log,
           serialize_notifications,
           deserialize_notification.
```

**Implementation Highlights**:
```abap
METHOD if_http_extension~handle_request.
  " Store server reference
  mo_server = server.

  " Set CORS headers for Fiori apps
  server->response->set_header_field(
    name = 'Access-Control-Allow-Origin' value = '*' ).
  server->response->set_header_field(
    name = 'Access-Control-Allow-Methods'
    value = 'GET,POST,PUT,DELETE,OPTIONS' ).
  server->response->set_header_field(
    name = 'Access-Control-Allow-Headers'
    value = 'Content-Type,Accept,Authorization,X-Requested-With' ).

  " Handle OPTIONS preflight for CORS
  IF lv_method = 'OPTIONS'.
    server->response->set_status( code = 200 reason = 'OK' ).
    RETURN.
  ENDIF.

  " Route to appropriate handler based on method and path
  CASE lv_method.
    WHEN 'GET'.
      IF lv_path CS '/stats'.
        handle_get_stats( ).
      ELSEIF lv_path CS '/log'.
        handle_get_log( ).
      ELSE.
        handle_get_notifications( ).
      ENDIF.
    WHEN 'POST'.
      handle_create_notification( ).
    " ... etc
  ENDCASE.
ENDMETHOD.
```

**Actions**:
1. **Eclipse ADT** (Recommended):
   - New → ABAP Class → `ZCL_NOTIFICATION_REST`
   - Copy definition and implementation from `abap/zcl_notification_rest.clas.abap`
   - **Save** (Ctrl+S) → **Activate** (Ctrl+F3)

2. **SE80** (Alternative):
   - Class Builder → Create class `ZCL_NOTIFICATION_REST`
   - Add interface: `IF_HTTP_EXTENSION`
   - Implement method: `IF_HTTP_EXTENSION~HANDLE_REQUEST`
   - Copy implementation code
   - **Save** → **Check** → **Activate**

**🎯 Key Features**:
- ✅ **IF_HTTP_EXTENSION** interface (required by SICF handlers)
- ✅ **Direct HTTP server access** via `mo_server->request/response`
- ✅ **CORS handling** in code (no SICF configuration needed)
- ✅ **OPTIONS preflight** support for cross-origin requests
- ✅ **JSON serialization** using `/UI2/CL_JSON` (standard SAP class)
- ✅ **HTTP status codes**: 200 OK, 201 Created, 500 Internal Server Error
- ✅ **Query parameter handling**: `get_form_field('user_id')`, `get_form_field('message_id')`
- ✅ **Request body parsing**: `mo_server->request->get_cdata()`
- ✅ **Response formatting**: `mo_server->response->set_cdata( lv_json )`

**Modern ABAP Compliance**:
- ✅ All SQL statements use @ escape: `WHERE active = 'X' AND start_date <= @lv_today`
- ✅ Type conversion: `string` → `char32` / `sy-uname` for method calls
- ✅ JSON handling: `/ui2/cl_json=>serialize()` and `=>deserialize()`
- ✅ No deprecated CL_REST_* classes (pure IF_HTTP_EXTENSION implementation)

**✅ Verification**:
- SE80 → Display both classes → Check "Active" status
- SE24 → ZCL_NOTIFICATION_MANAGER → Test method `get_active_notifications`
- Eclipse ADT → Verify no syntax errors or warnings

**⚠️ Common Activation Errors & Solutions**:

| Error | Solution |
|-------|----------|
| "A RETURNING parameter must be fully typed" | Use CHAR types (not string), add WITH DEFAULT KEY to table types |
| "Variable SY-DATUM must be escaped using @" | Add @ prefix: `@sy-datum`, `@sy-uname` |
| "DDIC-based CDS views are obsolete" | Use `define view entity` instead of `define view` |
| "Name of entity and DDL source must be identical" | Ensure file name matches entity name (case-sensitive) |

---

### Step 6: Configure REST Service

**Transaction**: SICF (HTTP Service Hierarchy)

Configure the REST endpoint to make the notification service accessible via HTTP.

**Actions**:

1. **Navigate to REST services**:
   ```
   Transaction: SICF
   Path: /default_host/sap/bc/rest/
   ```

2. **Create new sub-element**:
   - Right-click on `/rest/` → **New Sub-Element**
   - Service Name: `zcl_notif_rest` (shorter name, max 30 chars)
   - Description: `Global Notification REST Service`
   - **Important**: Select **"Standalone Service"** (NOT "Alias to an existing service")
   - **Administration Service**: Leave **unchecked** (standard application service)

3. **Configure Handler**:
   - Go to **"Handler List"** tab
   - Click **"New Entry"** or **"+"**
   - Handler class: `ZCL_NOTIFICATION_REST`
   - **Save**

4. **Security Settings** (Logon Data tab):
   - Go to **"Logon Data"** tab
   - Service Specific Settings:
     - ✅ **Standard** (SAP Standard)
     - ✅ **Alternative Logon** → Basic Authentication
     - ✅ **SAP Logon Ticket**
   - Security Requirement: **Standard User**

5. **CORS Configuration** (handled in code):
   - ✅ **No SICF configuration needed** - CORS headers are set in `ZCL_NOTIFICATION_REST->IF_HTTP_EXTENSION~HANDLE_REQUEST`
   - The class automatically sets:
     - `Access-Control-Allow-Origin: *`
     - `Access-Control-Allow-Methods: GET,POST,PUT,DELETE,OPTIONS`
     - `Access-Control-Allow-Headers: Content-Type,Accept,Authorization,X-Requested-With`
     - `Access-Control-Max-Age: 3600`

6. **Activate Service**:
   - Right-click on `zcl_notif_rest` → **Activate Service**
   - Status should show **green traffic light** 🟢

**SICF Configuration Summary**:
```
Service Path: /sap/bc/rest/zcl_notif_rest
Handler Class: ZCL_NOTIFICATION_REST
Interface: IF_HTTP_EXTENSION
Service Type: Standalone (Independent Service)
Administration: No (Standard Application Service)
Security: SAP Standard + Basic Auth + Logon Ticket
CORS: Handled in code (automatic)
```

**Test the Endpoint**:

**Test 1 - Empty Response (expected if no data)**:
```
URL: https://<your-server>:<port>/sap/bc/rest/zcl_notif_rest?sap-client=<client>
Method: GET
Expected Response: []
HTTP Status: 200 OK
```

**Test 2 - Statistics Endpoint**:
```
URL: https://<your-server>:<port>/sap/bc/rest/zcl_notif_rest/stats?sap-client=<client>
Method: GET
Expected Response: {"total":0,"high_count":0,"medium_count":0,"low_count":0}
HTTP Status: 200 OK
```

**Test 3 - CORS Preflight**:
```
URL: https://<your-server>:<port>/sap/bc/rest/zcl_notif_rest?sap-client=<client>
Method: OPTIONS
Expected Headers:
  Access-Control-Allow-Origin: *
  Access-Control-Allow-Methods: GET,POST,PUT,DELETE,OPTIONS
HTTP Status: 200 OK
```

**Example Working URL** (based on your test):
```
https://vhwqtds4ci.sap.windtre.it:44300/sap/bc/rest/zcl_notif_rest?sap-client=100
Response: []  ✅ SERVICE WORKING!
```

**Note**:
- Service name in SICF: `zcl_notif_rest` (14 chars)
- Handler class name: `ZCL_NOTIFICATION_REST` (21 chars)
- Empty array `[]` response means service is working correctly - table has no active notifications

**✅ Verification**:
- ✅ SICF → Check `zcl_notif_rest` is active (green traffic light 🟢)
- ✅ Test GET request using browser or Postman
- ✅ Handler class `ZCL_NOTIFICATION_REST` is assigned
- ✅ Response returns `[]` (empty array) if no data exists
- ✅ Response returns JSON array with notifications if data exists
- ✅ HTTP Status: 200 OK
- ✅ CORS headers present in response (check browser Developer Tools → Network)

**🎯 Success Criteria**:
- Browser/Postman returns `[]` without authentication errors
- No 401 Unauthorized or 403 Forbidden errors
- No 500 Internal Server Error
- Response Content-Type: `application/json`

---

### Step 7: Create Authorization Object (Optional)

**⚠️ IMPORTANT**: This step is OPTIONAL. The notification system works with public access (@AccessControl.authorizationCheck: #NOT_REQUIRED). Only create this if you need role-based admin restrictions.

#### 7.1 Create Authorization Object Z_NOTIFY

**Transaction**: SU21 (Maintain Authorization Objects)

##### Step 7.1.1: Create Authorization Object

1. **Open Transaction SU21**
   - Enter transaction code: `/nsu21`
   - Press Enter

2. **Navigate to Object Class**
   - In the left tree, expand **"Authorization Objects"**
   - Expand **"Cross-Application Authorization Objects"**
   - Select **"BC_A"** (or your preferred object class)

3. **Create New Object**
   - Click **"Create"** button (or press F5)
   - Enter object name: `Z_NOTIFY`
   - Press Enter

4. **Enter Object Details**
   ```
   Object: Z_NOTIFY
   Object Class: BC_A (Cross-Application Authorization Objects)
   Short Text: Notification Banner Administration
   ```
   - Click **"Continue"** (green checkmark) or press Enter

##### Step 7.1.2: Add Field 1 - ACTVT (Activity)

5. **Add ACTVT Field** (standard SAP authorization field)
   - In the "Fields" section at the bottom, click **"New Entries"** or press F5
   - Enter field details:
     ```
     Authorization Field: ACTVT
     ```
   - **IMPORTANT**: Type `ACTVT` and press Enter - the system will automatically populate:
     - Data Element: `ACTIV_AUTH`
     - Short Text: "Activity"
   - **Do NOT manually select a data element** - ACTVT is a standard SAP field

##### Step 7.1.3: Create Authorization Field ZNOTIFY_TP

**💡 CRITICAL**: Authorization fields must be created in **SU20** (Maintain Authorization Fields), NOT in SE11!

**⚠️ Name Length Limit**:
- SU21 authorization field names: **Maximum 10 characters**
- `ZNOTIFY_MSG_TYPE` (17 chars) ❌ Too long
- `ZNOTIFY_TYP` (12 chars) ❌ Still too long
- `ZNOTIFY_TP` (10 chars) ✅ Perfect!

6. **Open Transaction SU20** (in a new session, keep SU21 open)
   - Enter transaction code: `/nsu20`
   - Press Enter
   - You will see the list of existing authorization fields

7. **Create New Authorization Field** ⭐ CRITICAL STEP!
   - Click **"New Entries"** button (or press F5)
   - You will see the entry form for a new authorization field

8. **Fill in Authorization Field Details**
   ```
   Field Name: ZNOTIFY_TP
   Data Element: ZNOTIFY_MSG_TYPE  ⭐ REUSE DATA ELEMENT FROM STEP 2!
   ```

   **⚠️ IMPORTANT**:
   - Use the existing data element `ZNOTIFY_MSG_TYPE` created in Step 2
   - This automatically reuses domain `ZDOMAIN_MSG_TYPE` with its 6 fixed values
   - F4 help will automatically work in authorization maintenance (PFCG)
   - Authorization field inherits all properties from the data element

9. **Save the Authorization Field**
   - Click **"Save"** (Ctrl+S)
   - Assign to transport request (or save as local object $TMP)
   - The field is now registered in the SAP authorization field catalog
   - ✅ The field ZNOTIFY_TP is now available for use in SU21

**✨ What Just Happened**:
```
SU20: Authorization Field ZNOTIFY_TP
  └── uses Data Element: ZNOTIFY_MSG_TYPE
       └── uses Domain: ZDOMAIN_MSG_TYPE
            └── 6 fixed values: URGENT, INFO, MAINT, WARNING, TIP, SUCCESS
```

**Benefits of This Approach**:
- ✅ Single source of truth: Domain ZDOMAIN_MSG_TYPE
- ✅ Automatic F4 help in PFCG authorization maintenance
- ✅ Same values in table (ZTNOTIFY_MSGS.MESSAGE_TYPE) and auth object (Z_NOTIFY.ZNOTIFY_TP)
- ✅ Field length: 12 characters (from domain)
- ✅ Field name: 10 characters (fits SU21 limits)

##### Step 7.1.4: Add Field 2 - ZNOTIFY_TP to Authorization Object

10. **Return to SU21 Window** (should still be open)
    - In the "Fields" section, click **"New Entries"** again (or press F5)
    - Enter field details:
      ```
      Authorization Field: ZNOTIFY_TP
      ```
    - Press Enter - the system will populate:
      - Short Text: "Notification Type" (from SU20 authorization field definition)
      - Data Element: `ZNOTIFY_MSG_TYPE`

11. **Save Authorization Object**
    - Click **"Save"** button (Ctrl+S)
    - Select package: `$TMP` (local object) or your custom package (e.g., `ZNOTIFY`)
    - Enter transport request if using custom package

12. **Check Authorization Object**
    - The object should now show 2 fields:
      1. ACTVT (Activity) - Data Element: ACTIV_AUTH
      2. ZNOTIFY_TP (Notification Type) - Data Element: ZNOTIFY_MSG_TYPE

##### Step 7.1.5: Verification

**✅ Verification Steps**:

1. **Check Authorization Field ZNOTIFY_TP**:
   ```
   SU20 → Display Authorization Field ZNOTIFY_TP
   Expected:
   - Field Name: ZNOTIFY_TP
   - Data Element: ZNOTIFY_MSG_TYPE ⭐
   - Inherited from data element:
     * Domain: ZDOMAIN_MSG_TYPE
     * Data Type: CHAR(12)
     * Fixed Values: 6 values (URGENT, INFO, MAINT, WARNING, TIP, SUCCESS)
   - Name Length: 10 characters (within SU21 limit)
   ```

2. **Check Authorization Object**:
   ```
   SU21 → Authorization Objects → BC_A → Z_NOTIFY → Display
   Expected:
   - Object: Z_NOTIFY
   - Object Class: BC_A
   - 2 Fields:
     1. ACTVT (Activity) - Data Element: ACTIV_AUTH
     2. ZNOTIFY_TP (Notification Type) - Data Element: ZNOTIFY_MSG_TYPE
   ```

3. **Test in PFCG**:
   - Transaction PFCG → Create test role
   - Authorizations → Change Authorization Data → Manually
   - Search for "Z_NOTIFY"
   - **Expected**: Object appears with 2 fields (ACTVT and ZNOTIFY_TP)
   - **Test F4 Help**: Click on ZNOTIFY_TP value field → Should show 6 values from domain + wildcard (*)

4. **Verify Data Element Reuse**:
   ```
   SE11 → Data Element → ZNOTIFY_MSG_TYPE → Where-Used List
   Expected to find:
   - ZTNOTIFY_MSGS.MESSAGE_TYPE (table field)
   - ZNOTIFY_TP (authorization field in SU20)
   ```

**Authorization Values for ACTVT**:
- `01` - Create notifications (POST)
- `02` - Modify notifications (PUT)
- `03` - Display notifications (GET)
- `06` - Delete notifications (DELETE)

**Authorization Values for ZNOTIFY_TP** (from domain ZDOMAIN_MSG_TYPE):
- `*` - All notification types (wildcard - for admin authorization)
- `URGENT` - Only urgent notifications
- `INFO` - Only informational notifications
- `MAINT` - Only maintenance notifications
- `WARNING` - Only warning notifications
- `TIP` - Only tips and suggestions
- `SUCCESS` - Only success messages

**✨ Benefits of Data Element Reuse Architecture**:
```
ZDOMAIN_MSG_TYPE (Domain - 6 fixed values)
  └── ZNOTIFY_MSG_TYPE (Data Element)
       ├── ZTNOTIFY_MSGS.MESSAGE_TYPE (table field)
       └── ZNOTIFY_TP (authorization field via SU20)
```
- ✅ Single domain maintains all fixed values (URGENT, INFO, MAINT, WARNING, TIP, SUCCESS)
- ✅ One data element reused in both table field and authorization field
- ✅ Automatic F4 help in both table maintenance (SM30) and authorization maintenance (PFCG)
- ✅ Single point of maintenance: Update domain → affects both table and authorization
- ✅ Consistency guaranteed: Same CHAR(12) length and values everywhere
- ✅ SU21 compatible: ZNOTIFY_TP (10 chars) fits authorization field name limits
- ✅ Created in SU20: Properly registered as authorization field

**🎯 Common Issues & Solutions**:

| Issue | Solution |
|-------|----------|
| "Authorization field ZNOTIFY_TP does not exist" | Create in **SU20** first (Step 7.1.3), NOT in SE11 |
| "Field ZNOTIFY_TP not found in SU21" | Create the field in SU20 first, then it will appear in SU21 field list |
| "Cannot add field to authorization object" | Ensure ZNOTIFY_TP was created in SU20 and saved properly |
| "Field does not appear in SU21 field list" | Transaction SU20 → Create field ZNOTIFY_TP → Reference data element ZNOTIFY_MSG_TYPE → Save |
| "Field does not appear in PFCG" | Save and re-open PFCG role, or restart transaction |
| "Data element ZNOTIFY_MSG_TYPE not found" | Ensure Step 2 (Create Data Elements) was completed and activated |
| "F4 help shows no values in PFCG" | Check domain ZDOMAIN_MSG_TYPE has 6 fixed values defined (URGENT, INFO, MAINT, WARNING, TIP, SUCCESS) |
| "Field name too long in SU21" | SU21 limit is 10 chars: ZNOTIFY_TP (10 chars) ✅, ZNOTIFY_TYP (12 chars) ❌ |

---

#### 7.2 Create Authorization Role

**Transaction**: PFCG (Role Maintenance)

**📖 Reference**: See [AUTHORIZATION_OBJECTS.md](./AUTHORIZATION_OBJECTS.md) for complete documentation of all authorization objects.

**Create Role: Z_NOTIFICATION_ADMIN**

**Actions**:

1. **Transaction PFCG** → Enter role name: `Z_NOTIFICATION_ADMIN`
2. Click **Create** (single role)
3. **Description tab**:
   ```
   Description: Notification Banner Administrator
   ```

4. **Authorizations tab** → Click **Change Authorization Data**
5. **Manually** → Add the following authorization objects:

   **A) Z_NOTIFY (Custom - Created in Step 7.1)**
   ```
   Authorization Object: Z_NOTIFY
   ACTVT: 01, 02, 03, 06    (Create, Change, Display, Delete)
   ZNOTIFY_TP: *            (All notification types)
   ```
   **Purpose**: Application-specific authorization for notification management
   **Note**: This is a CUSTOM object created in Step 7.1
   **Field Details**:
   - ACTVT: Standard SAP activity field (01=Create, 02=Change, 03=Display, 06=Delete)
   - ZNOTIFY_TP: Custom authorization field created in SU20
   - Data Element: ZNOTIFY_MSG_TYPE (same as table field MESSAGE_TYPE)
   - Domain: ZDOMAIN_MSG_TYPE (shared domain ensures consistency)
   - F4 Help: Shows 6 values (URGENT, INFO, MAINT, WARNING, TIP, SUCCESS) + wildcard (*)
   - Architecture: ZDOMAIN_MSG_TYPE → ZNOTIFY_MSG_TYPE → ZNOTIFY_TP (via SU20)
   - Name: 10 characters (SU21 limit)

   **B) S_TABU_DIS (SAP Standard - Table Authorization Group)**
   ```
   Authorization Object: S_TABU_DIS
   ACTVT: 01, 02, 03, 06
   DICBERCLS: &NC&  (Customer namespace tables Z*/Y*)
   ```
   **Purpose**: Coarse-grained access to customer tables
   **SAP Note**: Pre-existing SAP standard object, no creation needed

   **C) S_TABU_NAM (SAP Standard - Specific Table Authorization)**
   ```
   Authorization Object: S_TABU_NAM
   ACTVT: 01, 02, 03, 06
   TABLE: ZTNOTIFY_MSGS
   ```
   **Purpose**: Fine-grained access to ZTNOTIFY_MSGS table
   **Protects**: SM30, SE16, SE16N transactions
   **SAP Note**: Pre-existing SAP standard object, no creation needed

   **D) S_DEVELOP (SAP Standard - Development Authorization)**
   ```
   Authorization Object: S_DEVELOP
   ACTVT: 01, 02, 03
   DEVCLASS: *, $TMP, ZNOTIFY
   OBJTYPE: TABL, CLAS, DDLS, FUGR
   OBJNAME: Z*, ZTNOTIFY_*
   P_GROUP: *
   ```
   **Purpose**: Repository object maintenance (SE11, SE80, SE24)
   **Protects**: Creation/modification of tables, classes, CDS views
   **SAP Note**: Pre-existing SAP standard object, no creation needed

   **E) S_SERVICE (SAP Standard - Service Authorization)**
   ```
   Authorization Object: S_SERVICE
   SRV_NAME: ZCL_NOTIFICATION_REST
   SRV_TYPE: HTTP
   ACTVT: 01, 02, 03, 06, 16
   ```
   **Purpose**: HTTP service handler class authorization
   **Protects**: REST API service execution
   **SAP Note**: Pre-existing SAP standard object, no creation needed

   **F) S_ICF (SAP Standard - ICF Service Authorization)**
   ```
   Authorization Object: S_ICF
   ICF_FIELD: /sap/bc/rest/zcl_notif_rest*
   ICF_VALUE: *
   ACTVT: 03, 20
   ```
   **Purpose**: ICF service node access and activation
   **Protects**: SICF transaction, service activation
   **SAP Note**: Pre-existing SAP standard object, no creation needed

   **G) S_RFC (SAP Standard - RFC Authorization)**
   ```
   Authorization Object: S_RFC
   RFC_NAME: /IWFND/*, SYST
   RFC_TYPE: FUNC
   ACTVT: 16
   ```
   **Purpose**: Execute RFC calls (OData Gateway Foundation)
   **Note**: Only required if using OData service (optional for pure REST)
   **SAP Note**: Pre-existing SAP standard object, no creation needed

   **H) S_TCODE (SAP Standard - Transaction Code Authorization)**
   ```
   Authorization Object: S_TCODE
   TCD: SE11, SE24, SE80, SM30, SICF, SE10, PFCG
   ```
   **Purpose**: Grant access to development and maintenance transactions
   **SAP Note**: Pre-existing SAP standard object, no creation needed

6. **Generate** authorization profile:
   - Click **Generate** button (or Ctrl+F3)
   - System creates profile: `Z_NOTIFICATION_ADMIN`
   - Wait for green status message

7. **User Assignment tab**:
   - Add users who need admin access
   - Click **User Comparison** → **Complete Comparison**
   - Verify users have profile assigned

8. **Save** the role

**✅ Verification**:
```
PFCG → Display Z_NOTIFICATION_ADMIN → Authorizations tab
Expected: 8 authorization objects
  1. Z_NOTIFY (Custom)
  2. S_TABU_DIS (SAP Standard)
  3. S_TABU_NAM (SAP Standard)
  4. S_DEVELOP (SAP Standard)
  5. S_SERVICE (SAP Standard)
  6. S_ICF (SAP Standard)
  7. S_RFC (SAP Standard)
  8. S_TCODE (SAP Standard)
```

**📖 For detailed field descriptions and testing procedures**, see [AUTHORIZATION_OBJECTS.md](./AUTHORIZATION_OBJECTS.md)

---

#### 7.3 Create Read-Only Role (Optional)

For users who only need to view notifications in SM30/SE16:

**Transaction**: PFCG

**Create Role: Z_NOTIFICATION_DISPLAY**

```
Role Name: Z_NOTIFICATION_DISPLAY
Description: Notification Banner Display Only
```

**Authorizations**:
- **Z_NOTIFY**: ACTVT = 03 (Display only), NOTIFY_TYPE = *
- **S_TABU_NAM**: TABLE = ZTNOTIFY_MSGS, ACTVT = 03 (Display)
- **S_TABU_DIS**: DICBERCLS = &NC&

**✅ Verification**:
- User with this role can view notifications (SE16, SM30) but cannot create/modify

---

#### 7.4 Authorization Testing

**Test Admin Access**:
1. Assign Z_NOTIFICATION_ADMIN to test user (SU01)
2. Login as test user
3. **Test SM30**: SM30 → ZTNOTIFY_MSGS → Should allow Create/Change/Delete
4. **Test REST API**: POST to `/sap/bc/rest/zcl_notification_rest/` → Should succeed
5. **Test SE11**: SE11 → ZTNOTIFY_MSGS → Should allow display

**Test Read-Only Access**:
1. Assign Z_NOTIFICATION_DISPLAY to test user (SU01)
2. Login as test user
3. **Test SM30**: SM30 → ZTNOTIFY_MSGS → Should show data but greyed-out buttons
4. **Test REST API**: POST → Should return 403 Forbidden
5. **Test SE16**: SE16 → ZTNOTIFY_MSGS → Should allow display only

**Test No Authorization**:
1. Remove all notification roles from test user
2. Login as test user
3. **Test SM30**: SM30 → ZTNOTIFY_MSGS → Should return "No authorization"
4. **Test REST API**: All methods → Should return 403 Forbidden

**✅ Final Verification**:
```
SU53 → Display last authorization check
Expected: Shows Z_NOTIFY check with values used
```

---

## 💻 Frontend Deployment (UI5)

### 🎯 Deployment Strategy

The **RECOMMENDED** deployment method is **automated deployment** using Fiori tools (`npm run deploy`). Manual deployment via SE80 is provided as a fallback option for environments where automated deployment is not available.

### Option A: Automated Deployment with Fiori Tools (✅ RECOMMENDED)

This is the **primary and recommended** deployment method. It automates the entire deployment process using SAP Fiori tools.

#### Prerequisites

**Local Environment**:

```bash
npm install -g @sap/ux-ui5-tooling
npm install -g @ui5/cli
```

**SAP System Requirements** ⚠️ **CRITICAL**:

1. **OData Service for UI5 Repository** (must be active):

   ```
   Transaction: SICF
   Path: /default_host/sap/opu/odata/UI5/ABAP_REPOSITORY_SRV
   Status: Must be ACTIVE (green traffic light)
   ```

   **How to Activate**:

   ```
   1. SICF → Navigate to /sap/opu/odata/UI5/ABAP_REPOSITORY_SRV
   2. Right-click on service
   3. Select "Activate Service"
   4. Check status shows green traffic light ✅
   ```

2. **Gateway Service Registration** (CRITICAL - Required for OData deployment):

   ```
   Transaction: /n/IWFND/MAINT_SERVICE
   Purpose: Register UI5 Repository service in Gateway
   ```

   **How to Register** (do this BEFORE attempting deployment):

   ```
   1. Transaction: /n/IWFND/MAINT_SERVICE
   2. Click "Add Service" button
   3. System Alias: LOCAL (or your backend system alias)
   4. External Service Name: UI5_REPOSITORY_SRV
   5. Click "Get Services" button
   6. Select service from list: UI5_REPOSITORY_SRV
   7. Package Assignment: $TMP (or ZNOTIFY)
   8. Save
   9. Verify service appears in service list with status "Active"
   ```

   **Verification**:

   ```
   /IWFND/MAINT_SERVICE → Filter by "UI5_REPOSITORY"
   Expected: Service listed with green status icon
   ```

   ⚠️ **Without this step, deployment will fail with 403/404 errors even if SICF is active!**

3. **Virus Scan Profile Configuration** (SAP Note 1797736):

   ```
   Transaction: VSCAN or VSCANPROFILE
   Purpose: Required for OData file uploads
   SAP Note: 1797736 - Virus scan at upload via OData service
   ```

   **How to Configure** (requires BASIS team):

   ```
   1. Transaction: VSCAN
   2. Create/activate a default virus scan profile
   3. Configure virus scan interface (external antivirus software)
   4. Test profile: Upload a test file via OData
   ```

   **Alternative**: If virus scan profile cannot be configured, use **Option B: Manual Deployment**

4. **User Authorization**:

   ```
   Required Authorization Objects:
   - S_DEVELOP: ACTVT=01,02 (Create/Change repository objects)
   - S_TABU_NAM: TABLE=TADIR (Repository information system)
   - S_RFC: Execute OData RFC calls
   ```

**Verification Before Deployment**:

```bash
# Test 1: Check OData service accessibility
curl -u username:password \
  "https://your-system:port/sap/opu/odata/UI5/ABAP_REPOSITORY_SRV/Repositories?sap-client=100"

# Expected: XML response with repository list (not 404 or 403)

# Test 2: Check virus scan profile (via ABAP report or BASIS team)
# Execute report: RSVSCAN_CHECK_PROFILE
# Expected: "Default virus scan profile found" message
```

#### Deploy Command

```bash
# Set environment variables for authentication
export SAP_USER="your-username"
export SAP_PASSWORD="your-password"

# Run deployment
npx fiori deploy
```

**Deployment Configuration** (when prompted):

```
Target System: [Your S/4HANA system URL]
Client: [Your client number, e.g., 100]
BSP Application Name: ZNOTIFY_BANNER2
Package: $TMP (or your custom package, e.g., ZNOTIFY)
Transport Request: [Your TR number or leave empty for $TMP]
```

**Alternative: Use ui5-deploy.yaml Configuration**:

```bash
# Deployment configuration already exists in ui5-deploy.yaml
# Uses environment variables: SAP_USER and SAP_PASSWORD
npx fiori deploy --config ui5-deploy.yaml
```

**Expected Output (Success)**:

```
ℹ info Deploying application ZNOTIFY_BANNER2...
ℹ info Uploading files to /sap/opu/odata/UI5/ABAP_REPOSITORY_SRV...
✔ Deployment successful
✔ BSP application ZNOTIFY_BANNER2 created
✔ All resources uploaded and activated
✔ Application URL: https://your-system:port/sap/bc/bsp/sap/znotify_banner2/index.html
```

**Common Deployment Errors**:

| Error Code | Error Message | Root Cause | Solution |
|------------|--------------|------------|----------|
| **403** | Request failed with status code 403 | OData service not active OR missing authorization | 1. SICF → Activate `/sap/opu/odata/UI5/ABAP_REPOSITORY_SRV`<br>2. Check user has S_DEVELOP authorization |
| **400** | No default virus profile active or found | Virus scan profile not configured (SAP Note 1797736) | 1. Contact BASIS team to configure VSCAN profile<br>2. **OR** Use Option B: Manual Deployment |
| **404** | Service not found | OData service doesn't exist or path incorrect | 1. Verify SAP_GWFND component is installed<br>2. Check service path in SICF |
| **401** | Unauthorized | Incorrect credentials | 1. Check SAP_USER and SAP_PASSWORD env vars<br>2. Verify user account is not locked (SU01) |
| **500** | Internal Server Error | ABAP runtime error during upload | 1. Check ST22 dump analysis<br>2. Review SM21 system log<br>3. Check application log in SLG1 |

**✅ Verification After Successful Deployment**:

```bash
# 1. Check BSP Application created
# SE80 → Display ZNOTIFY_BANNER2
# Expected: Application exists with MIME Objects folder populated

# 2. Test application URL
curl -u username:password \
  "https://your-system:port/sap/bc/bsp/sap/znotify_banner2/index.html?sap-client=100"

# Expected: HTML content with UI5 bootstrap (not 404)

# 3. Verify MIME Objects count
# SE80 → ZNOTIFY_BANNER2 → MIME Objects
# Expected: 23 files in correct folder structure
```

**⚠️ If Automated Deployment Fails**:

1. Review error message and check table above
2. If virus scan profile error: **Use Option B: Manual Deployment** (recommended)
3. If OData service error: Contact BASIS team to activate service
4. If authorization error: Request required authorizations from security team

**Troubleshooting Steps**:

```bash
# Enable verbose logging
npx fiori deploy --config ui5-deploy.yaml --verbose

# Check ui5-deploy.yaml configuration
cat ui5-deploy.yaml

# Verify environment variables are set
echo $SAP_USER
echo $SAP_PASSWORD

# Test SICF service manually
# SICF → Test Service → /sap/opu/odata/UI5/ABAP_REPOSITORY_SRV
```

---

### Option B: Manual Deployment to BSP Application (Fallback)

⚠️ **IMPORTANT**: This manual deployment approach is provided as a fallback for environments where automated deployment (Option A) is not available or fails due to system configuration issues.

Use this method when:
- OData service `/sap/opu/odata/UI5/ABAP_REPOSITORY_SRV` is not available
- Virus scan profile cannot be configured
- Automated deployment fails with unresolvable errors

#### Step 1: Build SAP-Compatible Package

⚠️ **IMPORTANT**: SAP BSP does NOT support filenames with hyphens (`-`).
Use the special `build:sap` command to generate SAP-compatible files.

```bash
cd sap-fiori-notification-banner
npm install
npm run build:sap
```

**What This Does**:

1. Builds the UI5 application (`npm run build`)
2. Creates `deploy-sap/` folder with SAP-compatible files:
   - ✅ Removes debug files (`*-dbg.js`)
   - ✅ Removes source maps (`*.js.map`)
   - ✅ Removes preload bundle (`Component-preload.js` - contains hyphens)
   - ✅ Keeps only essential production files (10 files total)

**Expected Output**:

```
🚀 Preparing SAP BSP Deployment...

⏭️  Skipped: Component-preload.js
✅ Copied: Component.js
✅ Copied: controller/NotificationBanner.js
✅ Copied: controller/TileCounter.js
✅ Copied: i18n/i18n.properties
... (10 files total)

✅ Deployment folder created successfully!
📁 Location: deploy-sap/

📝 Note: Preload bundle excluded (contains hyphens incompatible with SAP BSP)
   UI5 will load individual files - works perfectly, just slightly slower on first load.
```

**Result**: `deploy-sap/` folder with 10 clean files ready for SE80 import

#### Step 2: Upload to BSP Application

**Transaction**: SE80 → Repository Browser

1. **Create BSP Application**:

   ```
   - SE80 → Repository Browser (first dropdown)
   - Select: "BSP Application" from object type dropdown
   - Enter name in field: ZNOTIFY_BANNER2
   - Click "Create" button (F5)
   - Popup appears:
     * Application Name: ZNOTIFY_BANNER2
     * Description: Global Notification Banner
     * Package: ZNOTIFY (or $TMP for local)
   - Click Save (popup closes)
   - BSP Application created with MIME Objects folder automatically
   ```

2. **Locate MIME Objects Folder** (already exists):

   ```
   - SE80 → Display BSP Application: ZNOTIFY_BANNER2
   - Expand tree: ZNOTIFY_BANNER2
   - You should see:
     ├── ZNOTIFY_BANNER2
     │   ├── Pages
     │   ├── Page Fragments
     │   └── MIME Objects  ← This folder already exists!

   ⚠️ NOTE: MIME Objects folder is created AUTOMATICALLY
   You do NOT need to create it manually!
   ```

3. **Import UI5 Files as MIME Objects (23 files total)**:

   ⚠️ **IMPORTANT**: SE80 import dialog does NOT support folder selection.
   You must import files ONE BY ONE or use multi-select (Ctrl+Click).

   **Complete File List from `dist/` folder**:

   **Step 3.1: Import Root Files** (8 files)

   ```
   Right-click on "MIME Objects" → "Import" → "MIME Objects"
   Navigate to: dist/ folder
   Select these files (use Ctrl+Click for multi-select):

   1. Component.js               (MIME type: application/javascript)
   2. Component-dbg.js           (MIME type: application/javascript)
   3. Component-preload.js       (MIME type: application/javascript) ⭐ CRITICAL
   4. Component-preload.js.map   (MIME type: application/json)
   5. Component.js.map           (MIME type: application/json)
   6. index.html                 (MIME type: text/html) ⭐ CRITICAL
   7. manifest.json              (MIME type: application/json) ⭐ CRITICAL
   8. sap_fiori_notification_banner.zip (MIME type: application/zip) - OPTIONAL

   Click "Import" → Files uploaded to MIME Objects root
   ```

   **Step 3.2: Create Subfolder and Import controller/** (9 files)

   ```
   Right-click on "MIME Objects" → "Create" → "Folder"
   Folder Name: controller

   Right-click on "controller" folder → "Import" → "MIME Objects"
   Navigate to: dist/controller/ folder
   Select all 9 files:

   1. NotificationBanner.js      (MIME type: application/javascript) ⭐ CRITICAL
   2. NotificationBanner-dbg.js  (MIME type: application/javascript)
   3. NotificationBanner.js.map  (MIME type: application/json)
   4. TileCounter.js             (MIME type: application/javascript) ⭐ CRITICAL
   5. TileCounter-dbg.js         (MIME type: application/javascript)
   6. TileCounter.js.map         (MIME type: application/json)
   7. View1.controller.js        (MIME type: application/javascript)
   8. View1-dbg.controller.js    (MIME type: application/javascript)
   9. View1.controller.js.map    (MIME type: application/json)

   Click "Import" → Files uploaded to controller/ subfolder
   ```

   **Step 3.3: Create Subfolder and Import css/** (1 file)

   ```
   Right-click on "MIME Objects" → "Create" → "Folder"
   Folder Name: css

   Right-click on "css" folder → "Import" → "MIME Objects"
   Navigate to: dist/css/ folder
   Select:

   1. style.css                  (MIME type: text/css)

   Click "Import"
   ```

   **Step 3.4: Create Subfolder and Import i18n/** (1 file)

   ```
   Right-click on "MIME Objects" → "Create" → "Folder"
   Folder Name: i18n

   Right-click on "i18n" folder → "Import" → "MIME Objects"
   Navigate to: dist/i18n/ folder
   Select:

   1. i18n.properties            (MIME type: text/plain) ⭐ CRITICAL

   Click "Import"
   ```

   **Step 3.5: Create Subfolder and Import model/** (3 files)

   ```
   Right-click on "MIME Objects" → "Create" → "Folder"
   Folder Name: model

   Right-click on "model" folder → "Import" → "MIME Objects"
   Navigate to: dist/model/ folder
   Select all 3 files:

   1. models.js                  (MIME type: application/javascript)
   2. models-dbg.js              (MIME type: application/javascript)
   3. models.js.map              (MIME type: application/json)

   Click "Import"
   ```

   **Step 3.6: Create Subfolder and Import view/** (1 file)

   ```
   Right-click on "MIME Objects" → "Create" → "Folder"
   Folder Name: view

   Right-click on "view" folder → "Import" → "MIME Objects"
   Navigate to: dist/view/ folder
   Select:

   1. View1.view.xml             (MIME type: application/xml)

   Click "Import"
   ```

   **📊 Import Summary**:

   ```
   Total Files: 23 (excluding .zip)
   Critical Files (⭐): 6
   - Component-preload.js (minified bundle)
   - index.html (entry point)
   - manifest.json (app descriptor)
   - NotificationBanner.js (banner logic)
   - TileCounter.js (tile counter logic)
   - i18n.properties (translations)

   Folder Structure After Import:
   ZNOTIFY_BANNER2/
   └── MIME Objects/
       ├── Component.js
       ├── Component-dbg.js
       ├── Component-preload.js ⭐
       ├── Component-preload.js.map
       ├── Component.js.map
       ├── index.html ⭐
       ├── manifest.json ⭐
       ├── controller/
       │   ├── NotificationBanner.js ⭐
       │   ├── NotificationBanner-dbg.js
       │   ├── NotificationBanner.js.map
       │   ├── TileCounter.js ⭐
       │   ├── TileCounter-dbg.js
       │   ├── TileCounter.js.map
       │   ├── View1.controller.js
       │   ├── View1-dbg.controller.js
       │   └── View1.controller.js.map
       ├── css/
       │   └── style.css
       ├── i18n/
       │   └── i18n.properties ⭐
       ├── model/
       │   ├── models.js
       │   ├── models-dbg.js
       │   └── models.js.map
       └── view/
           └── View1.view.xml
   ```

4. **Set MIME Type** (auto-detected but verify):

   ```
   - .js files: application/javascript
   - .json files: application/json
   - .html files: text/html
   - .css files: text/css
   - .properties files: text/plain
   ```

5. **Activate All Resources**:

   ```
   - Right-click on ZNOTIFY_BANNER2 root
   - Mass Activate → Object List
   - Select all MIME objects
   - Activate
   ```

**✅ Verification**:

```
SE80 → Display BSP Application: ZNOTIFY_BANNER2
→ Expand "MIME Objects" folder
→ All files should show green traffic light (active status)
→ Test URL: https://your-system:port/sap/bc/bsp/sap/znotify_banner2/index.html
```

**📝 Important Notes**:

- BSP application name must be lowercase in URL: `znotify_banner2` (not ZNOTIFY_BANNER2)
- Full URL pattern: `/sap/bc/bsp/sap/<app_name_lowercase>/<file_name>`
- If files not visible: Check MIME Objects folder, not Pages/Views
- If 404 error: Check ICF service `/sap/bc/bsp` is active in SICF

---

## 🚀 Fiori Launchpad Configuration

After deploying the application to SAP, you need to configure the Fiori Launchpad to display the notification management tile and make it accessible to users.

### 📖 Complete Configuration Guide

For detailed step-by-step instructions, see the dedicated guide:

**➡️ [FLP_CONFIGURATION.md](./FLP_CONFIGURATION.md)**

This comprehensive guide includes:
- Architecture overview and tile design
- Prerequisites checklist
- Step-by-step configuration (Target Mapping, Catalog, Dynamic Tile, Group, Roles)
- Authorization setup (PFCG, SU01)
- Troubleshooting and verification procedures

### 🎯 Quick Overview

The FLP configuration creates a **single dynamic tile** that serves two purposes:

1. **Visual Feedback** (all users):
   - Shows active notification statistics
   - Color-coded by severity (🔴 RED | 🟡 ORANGE | 🟢 GREEN)
   - Auto-refreshes every 60 seconds

2. **Admin Interface** (on click):
   - Opens full CRUD table for notification management
   - Only accessible to users with proper authorization

### 📋 Configuration Steps Summary

1. **Create Target Mapping** (`/UI2/FLPD_CUST`)
   - Semantic Object: `NotificationBanner`
   - Action: `display`

2. **Create Dynamic Tile** with service URL:
   - Service: `/sap/bc/rest/zcl_notif_rest/stats`
   - Refresh: 60 seconds

3. **Assign to Group** and configure role-based access

4. **Test** in Fiori Launchpad (`/sap/bc/ui2/flp`)

### ⚠️ Prerequisites

Before configuring FLP, ensure:
- ✅ Backend deployed (database tables, REST service active)
- ✅ Frontend deployed (BSP application `ZNOTIFY_BANNER2` available)
- ✅ `/sap/bc/rest/zcl_notif_rest/stats` endpoint returns valid OData V2 format
- ✅ You have authorization for `/UI2/FLPD_CUST`, `PFCG`, and `SU01`

---

## 🧪 Testing

For comprehensive testing procedures, see the dedicated testing guide:

**➡️ [TESTING_GUIDE.md](./TESTING_GUIDE.md)**

This guide includes detailed test cases for:

### Backend Testing
- [Test 1: Database Table](./TESTING_GUIDE.md#test-1-database-table) - Verify table structure and F4 value helps
- [Test 2: CDS View](./TESTING_GUIDE.md#test-2-cds-view) - Verify view definition and filtering logic
- [Test 3: REST API](./TESTING_GUIDE.md#test-3-rest-api) - Test GET, POST endpoints and stats API

### Frontend Testing
- [Test 1: Display Modes](./TESTING_GUIDE.md#test-1-display-modes) - BANNER, TOAST, BOTH, SILENT modes
- [Test 2: Tile Counter](./TESTING_GUIDE.md#test-2-tile-counter) - Dynamic tile updates and color coding
- [Test 3: Multi-Notification Navigation](./TESTING_GUIDE.md#test-3-multi-notification-navigation) - Navigation arrows and counter
- [Test 4: Target Audience Filtering](./TESTING_GUIDE.md#test-4-target-audience-filtering-sap-standard-roles-only) - Role-based visibility (ALL, ADMIN, DEVELOPER)

### Performance Testing
- [Test 1: Polling Performance](./TESTING_GUIDE.md#test-1-polling-performance) - 30-second polling verification
- [Test 2: Large Data Set](./TESTING_GUIDE.md#test-2-large-data-set) - 50+ notifications performance
- [Test 3: Error Recovery](./TESTING_GUIDE.md#test-3-error-recovery) - Circuit breaker and backend failure handling

---

## 🔧 Troubleshooting

### Issue 1: F4 Help Not Working

**Symptom**: Pressing F4 on MESSAGE_TYPE/SEVERITY/DISPLAY_MODE shows no values

**Cause**: Domains or data elements not activated

**Solution**:
1. SE11 → Check domain status (ZDOMAIN_MSG_TYPE, etc.)
2. If inactive: Activate domain
3. SE11 → Check data element status (ZNOTIFY_MSG_TYPE, etc.)
4. If inactive: Activate data element
5. SE14 → ZTNOTIFY_MSGS → Adjust table (if needed)

**Verification**:
```
SM30 → ZTNOTIFY_MSGS → Create entry → Press F4 on MESSAGE_TYPE
Expected: Dropdown with URGENT, INFO, TIP, SUCCESS, MAINT, WARNING
```

---

### Issue 2: REST API Returns 404

**Symptom**: GET request to `/sap/bc/rest/zcl_notif_rest/` returns 404 Not Found

**Cause**: SICF service not activated

**Solution**:
1. SICF → Navigate to `/default_host/sap/bc/rest/zcl_notif_rest`
2. Check service status (should have green traffic light)
3. If inactive: Right-click → Activate Service
4. Check Handler List contains `ZCL_NOTIFICATION_REST`

**Verification**:
```bash
curl https://your-system.com/sap/bc/rest/zcl_notif_rest/
Expected: {"notifications": [...]}
```

---

### Issue 3: Tile Counter Not Updating

**Symptom**: FLP tile shows "0 Active" despite active notifications

**Cause**: /stats endpoint not accessible or returning wrong format

**Solution**:
1. Test endpoint directly:
   ```javascript
   jQuery.ajax({
       url: "/sap/bc/rest/zcl_notif_rest/stats",
       type: "GET",
       success: console.log
   });
   ```
2. Expected response:
   ```json
   {
       "total": 10,
       "high_count": 3,
       "medium_count": 5,
       "low_count": 2
   }
   ```
3. If response is wrong: Check ZCL_NOTIFICATION_MANAGER → get_statistics method
4. Ensure method is activated (SE24)

**Verification**:
- Open FLP
- Check tile shows correct counts
- Wait 60 seconds → Verify auto-update works

---

### Issue 4: Toast Not Appearing

**Symptom**: DISPLAY_MODE = TOAST but no toast notification appears

**Cause**: Browser cache or JavaScript error

**Solution**:
1. Hard refresh browser: Ctrl+F5 (Windows) / Cmd+Shift+R (Mac)
2. Clear browser cache completely
3. Check browser console for JavaScript errors
4. Verify sap.m.MessageToast is loaded:
   ```javascript
   console.log(typeof sap.m.MessageToast); // Should be "function"
   ```

**Verification**:
1. Create notification with DISPLAY_MODE: TOAST
2. Open Fiori app
3. Expected: Toast appears at bottom-right for 5 seconds

---

### Issue 5: Banner Not Closing

**Symptom**: Clicking close button (X) doesn't dismiss banner

**Cause**: Event handler not attached

**Solution**:
1. Check browser console for errors
2. Verify Component.js is loaded correctly
3. Check NotificationBanner.js contains close handler:
   ```javascript
   onCloseBanner: function() {
       this.byId("notificationBanner").setVisible(false);
   }
   ```

**Verification**:
- Click close button → Banner should disappear immediately

---

### Issue 6: Authorization Field Not Found in SU21

**Symptom**: When creating authorization object Z_NOTIFY in SU21, field ZNOTIFY_TP doesn't appear in the available fields list

**Cause**: Authorization field not created in SU20 (Maintain Authorization Fields)

**Solution**:
1. **Create authorization field in SU20** (NOT SE11):
   ```
   Transaction: SU20
   → New Entries
   → Field Name: ZNOTIFY_TP
   → Data Element: ZNOTIFY_MSG_TYPE
   → Save
   ```

2. **Verify the field exists**:
   ```
   Transaction: SU20
   → Search for ZNOTIFY_TP
   → Should show: Data Element = ZNOTIFY_MSG_TYPE
   ```

3. **Return to SU21 and add field**:
   ```
   Transaction: SU21
   → BC_A → Z_NOTIFY
   → New Entries in Fields section
   → Authorization Field: ZNOTIFY_TP
   → Press Enter (system populates Data Element automatically)
   → Save
   ```

**Verification**:
- SU20 → Display field ZNOTIFY_TP → Should exist
- SU21 → Z_NOTIFY → Should show 2 fields (ACTVT, ZNOTIFY_TP)
- PFCG → Test role → F4 on ZNOTIFY_TP → Should show 6 values

**Common Mistakes**:
- ❌ Creating field in SE11 as "Data Element" → Won't appear in SU21
- ❌ Creating field in SE11 as "Authorization Object Field" → Wrong transaction, use SU20
- ✅ Creating field in SU20 referencing data element ZNOTIFY_MSG_TYPE → Correct!

---

### Issue 7: Wrong Display Mode Applied

**Symptom**: DISPLAY_MODE = SILENT but banner appears anyway

**Cause**: Default value not set or migration script not run

**Solution**:
1. SE16 → ZTNOTIFY_MSGS → Check DISPLAY_MODE field values
2. If NULL or empty: Run update:
   ```sql
   UPDATE ztnotify_msgs
   SET display_mode = 'BANNER'
   WHERE display_mode IS NULL OR display_mode = ''
   ```
3. Check ZCL_NOTIFICATION_MANAGER → get_notifications logic:
   ```abap
   IF ls_notification-display_mode = 'SILENT'.
     CONTINUE. " Skip this notification
   ENDIF.
   ```

**Verification**:
- Create notification with each display mode
- Verify correct behavior (BANNER stays, TOAST auto-dismisses, BOTH shows both, SILENT logs only)

---

## 🛠️ Maintenance

### Regular Maintenance Tasks

#### Daily
- Monitor error logs (ST22, SM21)
- Check REST API response times
- Verify tile counter accuracy

#### Weekly
- Review active notifications (SE16 → ZTNOTIFY_MSGS)
- Clean up expired notifications:
  ```sql
  DELETE FROM ztnotify_msgs
  WHERE end_date < sy-datum
  ```

#### Monthly
- Analyze notification statistics
- Update message templates
- Review target audience filters
- Performance tuning if needed

---

### Backup and Recovery

#### Backup Table Data

**Transaction**: SE16 (Data Browser)

```sql
-- Export to local file
SE16 → ZTNOTIFY_MSGS → Download → Spreadsheet
```

**Alternative: ABAP Report**
```abap
REPORT z_backup_notifications.

DATA: lt_notifications TYPE TABLE OF ztnotify_msgs,
      lv_filename TYPE string.

SELECT * FROM ztnotify_msgs INTO TABLE lt_notifications.

lv_filename = '/tmp/notifications_backup_' && sy-datum && '.txt'.

OPEN DATASET lv_filename FOR OUTPUT IN TEXT MODE.
LOOP AT lt_notifications INTO DATA(ls_notif).
  TRANSFER ls_notif TO lv_filename.
ENDLOOP.
CLOSE DATASET lv_filename.

WRITE: / 'Backup completed:', lv_filename.
```

#### Restore Data

```sql
-- Transaction: SE16N (direct table update mode)
1. SE16N → ZTNOTIFY_MSGS
2. Upload from file
3. Verify data integrity
```

---

### Monitoring and Logging

#### Enable Debug Logging

**Browser Console**:
```javascript
// Enable debug mode
localStorage.setItem("notificationDebug", "true");

// Check logs
console.log("Notification polling:", data);
```

**ABAP Debug**:
```abap
" Set breakpoint in ZCL_NOTIFICATION_MANAGER
BREAK-POINT.

" Enable trace
ST05 → Activate SQL Trace
```

---

## 📞 Support

- **📧 Email**: [gabriele.rendina@lutech.it](mailto:gabriele.rendina@lutech.it)
- **📖 Documentation**: [README.md](../README.md)
- **🎯 Admin UI Guide**: [ADMIN_UI_DISPLAY_MODE.md](./ADMIN_UI_DISPLAY_MODE.md)
- **🏗️ Custom Domains**: [domains/README.md](../abap/domains/README.md)

---

**Deployment Time Estimate**: 4-6 hours
**Recommended Window**: Non-peak hours
**Downtime Required**: None (zero-downtime deployment)

---

**Last Updated**: January 30, 2025
**Version**: 1.1.0
