# 🚀 Complete Deployment Guide - SAP Fiori Global Notification Banner

**Version**: 1.1.0
**Last Updated**: January 30, 2025

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

```bash
# Build the application
npm run build

# Verify build output
ls -lh dist/sap_fiori_notification_banner.zip
# Expected: ~27KB file
```

**Expected Results:**
- ✅ Build completes in ~20-30 seconds without errors
- ✅ `dist/sap_fiori_notification_banner.zip` created (~27KB)
- ✅ `dist/Component-preload.js` generated and minified

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

##### Step 7.1.3: Create Authorization Field ZNOTIFY_TYP

**💡 SMART APPROACH**: We'll reuse the existing data element `ZNOTIFY_MSG_TYPE` created in Step 2!

6. **Open Transaction SE11** (in a new session, keep SU21 open)
   - Enter transaction code: `/nse11`
   - Select: **"Data type"** radio button
   - Enter: `ZNOTIFY_TYP` (**Must start with Z!**)
   - Click **"Create"**

7. **Select Authorization Field Type**
   - A popup appears: "Create Data Type"
   - Select: **"Authorization Object Field"**
   - Click **"Continue"** (green checkmark)

8. **Define Authorization Field Using Existing Data Element**
   ```
   Short Description: Notification Type Authorization
   Data Element: ZNOTIFY_MSG_TYPE  ⭐ REUSE FROM TABLE!
   ```
   - **IMPORTANT**: By selecting `ZNOTIFY_MSG_TYPE`, you automatically get:
     - ✅ Data Type: CHAR(12) (from domain ZDOMAIN_MSG_TYPE)
     - ✅ Field labels: "Message Type"
     - ✅ F4 help with 6 fixed values (URGENT, INFO, MAINT, WARNING, TIP, SUCCESS)
     - ✅ Check table values from domain

9. **Verify Field Properties** (auto-populated from ZNOTIFY_MSG_TYPE)
   - Data Element: `ZNOTIFY_MSG_TYPE`
   - Domain: `ZDOMAIN_MSG_TYPE` (via data element)
   - Data Type: CHAR
   - Length: 12
   - Short field label: Msg Type
   - Field Label: Message Type
   - Header: Type

10. **Add Authorization-Specific Documentation**
    - Click **"Documentation"** button in toolbar
    - Enter additional authorization documentation:
      ```
      Authorization Field: Notification Type

      This authorization field restricts access to specific notification message types.
      Uses the same domain and values as ZTNOTIFY_MSGS.MESSAGE_TYPE field.

      Allowed values (from domain ZDOMAIN_MSG_TYPE):
      * - All notification types (wildcard - use in authorization checks)
      URGENT - Urgent system notifications
      INFO - Informational messages
      MAINT - Maintenance announcements
      WARNING - Warning messages
      TIP - Tips and suggestions
      SUCCESS - Success confirmations

      Authorization Examples:
      - Admin with full access: ZNOTIFY_TYP = *
      - User creating only INFO: ZNOTIFY_TYP = INFO
      - User managing URGENT+MAINT: Create 2 entries (URGENT, MAINT)

      Note: This field shares the same domain as MESSAGE_TYPE in table ZTNOTIFY_MSGS,
      ensuring consistency between data model and authorization model.
      ```
    - **Save** the documentation
    - Click **"Back"** to return to field definition

11. **Save and Activate Authorization Field**
    - Click **"Save"** (Ctrl+S)
    - Select package: Same package as ZNOTIFY_MSG_TYPE (e.g., `ZNOTIFY` or `$TMP`)
    - Click **"Activate"** (Ctrl+F3)
    - Close SE11 window

##### Step 7.1.4: Add Field 2 - ZNOTIFY_TYP to Authorization Object

12. **Return to SU21 Window** (should still be open)
    - In the "Fields" section, click **"New Entries"** again (or press F5)
    - Enter field details:
      ```
      Authorization Field: ZNOTIFY_TYP
      ```
    - Press Enter - the system will populate:
      - Data Element: `ZNOTIFY_MSG_TYPE`
      - Short Text: "Notification Type Authorization" (from SE11 definition)

13. **Save Authorization Object**
    - Click **"Save"** button (Ctrl+S)
    - Select package: `$TMP` (local object) or your custom package (e.g., `ZNOTIFY`)
    - Enter transport request if using custom package

14. **Check Authorization Object**
    - The object should now show 2 fields:
      1. ACTVT (Activity)
      2. ZNOTIFY_TYP (Notification Type Authorization)

##### Step 7.1.5: Verification

**✅ Verification Steps**:

1. **Check Authorization Field**:
   ```
   SE11 → Data Type → ZNOTIFY_TYP → Display
   Expected:
   - Type: Authorization Object Field
   - Data Element: ZNOTIFY_MSG_TYPE
   - Domain: ZDOMAIN_MSG_TYPE (via data element)
   - Data Type: CHAR(12)
   - Short Description: Notification Type Authorization
   - Documentation exists
   - Fixed Values: From domain (URGENT, INFO, MAINT, WARNING, TIP, SUCCESS)
   ```

2. **Check Authorization Object**:
   ```
   SU21 → Authorization Objects → BC_A → Z_NOTIFY → Display
   Expected:
   - Object: Z_NOTIFY
   - Object Class: BC_A
   - 2 Fields:
     1. ACTVT (Activity) - Data Element: ACTIV_AUTH
     2. ZNOTIFY_TYP (Notification Type Authorization) - Data Element: ZNOTIFY_MSG_TYPE
   ```

3. **Test in PFCG**:
   - Transaction PFCG → Create test role
   - Authorizations → Change Authorization Data → Manually
   - Search for "Z_NOTIFY"
   - **Expected**: Object appears with 2 fields (ACTVT and ZNOTIFY_TYP)
   - **Test F4 Help**: Click on ZNOTIFY_TYP value field → Should show 6 values from domain

**Authorization Values for ACTVT**:
- `01` - Create notifications (POST)
- `02` - Modify notifications (PUT)
- `03` - Display notifications (GET)
- `06` - Delete notifications (DELETE)

**Authorization Values for ZNOTIFY_TYP** (from domain ZDOMAIN_MSG_TYPE):
- `*` - All notification types (wildcard - for admin authorization)
- `URGENT` - Only urgent notifications
- `INFO` - Only informational notifications
- `MAINT` - Only maintenance notifications
- `WARNING` - Only warning notifications
- `TIP` - Only tips and suggestions
- `SUCCESS` - Only success messages

**✨ Benefits of Using ZNOTIFY_MSG_TYPE Data Element**:
- ✅ Reuses existing domain with fixed values (no duplication)
- ✅ Automatic F4 help in PFCG with domain values
- ✅ Consistency between table field (MESSAGE_TYPE) and authorization field (ZNOTIFY_TYP)
- ✅ Single point of maintenance: Update domain → affects both table and authorization
- ✅ Type-safe: CHAR(12) length matches MESSAGE_TYPE field exactly

**🎯 Common Issues & Solutions**:

| Issue | Solution |
|-------|----------|
| "Authorization field ZNOTIFY_TYP does not exist" | Create field in SE11 first (Step 7.1.3) with Z prefix |
| "Field ZNOTIFY_TYP not found" | Activate the field in SE11 (Step 7.1.3, item 11) |
| "Cannot add field to object" | Ensure SE11 field is activated and of type "Authorization Object Field" |
| "Field does not appear in PFCG" | Save and re-open PFCG role, or restart transaction |
| "Data element ZNOTIFY_MSG_TYPE not found" | Ensure Step 2 (Create Data Elements) was completed and activated |
| "F4 help shows no values in PFCG" | Check domain ZDOMAIN_MSG_TYPE has fixed values defined |

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
   ZNOTIFY_TYP: *           (All notification types)
   ```
   **Purpose**: Application-specific authorization for notification management
   **Note**: This is a CUSTOM object created in Step 7.1
   **Field Details**:
   - ACTVT: Standard SAP activity field (01=Create, 02=Change, 03=Display, 06=Delete)
   - ZNOTIFY_TYP: Custom field using ZNOTIFY_MSG_TYPE data element (domain: ZDOMAIN_MSG_TYPE)
   - F4 Help: Shows 6 values (URGENT, INFO, MAINT, WARNING, TIP, SUCCESS) + wildcard (*)

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
   ICF_FIELD: /sap/bc/rest/zcl_notification_rest*
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

### Option A: Manual Deployment to BSP Application

#### Step 1: Build the Application

```bash
cd sap-fiori-notification-banner
npm install
npm run build
```

**Expected Output**:
```
ℹ info graph Initializing module collection...
ℹ info build Building...
ℹ info minify Minifying resources...
✔ Build succeeded
✔ Created dist/sap_fiori_notification_banner.zip
```

#### Step 2: Upload to BSP Application

**Transaction**: SE80 → Repository Browser

1. **Create BSP Application**:
   ```
   Object Type: BSP Application
   Application: ZNOTIFY_BANNER
   Description: Global Notification Banner
   ```

2. **Import Resources**:
   - Right-click on ZNOTIFY_BANNER → Import → File System
   - Select all files from `dist/` folder
   - Import the following:
     ```
     Component-preload.js
     manifest.json
     index.html
     i18n/i18n.properties
     css/style.css
     controller/NotificationBanner.js
     controller/TileCounter.js
     view/View1.view.xml
     ```

3. **Activate All Resources**:
   - Select all imported files
   - Right-click → Activate

**✅ Verification**:
- SE80 → Display ZNOTIFY_BANNER → Check all files are active
- Test URL: `https://your-system.com/sap/bc/bsp/sap/znotify_banner/index.html`

---

### Option B: Automated Deployment with Fiori Tools

#### Prerequisites

```bash
npm install -g @sap/ux-ui5-tooling
npm install -g @ui5/cli
```

#### Deploy Command

```bash
npx fiori deploy
```

**Deployment Configuration** (when prompted):
```
Target System: [Your S/4HANA system]
BSP Application Name: ZNOTIFY_BANNER
Package: $TMP (or your custom package)
Transport Request: [Your TR number]
```

**Expected Output**:
```
✔ Deployment successful
✔ BSP application ZNOTIFY_BANNER created
✔ All resources uploaded and activated
```

**✅ Verification**:
- SE80 → Display ZNOTIFY_BANNER
- Test application URL

---

## 🚀 Fiori Launchpad Configuration

### Step 1: Create Tile for Notification Management

**Transaction**: /UI2/FLPD_CUST (Fiori Launchpad Designer)

#### 1.1 Create Semantic Object

```
Semantic Object: NotificationBanner
Description: Global Notification Banner
```

#### 1.2 Create Tile

```
Tile ID: ZNOTIFY_BANNER_TILE
Title: Notifications
Subtitle: Active System Messages
Icon: sap-icon://message-information
Type: Dynamic
```

**Dynamic Tile Configuration**:
```javascript
{
  "service_url": "/sap/bc/rest/zcl_notification_rest/stats",
  "service_refresh_interval": 60,
  "title": "Notifications",
  "number_unit": "Active",
  "info": "{{highCount}}H|{{mediumCount}}M|{{lowCount}}L",
  "info_state": "{{tileColor}}"
}
```

**Tile Color Logic** (based on HIGH count):
- RED: high_count >= 3
- ORANGE: high_count 1-2
- GREEN: high_count = 0

#### 1.3 Create Target Mapping

```
Semantic Object: NotificationBanner
Action: display
URL: /sap/bc/ui5_ui5/sap/znotify_banner/index.html
```

#### 1.4 Assign to Catalog

```
Catalog ID: ZNOTIFY_CATALOG
Title: System Notifications
Description: Global notification management
```

#### 1.5 Assign to Group

```
Group ID: SYSTEM_ADMIN
Title: System Administration
Priority: High
```

**✅ Verification**:
- Open Fiori Launchpad
- Check tile appears in System Administration group
- Click tile → Should open notification management app
- Tile should update every 60 seconds with real-time statistics

---

## 🧪 Testing

### Backend Testing

#### Test 1: Database Table

**Transaction**: SE16 (Data Browser)

```sql
-- View table structure
Table: ZTNOTIFY_MSGS

-- Test F4 help
1. SM30 → ZTNOTIFY_MSGS → Create new entry
2. Click on MESSAGE_TYPE field → Press F4
3. Expected: Dropdown with 6 values (URGENT, INFO, TIP, SUCCESS, MAINT, WARNING)
4. Click on SEVERITY field → Press F4
5. Expected: Dropdown with 3 values (HIGH, MEDIUM, LOW)
6. Click on DISPLAY_MODE field → Press F4
7. Expected: Dropdown with 4 values (BANNER, TOAST, BOTH, SILENT)
```

#### Test 2: CDS View

**Transaction**: SE11 (ABAP Dictionary)

```sql
-- Display CDS view
View: ZTNOTIFY_MESSAGES (DDLS)
SQL View: ZNOTIFY_MSG

-- Test data retrieval
SE16 → ZNOTIFY_MSG
Expected: Only active notifications where:
  - active = 'X'
  - start_date <= today
  - end_date >= today
```

#### Test 3: REST API

**GET /sap/bc/rest/zcl_notification_rest/**

```javascript
jQuery.ajax({
    url: "/sap/bc/rest/zcl_notification_rest/",
    type: "GET",
    success: function(data) {
        console.log("Notifications:", data);
        // Expected: {"notifications": [...]}
    }
});
```

**GET /sap/bc/rest/zcl_notification_rest/stats**

```javascript
jQuery.ajax({
    url: "/sap/bc/rest/zcl_notification_rest/stats",
    type: "GET",
    success: function(data) {
        console.log("Statistics:", data);
        // Expected: {"total": 10, "high_count": 3, "medium_count": 5, "low_count": 2}
    }
});
```

**POST /sap/bc/rest/zcl_notification_rest/**

```javascript
jQuery.ajax({
    url: "/sap/bc/rest/zcl_notification_rest/",
    type: "POST",
    contentType: "application/json",
    data: JSON.stringify({
        message_type: "MAINT",
        severity: "MEDIUM",
        title: "Scheduled Maintenance",
        message_text: "System will be unavailable Sunday 2-4 AM",
        start_date: "20250401",
        end_date: "20250430",
        target_users: "ALL",
        active: "X",
        display_mode: "TOAST"
    }),
    success: function(data) {
        console.log("Created:", data);
    }
});
```

---

### Frontend Testing

#### Test 1: Display Modes

Create test notifications with different display modes:

**Test Case 1: BANNER Mode**
```
Transaction: SM30 → ZTNOTIFY_MSGS
Create entry:
- MESSAGE_TYPE: URGENT
- SEVERITY: HIGH
- TITLE: Test Banner
- MESSAGE_TEXT: This is a banner notification
- DISPLAY_MODE: BANNER
- ACTIVE: X
```

**Expected Result**:
- Fixed banner appears at top of all Fiori apps
- Red color (HIGH severity)
- User must click close button to dismiss
- Banner stays until manually closed

**Test Case 2: TOAST Mode**
```
Create entry:
- MESSAGE_TYPE: INFO
- SEVERITY: LOW
- TITLE: Test Toast
- MESSAGE_TEXT: This is a toast notification
- DISPLAY_MODE: TOAST
- ACTIVE: X
```

**Expected Result**:
- Toast appears at bottom-right
- Blue color (LOW severity)
- Auto-dismisses after 5 seconds
- Slide-in animation

**Test Case 3: BOTH Mode**
```
Create entry:
- MESSAGE_TYPE: MAINT
- SEVERITY: MEDIUM
- TITLE: Test Both
- MESSAGE_TEXT: This appears as both banner and toast
- DISPLAY_MODE: BOTH
- ACTIVE: X
```

**Expected Result**:
- Both banner AND toast appear simultaneously
- Orange color (MEDIUM severity)
- Banner stays until closed, toast auto-dismisses

**Test Case 4: SILENT Mode**
```
Create entry:
- MESSAGE_TYPE: TIP
- SEVERITY: LOW
- TITLE: Test Silent
- MESSAGE_TEXT: This is logged but not displayed
- DISPLAY_MODE: SILENT
- ACTIVE: X
```

**Expected Result**:
- No UI display (banner or toast)
- Check browser console: "Silent notification logged: Test Silent"
- Verify with: GET /sap/bc/rest/zcl_notification_rest/log

---

#### Test 2: Tile Counter

**Setup**:
Create multiple notifications with different severities:
```
3 notifications with SEVERITY: HIGH
5 notifications with SEVERITY: MEDIUM
2 notifications with SEVERITY: LOW
```

**Expected FLP Tile Display**:
```
┌─────────────────────────┐
│   10  Active            │ ← Total count
│   🔔                    │ ← Icon
│   3H|5M|2L             │ ← Breakdown
└─────────────────────────┘
   RED background          ← Color (≥3 HIGH = RED)
```

**Color Coding Rules**:
- **RED**: high_count >= 3
- **ORANGE**: high_count 1-2
- **GREEN**: high_count = 0

**Test Steps**:
1. Open Fiori Launchpad
2. Locate notification tile
3. Verify count: "10 Active"
4. Verify breakdown: "3H|5M|2L"
5. Verify color: RED
6. Wait 60 seconds → Verify auto-update

**Dynamic Update Test**:
1. Delete 2 HIGH notifications (SM30)
2. Wait 60 seconds
3. Expected: Tile updates to "8 Active | 1H|5M|2L" with ORANGE color

---

#### Test 3: Multi-Notification Navigation

**Setup**:
Create 3 active notifications:
```
1. URGENT - System Downtime - HIGH
2. MAINT - Scheduled Maintenance - MEDIUM
3. TIP - New Feature Available - LOW
```

**Test Steps**:
1. Open any Fiori app
2. Banner appears with first notification (System Downtime)
3. Click **Next** arrow (→)
4. Expected: Shows "Scheduled Maintenance" (2/3)
5. Click **Next** arrow (→)
6. Expected: Shows "New Feature Available" (3/3)
7. Click **Previous** arrow (←)
8. Expected: Returns to previous notification
9. Counter shows: "2 / 3"

---

#### Test 4: Target Audience Filtering (SAP Standard Roles Only)

**Test Case 1: Public Notification**
```
Transaction: SM30 → ZTNOTIFY_MSGS → Create entry
TARGET_USERS: ALL (select from F4 dropdown - 3 values available)
```
**Expected**: All users see this notification (no role check)

**Test Case 2: Administrators Only**
```
Transaction: SM30 → ZTNOTIFY_MSGS → Create entry
TARGET_USERS: ADMIN (select from F4 dropdown)
```
**Expected**: Only users with **SAP_ALL** role see this (exact match)
**Verification**:
- Check AGR_USERS table: `SELECT * FROM agr_users WHERE uname = 'USERNAME' AND agr_name = 'SAP_ALL'`
- SU01 → User → Roles tab → Should have SAP_ALL role

**Test Case 3: Developers Only**
```
Transaction: SM30 → ZTNOTIFY_MSGS → Create entry
TARGET_USERS: DEVELOPER (select from F4 dropdown)
```
**Expected**: Only users with **SAP_BR_DEVELOPER** role see this (exact match)
**Verification**:
- Check AGR_USERS table: `SELECT * FROM agr_users WHERE uname = 'USERNAME' AND agr_name = 'SAP_BR_DEVELOPER'`
- SU01 → User → Roles tab → Should have SAP_BR_DEVELOPER role

**Verification Steps**:
1. Create notification with specific TARGET_USERS value
2. Check user roles: SU01 → User → Roles tab (must have exact role name)
3. Login as user WITH required role → Should see notification
4. Login as user WITHOUT required role → Should NOT see notification
5. Check ABAP logic: zcl_notification_manager=>check_target_audience method (uses exact match, no LIKE patterns)

---

### Performance Testing

#### Test 1: Polling Performance

**Monitor**:
- Open Chrome DevTools → Network tab
- Refresh Fiori app
- Observe polling requests every 30 seconds

**Expected**:
- Request URL: `/sap/bc/rest/zcl_notification_rest/`
- Interval: 30 seconds
- Response Time: < 500ms
- Response Size: < 5KB

#### Test 2: Large Data Set

**Setup**:
Create 50 active notifications

**Expected**:
- All 50 notifications load successfully
- Navigation arrows work smoothly
- No UI freeze or lag
- Memory usage stable

#### Test 3: Error Recovery

**Simulate Backend Downtime**:
1. SICF → Deactivate `zcl_notification_rest` service
2. Open Fiori app
3. Expected: Silent failure, no error popup
4. Check console: "Circuit breaker opened after 5 failures"
5. SICF → Reactivate service
6. Wait 60 seconds
7. Expected: Circuit breaker closes, polling resumes

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

**Symptom**: GET request to `/sap/bc/rest/zcl_notification_rest/` returns 404 Not Found

**Cause**: SICF service not activated

**Solution**:
1. SICF → Navigate to `/default_host/sap/bc/rest/zcl_notification_rest`
2. Check service status (should have green traffic light)
3. If inactive: Right-click → Activate Service
4. Check Handler List contains `ZCL_NOTIFICATION_REST`

**Verification**:
```
curl https://your-system.com/sap/bc/rest/zcl_notification_rest/
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
       url: "/sap/bc/rest/zcl_notification_rest/stats",
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

### Issue 6: Wrong Display Mode Applied

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
