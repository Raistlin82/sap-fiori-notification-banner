# 🏗️ Backend Deployment Guide (ABAP)

**SAP Fiori Global Notification Banner**
**Version**: 1.3.0
**Last Updated**: October 10, 2025

---

## 📋 Table of Contents

1. [Prerequisites](#prerequisites)
2. [System Requirements](#system-requirements)
3. [Step 1: Create Custom Domains](#step-1-create-custom-domains)
4. [Step 2: Create Data Elements](#step-2-create-data-elements)
5. [Step 3: Create Database Table](#step-3-create-database-table)
6. [Step 4: Create CDS View](#step-4-create-cds-view)
7. [Step 5: Create ABAP Classes](#step-5-create-abap-classes)
8. [Step 6: Configure REST Service](#step-6-configure-rest-service)
9. [Step 7: Create Authorization Object (Optional)](#step-7-create-authorization-object-optional)
10. [Data Migration](#data-migration)
11. [Monitoring](#monitoring)

---

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
| REQUIRES_ACK    |     | CHAR1                  | CHAR      | 1      | Requires Acknowledgment (v1.3.0) |
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
- **REQUIRES_ACK** (v1.3.0): 'X' = requires user acknowledgment, blank = simple dismiss

**✅ Verification**:
- SE11 → Display ZTNOTIFY_MSGS → Check all fields exist (including REQUIRES_ACK)
- SM30 → ZTNOTIFY_MSGS → Test F4 help on MESSAGE_TYPE (should show 6 values)
- SM30 → ZTNOTIFY_MSGS → Test F4 help on TARGET_USERS (should show 3 values: ALL, ADMIN, DEVELOPER)

---

### Step 3b: Create Acknowledgment Log Table (v1.3.0) 🆕

**Transaction**: SE11 → Database Table

Create table `ZNOTIFY_ACK_LOG` to track user acknowledgments:

```
Table Name: ZNOTIFY_ACK_LOG
Short Description: Notification Acknowledgment Log
Delivery Class: A (Application Table)
```

**Fields**:

| Field Name      | Key | Data Element           | Type      | Length | Description                  |
|-----------------|-----|------------------------|-----------|--------|------------------------------|
| CLIENT          | ✅  | MANDT                  | CLNT      | 3      | Client                       |
| MESSAGE_ID      | ✅  | CHAR32                 | CHAR      | 32     | Message ID (FK to ZTNOTIFY_MSGS) |
| USERID          | ✅  | SYUNAME                | CHAR      | 12     | User Who Acknowledged        |
| ACK_TIMESTAMP   |     | TIMESTAMPL             | DEC       | 21     | Acknowledgment Timestamp     |
| CLIENT_INFO     |     | CHAR255                | CHAR      | 255    | Browser/Device Information   |

**Actions**:
1. SE11 → Database Table → Enter "ZNOTIFY_ACK_LOG" → Create
2. Short Description: "Notification Acknowledgment Log"
3. Delivery Class: A (Application Table)
4. **Add all fields as shown above**
5. **Primary Key**: MANDT + MESSAGE_ID + USERID (composite key prevents duplicates)
6. Technical Settings:
   - Data Class: APPL0 (Master Data)
   - Size Category: 2 (16,000-150,000 rows expected)
7. **Create Indexes**:
   - Index 1: MESSAGE_ID (for reporting queries)
   - Index 2: USERID (for user lookup)
8. **Save** → **Check** → **Activate**

**SQL Alternative** (for HANA systems):
```sql
CREATE COLUMN TABLE znotify_ack_log (
    mandt NVARCHAR(3) NOT NULL,
    message_id NVARCHAR(32) NOT NULL,
    userid NVARCHAR(12) NOT NULL,
    ack_timestamp DECIMAL(21,7) NOT NULL,
    client_info NVARCHAR(255),
    PRIMARY KEY (mandt, message_id, userid)
);

CREATE INDEX znotify_ack_log_msg_idx ON znotify_ack_log (message_id);
CREATE INDEX znotify_ack_log_usr_idx ON znotify_ack_log (userid);
```

**🎯 Key Points**:
- **Composite Primary Key**: Prevents same user from acknowledging twice
- **Foreign Key Relationship**: MESSAGE_ID links to ZTNOTIFY_MSGS.MESSAGE_ID
- **Audit Trail**: Records WHO, WHEN, and FROM WHICH DEVICE
- **Immutable Log**: No UPDATE/DELETE operations allowed (INSERT only)

**Populate Existing Notifications** (optional):
```sql
-- Auto-set requires_ack for critical notifications
UPDATE ztnotify_msgs
SET requires_ack = 'X'
WHERE severity = 'HIGH' AND display_mode IN ('BANNER', 'BOTH')
  AND (requires_ack IS NULL OR requires_ack = '');

UPDATE ztnotify_msgs
SET requires_ack = 'X'
WHERE message_type = 'URGENT' AND display_mode IN ('BANNER', 'BOTH')
  AND (requires_ack IS NULL OR requires_ack = '');

COMMIT;
```

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
      requires_ack,      // v1.3.0 - Acknowledgment tracking
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
- `get_active_notifications` - Retrieve active notifications for a user (filters acknowledged notifications in v1.3.0)
- `create_notification` - Create a new notification
- `update_notification` - Update an existing notification
- `deactivate_notification` - Deactivate a notification (sets active = ' ')
- `check_user_authorization` - Check if user is authorized
- **`has_user_acknowledged`** (v1.3.0) - Check if user already acknowledged a notification
- **`record_acknowledgment`** (v1.3.0) - Record user acknowledgment with duplicate prevention
- **`get_acknowledgments`** (v1.3.0) - Query acknowledgment history for reporting

**Key Type Definitions**:
```abap
" Notification structure
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
         requires_ack TYPE char1,       " Requires acknowledgment (v1.3.0)
         created_by   TYPE syuname,     " Created by user
         created_at   TYPE timestampl,  " Created timestamp
         changed_by   TYPE syuname,     " Changed by user
         changed_at   TYPE timestampl,  " Changed timestamp
       END OF ty_notification.

TYPES: tt_notifications TYPE STANDARD TABLE OF ty_notification WITH DEFAULT KEY.

" Acknowledgment structure (v1.3.0)
TYPES: BEGIN OF ty_acknowledgment,
         message_id    TYPE char32,     " FK to ZTNOTIFY_MSGS
         userid        TYPE syuname,    " User who acknowledged
         ack_timestamp TYPE timestampl, " When acknowledged
         client_info   TYPE char255,    " Browser/device info
       END OF ty_acknowledgment.

TYPES: tt_acknowledgments TYPE STANDARD TABLE OF ty_acknowledgment WITH DEFAULT KEY.
```

**Important - Modern ABAP Compliance**:
- ✅ All string types replaced with fixed CHAR types (fully-typed parameters)
- ✅ Table type uses STANDARD TABLE WITH DEFAULT KEY (required for RETURNING)
- ✅ System variables escaped with @ in SQL: `@sy-datum`, `@sy-uname`
- ✅ Method parameters escaped with @ in SQL: `@iv_message_id`, `@lv_timestamp`

**Actions**:
1. SE80 → Class Builder → Create class `ZCL_NOTIFICATION_MANAGER`
2. Copy definition and implementation from `abap/zcl_notification_manager.clas.abap` (819 lines)
3. Verify all 8 public static methods are present (5 original + 3 new acknowledgment methods)
4. **Save** → **Check** → **Activate**
5. **Important**: Ensure Eclipse ADT is used for activation (modern syntax required)

**🎯 Key Features**:
- Role-based target audience filtering (3 fixed values: ALL, ADMIN, DEVELOPER - SAP standard roles only)
- Display mode support (BANNER, TOAST, BOTH, SILENT)
- Audit trail (automatically sets created_by, created_at, changed_by, changed_at)
- Statistics calculation for tile counter
- **User acknowledgment tracking** (v1.3.0):
  - Check if user acknowledged: `has_user_acknowledged()`
  - Record acknowledgment with duplicate prevention: `record_acknowledgment()`
  - Query acknowledgment history: `get_acknowledgments()`
  - Filter acknowledged notifications in `get_active_notifications()`

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
- **`POST /acknowledge`** (v1.3.0) - Record user acknowledgment
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

  " No CORS headers: the Fiori apps are served from the same origin

  " CSRF protection: token on 'X-CSRF-Token: Fetch',
  " valid token required for POST/PUT/DELETE (403 + 'X-CSRF-Token: Required' otherwise)
  IF handle_csrf_token( lv_method ) = abap_false.
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
   - Copy definition and implementation from `abap/zcl_notification_rest.clas.abap` (760 lines)
   - Verify new method `handle_acknowledge()` is present (v1.3.0)
   - **Save** (Ctrl+S) → **Activate** (Ctrl+F3)

2. **SE80** (Alternative):
   - Class Builder → Create class `ZCL_NOTIFICATION_REST`
   - Add interface: `IF_HTTP_EXTENSION`
   - Implement method: `IF_HTTP_EXTENSION~HANDLE_REQUEST`
   - Add private method: `HANDLE_ACKNOWLEDGE` (v1.3.0)
   - Copy implementation code
   - **Save** → **Check** → **Activate**

**🎯 Key Features**:
- ✅ **IF_HTTP_EXTENSION** interface (required by SICF handlers)
- ✅ **Direct HTTP server access** via `mo_server->request/response`
- ✅ **CSRF protection** in code (standard SAP `X-CSRF-Token` handshake, v1.4.1)
- ✅ **Same-origin only**: no CORS headers are sent (v1.4.1)
- ✅ **Authorization**: create/update/delete and `GET ?all=X` require `Z_BR_ADMINISTRATOR` or `Z_NOTIFY`
- ✅ **JSON serialization** using `/UI2/CL_JSON` (standard SAP class), also for error responses
- ✅ **HTTP status codes**: 200 OK, 201 Created, 400 Bad Request, 403 Forbidden, 404 Not Found, 409 Conflict, 500 Internal Server Error
- ✅ **Query parameter handling**: `get_form_field('message_id')`, `get_form_field('all')` - the user is always taken from the session (`sy-uname`)
- ✅ **Request body parsing**: `mo_server->request->get_cdata()`
- ✅ **Response formatting**: `mo_server->response->set_cdata( lv_json )`
- ✅ **Acknowledgment endpoint** (v1.3.0):
  - POST /acknowledge with JSON body `{"message_id":"abc","client_info":"..."}`
  - Body parsed with `/UI2/CL_JSON`, unknown message IDs return 404
  - 409 Conflict for duplicate acknowledgments
  - Calls ZCL_NOTIFICATION_MANAGER=>record_acknowledgment()

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

5. **CSRF Protection** (handled in code, v1.4.1):
   - ✅ **No SICF configuration needed** - `ZCL_NOTIFICATION_REST->HANDLE_CSRF_TOKEN` implements the standard handshake
   - `GET` with header `X-CSRF-Token: Fetch` returns the token in the `X-CSRF-Token` response header
   - `POST`/`PUT`/`DELETE` without a valid token are rejected with `403` and `X-CSRF-Token: Required`
   - The Fiori apps fetch and send the token automatically (`webapp/util/CsrfAjax.js`)
   - No CORS headers are sent: the apps must be served from the same origin as the service

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
CSRF: Handled in code (X-CSRF-Token handshake)
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
Expected Response: {"d":{"number":"0","numberUnit":"Active","info":"🔴 0  🟠 0  🟢 0","infoState":"Success"}}
HTTP Status: 200 OK
```

**Test 3 - CSRF Token**:
```
URL: https://<your-server>:<port>/sap/bc/rest/zcl_notif_rest/stats?sap-client=<client>
Method: GET
Request Header: X-CSRF-Token: Fetch
Expected Response Header: X-CSRF-Token: <token>
HTTP Status: 200 OK

A POST/PUT/DELETE without this token must return 403 with header X-CSRF-Token: Required
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
- ✅ `X-CSRF-Token` header returned when requested with `X-CSRF-Token: Fetch`

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

**Hierarchy Created:**
- **SU20**: Authorization Field `ZNOTIFY_TP`
  - Uses Data Element: `ZNOTIFY_MSG_TYPE`
    - Uses Domain: `ZDOMAIN_MSG_TYPE`
      - 6 Fixed Values: `URGENT`, `INFO`, `MAINT`, `WARNING`, `TIP`, `SUCCESS`

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

