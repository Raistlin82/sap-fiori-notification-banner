# SAP Authorization Objects Reference

**Version**: 1.1.0
**Last Updated**: January 30, 2025

This document provides a comprehensive reference for all authorization objects used in the SAP Fiori Global Notification Banner application.

---

## Table of Contents

1. [Standard SAP Authorization Objects](#standard-sap-authorization-objects)
2. [Custom Authorization Objects](#custom-authorization-objects)
3. [Authorization Object Fields Reference](#authorization-object-fields-reference)
4. [Role Configuration Examples](#role-configuration-examples)
5. [Testing Authorization Setup](#testing-authorization-setup)

---

## Standard SAP Authorization Objects

These are **pre-existing SAP standard authorization objects** that do NOT need to be created. They are already present in your SAP system.

### S_TABU_DIS (Table Maintenance by Table Authorization Group)

**SAP Documentation**: Cross-Application Authorization Objects
**Object Class**: BC_A (Basis: Authorization and user administration)
**Purpose**: Controls access to tables by authorization group (DICBERCLS)

**Fields**:
- `ACTVT` (Activity): 01=Create, 02=Change, 03=Display, 06=Delete
- `DICBERCLS` (Authorization Group): Table authorization group from TDDAT

**Usage in Notification Banner**:
```
ACTVT: 01, 02, 03, 06
DICBERCLS: &NC&  (Customer namespace tables starting with Y or Z)
```

**Why Needed**: ZTNOTIFY_MSGS is a Z-table (customer namespace), protected by &NC& authorization group.

**SAP Note**: Authorization object S_TABU_DIS provides coarse-grained access control by table groups. Use together with S_TABU_NAM for specific table access.

---

### S_TABU_NAM (Table Maintenance by Table Name)

**SAP Documentation**: Cross-Application Authorization Objects
**Object Class**: BC_A
**Purpose**: Controls access to specific database tables by name

**Fields**:
- `ACTVT` (Activity): 01=Create, 02=Change, 03=Display, 06=Delete
- `TABLE` (Table Name): Name of the database table (e.g., ZTNOTIFY_MSGS)

**Usage in Notification Banner**:
```
ACTVT: 01, 02, 03, 06
TABLE: ZTNOTIFY_MSGS
```

**Why Needed**: Provides fine-grained access control for ZTNOTIFY_MSGS table specifically.

**SAP Note**: S_TABU_NAM is more specific than S_TABU_DIS. Use both for comprehensive table protection.

**Transactions Protected**:
- SM30 (Extended Table Maintenance)
- SE16 (Data Browser)
- SE16N (General Table Display)

---

### S_DEVELOP (ABAP Workbench Development Authorization)

**SAP Documentation**: Development Workbench Authorization
**Object Class**: BC_A
**Purpose**: Controls access to Repository Browser and development objects

**Fields**:
- `ACTVT` (Activity): 01=Create, 02=Change, 03=Display
- `DEVCLASS` (Development Class/Package): Package name (e.g., $TMP, ZNOTIFY)
- `OBJTYPE` (Object Type): TABL, CLAS, DDLS, FUGR, PROG, etc.
- `OBJNAME` (Object Name): Name pattern (e.g., Z*, ZTNOTIFY*)
- `P_GROUP` (Authorization Group): Developer group

**Usage in Notification Banner**:
```
ACTVT: 01, 02, 03
DEVCLASS: *, $TMP, ZNOTIFY
OBJTYPE: TABL, CLAS, DDLS, FUGR
OBJNAME: Z*, ZTNOTIFY_*
P_GROUP: *
```

**Why Needed**: Required for creating/modifying table ZTNOTIFY_MSGS, classes ZCL_NOTIFICATION_*, CDS view ZTNOTIFY_MESSAGES.

**Transactions Protected**:
- SE11 (ABAP Dictionary)
- SE24 (Class Builder)
- SE80 (Object Navigator)

---

### S_SERVICE (Service-Related Authorization Check)

**SAP Documentation**: Service Authorization for ICF/OData
**Object Class**: BC_A
**Purpose**: Controls access to HTTP/ICF services and OData services

**Fields**:
- `SRV_NAME` (Service Name): Name of the service (e.g., ZCL_NOTIFICATION_REST)
- `SRV_TYPE` (Service Type): HTTP, RFC, IWFND, IWSG
- `ACTVT` (Activity): 01=Create, 02=Change, 03=Display, 06=Delete, 16=Execute

**Usage in Notification Banner**:
```
SRV_NAME: ZCL_NOTIFICATION_REST
SRV_TYPE: HTTP
ACTVT: 01, 02, 03, 06, 16
```

**Why Needed**: Protects access to the REST API service handler class.

**Related Transactions**:
- SICF (HTTP Service Maintenance)

---

### S_ICF (ICF Service Authorization)

**SAP Documentation**: Internet Communication Framework Authorization
**Object Class**: BC_A
**Purpose**: Controls access to ICF (Internet Communication Framework) services and nodes

**Fields**:
- `ICF_FIELD` (ICF Field): Service path (e.g., /sap/bc/rest/zcl_notification_rest*)
- `ICF_VALUE` (ICF Value): Authorization value (typically *)
- `ACTVT` (Activity): 03=Display, 20=Activate/Deactivate

**Usage in Notification Banner**:
```
ICF_FIELD: /sap/bc/rest/zcl_notification_rest*
ICF_VALUE: *
ACTVT: 03, 20
```

**Why Needed**: Controls access to the ICF service node created in SICF transaction.

**Transactions Protected**:
- SICF (HTTP Service Hierarchy Maintenance)

---

### S_RFC (RFC Authorization Check)

**SAP Documentation**: Remote Function Call Authorization
**Object Class**: BC_A
**Purpose**: Controls execution of Remote Function Calls (RFC)

**Fields**:
- `RFC_NAME` (Function Name): Name of RFC-enabled function module
- `RFC_TYPE` (RFC Type): FUNC=Function Module, FUGR=Function Group, SYST=System
- `ACTVT` (Activity): 16=Execute

**Usage in Notification Banner**:
```
RFC_NAME: /IWFND/*, SYST, RFC_READ_TABLE
RFC_TYPE: FUNC
ACTVT: 16
```

**Why Needed**: OData/REST services may internally use RFC calls (e.g., /IWFND/* for Gateway Foundation).

**Note**: Only required if notification system is accessed via OData service (not just REST endpoint).

---

### S_TCODE (Transaction Code Authorization)

**SAP Documentation**: Transaction Code Authorization
**Object Class**: BC_A
**Purpose**: Controls access to SAP transaction codes

**Fields**:
- `TCD` (Transaction Code): Transaction code (e.g., SM30, SE11, SICF)

**Usage in Notification Banner**:
```
TCD: SE11, SE24, SE80, SM30, SICF, SE10, PFCG
```

**Why Needed**: Grants access to all development and maintenance transactions needed for notification management.

**Transactions Used**:
- SE11 - ABAP Dictionary
- SE24 - Class Builder
- SE80 - Object Navigator
- SM30 - Table Maintenance
- SICF - HTTP Service Configuration
- SE10 - Transport Organizer
- PFCG - Role Maintenance

---

## Custom Authorization Objects

These must be **created manually** following the deployment guide instructions.

### Z_NOTIFY (Custom Notification Authorization)

**Creation Required**: YES - Follow Step 7.1 in DEPLOYMENT_GUIDE.md
**Transaction**: SU21 (Maintain Authorization Objects)
**Object Class**: BC_A (Cross-Application Authorization Objects)
**Purpose**: Application-specific authorization for notification management

**Fields**:

#### Field 1: ACTVT (Activity)
```
Field Name: ACTVT
Data Element: ACTIV (SAP standard)
Type: CHAR, Length 2
Description: Activity (Create, Change, Display, Delete)
```

**Permitted Values**:
- `01` - Create notifications (POST /sap/bc/rest/zcl_notification_rest/)
- `02` - Modify notifications (PUT /sap/bc/rest/zcl_notification_rest/)
- `03` - Display notifications (GET /sap/bc/rest/zcl_notification_rest/)
- `06` - Delete notifications (DELETE /sap/bc/rest/zcl_notification_rest/)

#### Field 2: NOTIFY_TYPE (Notification Type)
```
Field Name: NOTIFY_TYPE
Data Element: CHAR10 (or create custom ZNOTIFY_TYPE_AUTH)
Type: CHAR, Length 10
Description: Notification Type Filter
```

**Permitted Values**:
- `*` - All notification types
- `URGENT` - Only urgent notifications
- `INFO` - Only informational notifications
- `TIP` - Only tips
- `SUCCESS` - Only success messages
- `MAINT` - Only maintenance notifications
- `WARNING` - Only warnings

**Implementation in ABAP Classes**:
The ZCL_NOTIFICATION_REST class should check this authorization before allowing REST operations:

```abap
AUTHORITY-CHECK OBJECT 'Z_NOTIFY'
  ID 'ACTVT' FIELD '03'           " Display
  ID 'NOTIFY_TYPE' FIELD '*'.     " All types

IF sy-subrc <> 0.
  " User not authorized
  lv_http_status = 403.  " Forbidden
ENDIF.
```

---

## Authorization Object Fields Reference

### Standard SAP Fields Used

| Field Name | Data Element | Type | Length | Description |
|------------|--------------|------|--------|-------------|
| ACTVT | ACTIV | CHAR | 2 | Activity (01,02,03,06,16,20) |
| DICBERCLS | TDICBERCLS | CHAR | 4 | Authorization Group for Tables |
| TABLE | TABNAME | CHAR | 30 | Table Name |
| DEVCLASS | DEVCLASS | CHAR | 30 | Development Class/Package |
| OBJTYPE | TROBJTYPE | CHAR | 4 | Object Type (TABL,CLAS,DDLS) |
| OBJNAME | SOBJ_NAME | CHAR | 40 | Repository Object Name |
| P_GROUP | PROGRAMMGR | CHAR | 8 | Developer Group |
| SRV_NAME | SRV_NAME | CHAR | 30 | Service Name |
| SRV_TYPE | SRV_TYPE | CHAR | 4 | Service Type |
| ICF_FIELD | ICF_FLDNM | CHAR | 255 | ICF Service Path |
| ICF_VALUE | ICF_VALUE | CHAR | 255 | ICF Authorization Value |
| RFC_NAME | RFCFNAME | CHAR | 30 | RFC Function Module Name |
| RFC_TYPE | RFCTYPE | CHAR | 4 | RFC Type |
| TCD | TCODE | CHAR | 20 | Transaction Code |

### Activity Values (ACTVT)

| Value | Description | Used For |
|-------|-------------|----------|
| 01 | Create | POST operations, table inserts |
| 02 | Change/Update | PUT operations, table updates |
| 03 | Display/Read | GET operations, table reads |
| 06 | Delete | DELETE operations, table deletes |
| 16 | Execute | RFC calls, service execution |
| 20 | Activate/Deactivate | ICF service activation |

---

## Role Configuration Examples

### Z_NOTIFICATION_ADMIN (Full Access Role)

**Purpose**: Full administrative access to notification system
**Transaction**: PFCG

**Authorization Objects**:

```
1. Z_NOTIFY (Custom)
   ACTVT: 01, 02, 03, 06
   NOTIFY_TYPE: *

2. S_TABU_DIS (SAP Standard)
   ACTVT: 01, 02, 03, 06
   DICBERCLS: &NC&

3. S_TABU_NAM (SAP Standard)
   ACTVT: 01, 02, 03, 06
   TABLE: ZTNOTIFY_MSGS

4. S_DEVELOP (SAP Standard)
   ACTVT: 01, 02, 03
   DEVCLASS: *, $TMP, ZNOTIFY
   OBJTYPE: TABL, CLAS, DDLS, FUGR
   OBJNAME: Z*, ZTNOTIFY_*
   P_GROUP: *

5. S_SERVICE (SAP Standard)
   SRV_NAME: ZCL_NOTIFICATION_REST
   SRV_TYPE: HTTP
   ACTVT: 01, 02, 03, 06, 16

6. S_ICF (SAP Standard)
   ICF_FIELD: /sap/bc/rest/zcl_notification_rest*
   ICF_VALUE: *
   ACTVT: 03, 20

7. S_RFC (SAP Standard)
   RFC_NAME: /IWFND/*, SYST
   RFC_TYPE: FUNC
   ACTVT: 16

8. S_TCODE (SAP Standard)
   TCD: SE11, SE24, SE80, SM30, SICF, SE10, PFCG
```

**Permitted Actions**:
- ✅ Create/modify/delete notifications (REST API)
- ✅ Maintain table ZTNOTIFY_MSGS (SM30)
- ✅ Modify ABAP classes and table structure (SE11/SE80)
- ✅ Configure ICF service (SICF)
- ✅ All CRUD operations

---

### Z_NOTIFICATION_DISPLAY (Read-Only Role)

**Purpose**: Read-only access to notifications
**Transaction**: PFCG

**Authorization Objects**:

```
1. Z_NOTIFY (Custom)
   ACTVT: 03
   NOTIFY_TYPE: *

2. S_TABU_DIS (SAP Standard)
   ACTVT: 03
   DICBERCLS: &NC&

3. S_TABU_NAM (SAP Standard)
   ACTVT: 03
   TABLE: ZTNOTIFY_MSGS

4. S_SERVICE (SAP Standard)
   SRV_NAME: ZCL_NOTIFICATION_REST
   SRV_TYPE: HTTP
   ACTVT: 03, 16

5. S_TCODE (SAP Standard)
   TCD: SE16, SE11
```

**Permitted Actions**:
- ✅ View notifications (REST API GET)
- ✅ Display table data (SE16)
- ❌ Create/modify/delete notifications
- ❌ Table maintenance (SM30)

---

### Z_NOTIFICATION_URGENT_ONLY (Restricted Role)

**Purpose**: Manage only urgent notifications
**Transaction**: PFCG

**Authorization Objects**:

```
1. Z_NOTIFY (Custom)
   ACTVT: 01, 02, 03, 06
   NOTIFY_TYPE: URGENT  ← Restricted to URGENT only

2. S_TABU_NAM (SAP Standard)
   ACTVT: 01, 02, 03, 06
   TABLE: ZTNOTIFY_MSGS

3. S_SERVICE (SAP Standard)
   SRV_NAME: ZCL_NOTIFICATION_REST
   SRV_TYPE: HTTP
   ACTVT: 01, 02, 03, 06, 16
```

**Permitted Actions**:
- ✅ Create/modify/delete URGENT notifications only
- ❌ Manage INFO, TIP, SUCCESS, MAINT, WARNING notifications

---

## Testing Authorization Setup

### Test 1: Verify Authorization Object Exists

**Transaction**: SU21

```
1. SU21 → Display Authorization Objects
2. Search for: Z_NOTIFY
3. Expected: Object found with 2 fields (ACTVT, NOTIFY_TYPE)
4. Check standard objects: S_TABU_DIS, S_TABU_NAM, S_DEVELOP, S_SERVICE, S_ICF, S_RFC
```

---

### Test 2: Verify Role Configuration

**Transaction**: PFCG

```
1. PFCG → Display role Z_NOTIFICATION_ADMIN
2. Authorizations tab → Check 8 authorization objects
3. User Assignment tab → Verify users assigned
4. Click "Generate" → Profile successfully generated
```

---

### Test 3: Test User Authorization

**Transaction**: SU53 (Display Authorization Check)

```
1. Login as test user with Z_NOTIFICATION_ADMIN role
2. Execute: SM30 → ZTNOTIFY_MSGS
3. If error occurs: Run SU53 immediately
4. Expected: Shows which authorization object failed

Example SU53 Output (Success):
  Authorization Check for Object: S_TABU_NAM
  User: TESTUSER
  Transaction: SM30
  Table: ZTNOTIFY_MSGS
  Activity: 03
  Result: ✓ Authorization granted
```

---

### Test 4: Test REST API Authorization

**Browser Console**:

```javascript
// Test with authorized user
jQuery.ajax({
    url: "/sap/bc/rest/zcl_notification_rest/",
    type: "GET",
    success: function(data) {
        console.log("✅ Authorized - Data retrieved:", data);
    },
    error: function(xhr) {
        if (xhr.status === 403) {
            console.log("❌ Authorization failed - Check Z_NOTIFY and S_SERVICE");
        }
    }
});
```

---

### Test 5: Authorization Trace

**Transaction**: ST01 (System Trace)

```
1. ST01 → Authorization Check
2. Select user: TESTUSER
3. Start Trace
4. Execute action (e.g., SM30 → ZTNOTIFY_MSGS)
5. Stop Trace
6. Display Trace → Shows all authorization checks performed

Expected Authorization Checks:
  ✓ S_TCODE for SM30
  ✓ S_TABU_DIS for &NC&
  ✓ S_TABU_NAM for ZTNOTIFY_MSGS
  ✓ Z_NOTIFY for notification operations
```

---

## Common Authorization Issues

### Issue 1: "No authorization for table ZTNOTIFY_MSGS"

**Symptom**: SM30 returns authorization error
**Cause**: Missing S_TABU_NAM or S_TABU_DIS
**Solution**:
1. PFCG → Z_NOTIFICATION_ADMIN → Authorizations tab
2. Add S_TABU_NAM with TABLE = ZTNOTIFY_MSGS, ACTVT = 01,02,03,06
3. Add S_TABU_DIS with DICBERCLS = &NC&, ACTVT = 01,02,03,06
4. Generate profile
5. User comparison

---

### Issue 2: "Authorization object Z_NOTIFY does not exist"

**Symptom**: PFCG shows error when adding Z_NOTIFY
**Cause**: Authorization object not created in SU21
**Solution**:
1. Follow DEPLOYMENT_GUIDE.md Step 7.1
2. Create Z_NOTIFY in SU21 with ACTVT and NOTIFY_TYPE fields
3. Save and activate
4. Return to PFCG and add Z_NOTIFY to role

---

### Issue 3: REST API returns 403 Forbidden

**Symptom**: All REST calls fail with 403
**Cause**: Missing S_SERVICE or S_ICF authorization
**Solution**:
1. PFCG → Z_NOTIFICATION_ADMIN
2. Add S_SERVICE: SRV_NAME = ZCL_NOTIFICATION_REST, SRV_TYPE = HTTP
3. Add S_ICF: ICF_FIELD = /sap/bc/rest/zcl_notification_rest*
4. Generate profile
5. User comparison

---

## References

- **SAP Help**: [Table Maintenance Authorization Objects](https://help.sap.com/docs/SAP_Solution_Manager/5dc51b444e704d079bc5293451858929/e50e0edcd9c54144b5b614c4ba27204d.html)
- **SAP Security Guide**: S/4HANA Authorization Concepts
- **Deployment Guide**: [DEPLOYMENT_GUIDE.md](./DEPLOYMENT_GUIDE.md) - Step 7
- **Role Template**: [Z_NOTIFICATION_ADMIN.txt](../abap/roles/Z_NOTIFICATION_ADMIN.txt)

---

**Last Updated**: January 30, 2025
**Version**: 1.1.0
