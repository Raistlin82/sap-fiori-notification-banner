# 📱 Fiori Launchpad Configuration Guide

**Application**: Notification Management (ZNOTIFY_BANNER2)
**Version**: 1.2.0
**Last Updated**: January 2025

---

## 🎯 Architecture Overview

### Single Dynamic Tile Design

```
╔══════════════════════════════════════════════════════╗
║  FIORI LAUNCHPAD                                     ║
╠══════════════════════════════════════════════════════╣
║                                                       ║
║   ╔═══════════════════════════╗                      ║
║   ║  System Notifications     ║  ← Dynamic Tile      ║
║   ║                           ║                      ║
║   ║       🔴 10               ║  Stats updated       ║
║   ║       Active              ║  every 60s           ║
║   ║                           ║                      ║
║   ║    3H | 5M | 2L          ║  Color: Red/Orange   ║
║   ╚═══════════════════════════╝  based on severity  ║
║            ↓ CLICK                                    ║
║                                                       ║
║   ┌────────────────────────────────────────────┐     ║
║   │ Notification Management App                │     ║
║   │ (ZNOTIFY_BANNER2)                          │     ║
║   │                                            │     ║
║   │ [+ Create] [Refresh] [Search...]          │     ║
║   │                                            │     ║
║   │ ┌──────────────────────────────────────┐  │     ║
║   │ │ Table with CRUD operations           │  │     ║
║   │ │ - Create/Edit/Delete notifications   │  │     ║
║   │ │ - Filter by severity/status          │  │     ║
║   │ │ - Toggle active/inactive             │  │     ║
║   │ └──────────────────────────────────────┘  │     ║
║   │                                            │     ║
║   │ Only visible to ADMIN users               │     ║
║   └────────────────────────────────────────────┘     ║
║                                                       ║
╚══════════════════════════════════════════════════════╝

                    ↓ (invisible to users)

╔══════════════════════════════════════════════════════╗
║  GLOBAL NOTIFICATION BANNER COMPONENT                ║
╠══════════════════════════════════════════════════════╣
║                                                       ║
║  • Loads automatically for ALL users in FLP          ║
║  • Polls every 30 seconds                            ║
║  • Shows banner/toast based on active messages       ║
║  • No tile needed (background service)               ║
║                                                       ║
╚══════════════════════════════════════════════════════╝
```

---

## 📋 Prerequisites

Before configuring the FLP tile, ensure:

1. ✅ **Backend deployed**:
   - ABAP table: `ZTNOTIFY_MSGS`
   - ABAP classes: `ZCL_NOTIFICATION_MANAGER`, `ZCL_NOTIFICATION_REST`
   - REST service active: `/sap/bc/rest/zcl_notif_rest/`
   - Stats endpoint active: `/sap/bc/rest/zcl_notif_rest/stats`

2. ✅ **Frontend deployed**:
   - BSP application: `ZNOTIFY_BANNER2`
   - App ID: `com.sap.notifications.banner2`
   - Files in correct BSP structure (Pages/Page Fragments)

3. ✅ **Verify application works**:
   ```
   URL: https://your-system:port/sap/bc/ui5_ui5/sap/znotify_banner2/index.html?sap-client=100

   Expected: Full admin table with Create/Edit/Delete buttons visible
   ```

4. ✅ **Test REST endpoints**:
   ```bash
   # Test stats endpoint (required for dynamic tile)
   curl -u username:password \
     "https://your-system:port/sap/bc/rest/zcl_notif_rest/stats?sap-client=100"

   # Expected response:
   {
     "d": {
       "results": {
         "number": "10",
         "numberUnit": "Active",
         "info": "3H|5M|2L",
         "infoState": "Error"
       }
     }
   }
   ```

---

## 🔧 Step 1: Create Target Mapping

Target Mapping connects the Semantic Object to the actual application.

### Transaction: `/n/UI2/FLPD_CUST`

### Steps:

1. **Navigate to Target Mappings**:
   ```
   /UI2/FLPD_CUST → Target Mappings (left menu)
   ```

2. **Click "+" (Create)**

3. **Fill Target Mapping Details**:

   **Identification**:
   ```
   Semantic Object: NotificationBanner
   Action: display
   ```

   **Application Details**:
   ```
   Application Type: SAPUI5 Component

   SAPUI5 Application:
   - Application ID: com.sap.notifications.banner2
   - Component ID: com.sap.notifications.banner2

   System Alias: (leave empty for local system)
   ```

   **URL Configuration** (Alternative - use ONE of these):

   **Option A - Repository Path** (Recommended):
   ```
   SAP UI5 Repository: ZNOTIFY_BANNER2
   Component Name: com.sap.notifications.banner2
   ```

   **Option B - Direct URL**:
   ```
   Application URL: /sap/bc/ui5_ui5/sap/znotify_banner2/index.html
   ```

4. **Save** the Target Mapping

**Verification**:
```
/UI2/FLPD_CUST → Target Mappings → Search: "NotificationBanner"
Expected: Target Mapping "NotificationBanner-display" visible
```

---

## 🎨 Step 2: Create Catalog

Catalogs contain tiles that can be assigned to groups.

### Transaction: `/n/UI2/FLPD_CUST`

### Steps:

1. **Navigate to Catalogs**:
   ```
   /UI2/FLPD_CUST → Catalogs (left menu)
   ```

2. **Click "+" (Create)**

3. **Fill Catalog Details**:
   ```
   Catalog ID: Z_NOTIF_ADMIN_CATALOG
   Title: Notification Administration
   Description: Catalog for notification management
   ```

4. **Save**

5. **Add Tile to Catalog**:

   a. Select the catalog you just created

   b. Click **"Edit"** (pencil icon)

   c. In the **"Tiles"** section, click **"+"**

   d. Select **"Dynamic"**

   e. **Fill Tile Details**:

   **General**:
   ```
   Title: System Notifications
   Subtitle: Active Messages
   Keywords: notifications, alerts, messages, system, admin
   Icon: sap-icon://message-information
   ```

   **Dynamic Data** (CRITICAL):
   ```
   Service URL: /sap/bc/rest/zcl_notif_rest/stats

   Refresh Interval (seconds): 60

   ⚠️ IMPORTANT: Refresh Interval Rules:
   - 0 = Refresh once (on tile load)
   - 1-9 → Auto-corrected to 10
   - 10+ = Custom interval in seconds
   - Recommended: 60 (1 minute)
   ```

   **Navigation** (Connect to Target Mapping):
   ```
   ☑ Use Semantic Object Navigation  [CHECKED]

   Semantic Object: NotificationBanner
   Action: display

   Parameters: (leave empty)

   Target URL: (auto-generated from target mapping)
   ```

   **Information** (optional):
   ```
   Number Unit: Active
   Information: (leave empty - comes from service)
   Information State: (leave empty - comes from service)
   ```

6. **Save** the tile

7. **Save** the catalog

---

## 👥 Step 3: Create Group

Groups organize tiles visually in the Launchpad.

### Transaction: `/n/UI2/FLPD_CUST`

### Steps:

1. **Navigate to Groups**:
   ```
   /UI2/FLPD_CUST → Groups (left menu)
   ```

2. **Click "+" (Create)**

3. **Fill Group Details**:
   ```
   Group ID: Z_NOTIF_ADMIN_GROUP
   Title: Administration
   Description: Administrative tools and system management
   ```

4. **Save**

5. **Assign Catalog to Group**:

   a. Select the group you just created

   b. Click **"Edit"**

   c. In **"Assigned Catalogs"** section, click **"+"**

   d. Select: `Z_NOTIF_ADMIN_CATALOG`

   e. Click **"OK"**

6. **Add Tile to Group** (make it visible):

   a. In **"Tiles"** section, click **"+"**

   b. Find and select: **"System Notifications"** tile

   c. Click **"OK"**

   d. **Arrange tiles** (drag & drop to position)

7. **Save** the group

---

## 🔐 Step 4: Assign Group to Role

Roles control who can see the group and tile.

### Transaction: `/nPFCG`

### Steps:

1. **Create or Modify Role**:
   ```
   Transaction: /nPFCG

   Role Name: Z_NOTIF_ADMIN
   Description: Notification Administration Role
   ```

2. **Click "Change"**

3. **Assign FLP Group**:

   a. Go to **"Menu"** tab

   b. Look for **"SAP Fiori Launchpad"** node

   c. If not present, create it:
      - Right-click on tree → **"Insert Node"** → **"Transaction"**
      - Transaction: `/UI2/FLP`
      - Save

   d. Right-click on **"SAP Fiori Launchpad"** → **"Insert Node"** → **"Role-Based"**

   e. Enter Group ID: **`Z_NOTIF_ADMIN_GROUP`**

   f. Save

4. **Generate Authorization Profile**:

   a. Go to **"Authorizations"** tab

   b. Click **"Change Authorization Data"**

   c. Click **"Generate"** (green disk icon)

   d. Template selection: Choose appropriate template or generate as-is

   e. **Review authorizations** (especially if new role):
      ```
      Required Authorization Objects:
      - S_RFC: Execute REST service calls
      - S_TABU_CLI: Read/Write ZTNOTIFY_MSGS table
      - S_DEVELOP: Access BSP application (if needed)
      ```

   f. Save

5. **Activate Role**:
   ```
   Back button → Save role
   ```

**Verification**:
```
PFCG → Display role Z_NOTIF_ADMIN → Menu tab
Expected: Z_NOTIF_ADMIN_GROUP visible under SAP Fiori Launchpad
```

---

## 👤 Step 5: Assign Role to Users

### Transaction: `/nSU01`

### Steps:

1. **Open User**:
   ```
   Transaction: /nSU01
   Username: [admin-username]
   ```

2. **Click "Change"**

3. **Go to "Roles" Tab**

4. **Add Role**:
   ```
   Click in empty role field
   Enter: Z_NOTIF_ADMIN
   Press Enter
   ```

5. **Save**

6. **Comparison** (if prompted):
   ```
   Execute user comparison (Utilities → User comparison)
   ```

---

## ✅ Step 6: Verification & Testing

### 6.1 Verify Tile is Visible

1. **Logout** from SAP system (clear session)

2. **Login** again with admin user

3. **Open Fiori Launchpad**:
   ```
   URL: https://your-system:port/sap/bc/ui5_ui5/ui2/ushell/shells/abap/FioriLaunchpad.html?sap-client=100
   ```

4. **Check Group "Administration"** is visible

5. **Check Tile "System Notifications"** is present

6. **Verify Tile Data Updates**:
   ```
   Tile should show:
   - Number: "X" (total active notifications)
   - Unit: "Active"
   - Info: "XH|XM|XL" (breakdown by severity)
   - Color: Red/Orange/Green based on HIGH count
   ```

### 6.2 Test Tile Navigation

1. **Click on "System Notifications" tile**

2. **Expected Behavior**:
   ```
   ✅ Opens: Notification Management app
   ✅ Shows: Table with columns (Severity, Title, Message, etc.)
   ✅ Visible: [+ Create] and [Refresh] buttons in header
   ✅ Filters: Active Only checkbox + Severity dropdown
   ✅ Search: Search field in toolbar
   ```

3. **If tile doesn't navigate**:
   - Check Target Mapping exists (Step 1)
   - Check Semantic Object name matches exactly
   - Check BSP application ZNOTIFY_BANNER2 is active

### 6.3 Test Admin Operations

1. **Click "+ Create" button**

   Expected: Dialog opens with form

2. **Fill form**:
   ```
   Title: Test Notification
   Message: This is a test message
   Severity: HIGH
   Display Mode: BANNER
   Target Audience: ALL
   Start Date: Today
   End Date: Tomorrow
   Active: ON
   ```

3. **Click "Save"**

   Expected:
   ```
   ✅ Success message: "Notification created successfully"
   ✅ Table refreshes
   ✅ New notification appears in table
   ```

4. **Test Edit**:
   - Click **Edit** icon (pencil) on a row
   - Modify title or message
   - Save
   - Verify changes appear

5. **Test Delete**:
   - Click **Delete** icon (trash) on a row
   - Confirm deletion
   - Verify row removed

6. **Test Toggle Active**:
   - Click **switch** in Status column
   - Verify instant update (Active → Inactive or vice versa)

### 6.4 Test Global Banner (All Users)

This component works for ALL users, not just admins.

1. **Login as regular user** (non-admin)

2. **Open Fiori Launchpad**

3. **Expected**:
   ```
   ✅ NO tile visible (regular users don't see admin tile)
   ✅ Banner appears at top if active notifications exist
   ✅ Banner auto-refreshes every 30 seconds
   ```

4. **Create active notification as admin**:
   ```
   Title: System Maintenance
   Message: Scheduled maintenance tonight 10 PM
   Severity: HIGH
   Display Mode: BANNER
   Target Audience: ALL
   Active: ON
   Start Date: Today
   End Date: +7 days
   ```

5. **Verify as regular user**:
   ```
   ✅ Red banner appears at top of Launchpad
   ✅ Shows: "System Maintenance: Scheduled maintenance tonight 10 PM"
   ✅ User can close banner
   ✅ Banner re-appears on next poll if still active
   ```

---

## 🔧 Troubleshooting

### Tile Not Visible

**Problem**: Tile doesn't appear in Launchpad

**Checks**:
1. ✅ Catalog assigned to Group? (`/UI2/FLPD_CUST` → Groups → Check "Assigned Catalogs")
2. ✅ Tile added to Group? (Groups → Tiles section should show tile)
3. ✅ Group assigned to Role? (`PFCG` → Role → Menu tab)
4. ✅ Role assigned to User? (`SU01` → User → Roles tab)
5. ✅ User logged out and back in? (Clear session cache)
6. ✅ Profile regenerated? (`PFCG` → Role → Authorizations → Generate)

### Tile Shows No Data

**Problem**: Tile visible but shows "0 Active" or no stats

**Checks**:
1. ✅ Stats endpoint working?
   ```bash
   curl -u user:pass "https://system:port/sap/bc/rest/zcl_notif_rest/stats?sap-client=100"
   ```
   Expected: JSON with `number`, `numberUnit`, `info`, `infoState`

2. ✅ SICF service active?
   ```
   Transaction: SICF
   Path: /sap/bc/rest/zcl_notif_rest
   Status: Green (active)
   ```

3. ✅ Active notifications exist in table?
   ```
   Transaction: SE16N
   Table: ZTNOTIFY_MSGS
   Filter: ACTIVE = 'X' AND START_DATE <= today AND END_DATE >= today
   Expected: At least one row
   ```

4. ✅ Refresh interval configured?
   ```
   /UI2/FLPD_CUST → Catalogs → Tile → Dynamic Data
   Refresh Interval: 60 (not 0!)
   ```

### Tile Doesn't Navigate to App

**Problem**: Click tile but nothing happens or error

**Checks**:
1. ✅ Target Mapping exists?
   ```
   /UI2/FLPD_CUST → Target Mappings → Search: "NotificationBanner"
   Expected: NotificationBanner-display found
   ```

2. ✅ Semantic Object name matches exactly?
   ```
   Target Mapping: NotificationBanner
   Tile Navigation: NotificationBanner (must match case-sensitive)
   ```

3. ✅ BSP application deployed?
   ```
   Transaction: SE80
   BSP Application: ZNOTIFY_BANNER2
   Expected: Files visible in Pages/Page Fragments
   ```

4. ✅ Application index updated?
   ```
   Transaction: SA38
   Report: /UI5/APP_INDEX_CALCULATE
   Execute (F8) - Leave parameters empty
   ```

5. ✅ Test URL directly?
   ```
   https://system:port/sap/bc/ui5_ui5/sap/znotify_banner2/index.html?sap-client=100
   Expected: Admin table opens
   ```

### App Opens But Shows Errors

**Problem**: App loads but REST calls fail

**Checks**:
1. ✅ REST service active and accessible?
   ```
   Transaction: SICF
   Path: /sap/bc/rest/zcl_notif_rest
   Handler Class: ZCL_NOTIFICATION_REST
   Status: Active (green)
   ```

2. ✅ CORS headers configured?
   ```
   ZCL_NOTIFICATION_REST should set:
   - Access-Control-Allow-Origin: *
   - Access-Control-Allow-Methods: GET,POST,PUT,DELETE
   ```

3. ✅ User has authorizations?
   ```
   Required:
   - S_RFC: Execute REST calls
   - S_TABU_CLI: Read/Write ZTNOTIFY_MSGS
   ```

4. ✅ Check browser console (F12)?
   ```
   Look for:
   - 401 Unauthorized → Missing auth
   - 403 Forbidden → Missing authorization object
   - 404 Not Found → Service not active
   - 500 Server Error → Check ST22 ABAP dumps
   ```

---

## 🎯 Summary Checklist

Before going live, verify:

- [ ] Backend REST service active and tested
- [ ] Frontend BSP application deployed
- [ ] Target Mapping created (NotificationBanner-display)
- [ ] Catalog created with dynamic tile
- [ ] Tile configured with correct service URL
- [ ] Group created and tile assigned
- [ ] Role created/modified with group
- [ ] Profile generated for role
- [ ] Users assigned to role
- [ ] Tile visible in Launchpad
- [ ] Tile shows live data (updates every 60s)
- [ ] Tile navigation works (opens admin app)
- [ ] Admin CRUD operations work (Create/Edit/Delete)
- [ ] Global banner works for all users
- [ ] Authorization tested (admin vs regular users)

---

**Configuration Complete!** ✅

Your Notification Management system is now ready for production use.

---

**Document Version**: 1.0
**Last Updated**: January 2025
**Application**: ZNOTIFY_BANNER2 v1.2.0
