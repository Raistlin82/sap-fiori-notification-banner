# Analytics Dashboard - FLP Tile Configuration

## Overview

This document describes how to configure the Fiori Launchpad tile for the **Notification Acknowledgment Analytics** dashboard.

---

## Tile Configuration

### Tile Properties

```json
{
  "id": "notificationAnalytics",
  "title": "Notification Analytics",
  "subtitle": "Track Acknowledgment Compliance",
  "icon": "sap-icon://chart-table-view",
  "size": "1x1",
  "tileType": "sap.ushell.ui.tile.StaticTile"
}
```

### Target Configuration

```json
{
  "semanticObject": "NotificationAnalytics",
  "action": "display",
  "title": "Notification Acknowledgment Analytics",
  "url": "/sap/bc/ui5_ui5/sap/analytics/index.html"
}
```

---

## SAP Fiori Launchpad Designer Configuration

### Step 1: Create Catalog

1. Open **Fiori Launchpad Designer** (transaction `/UI2/FLPD_CUST`)
2. Navigate to **Catalog** tab
3. Click **Create** → **New Catalog**
4. Enter:
   - **ID**: `Z_NOTIF_ANALYTICS_CAT`
   - **Title**: `Notification Analytics Catalog`
   - **Description**: `Analytics dashboard for notification compliance`

### Step 2: Create Target Mapping

1. In the catalog, click **Create** → **Target Mapping**
2. Enter:
   - **Semantic Object**: `NotificationAnalytics`
   - **Action**: `display`
   - **Title**: `Notification Acknowledgment Analytics`
   - **Subtitle**: `Track Compliance`
   - **Icon**: `sap-icon://chart-table-view`

3. **Application** section:
   - **Application Type**: `URL`
   - **URL**: `/sap/bc/ui5_ui5/sap/analytics/index.html`
   - **System Alias**: (leave empty for local system)

4. Click **Save**

### Step 3: Create Tile

1. In the catalog, click **Create** → **Static Tile**
2. Enter:
   - **ID**: `Z_NOTIF_ANALYTICS_TILE`
   - **Title**: `Notification Analytics`
   - **Subtitle**: `Track Acknowledgment Compliance`
   - **Icon**: `sap-icon://chart-table-view`
   - **Target Mapping**: Select the mapping created in Step 2

3. Click **Save**

### Step 4: Create Group

1. Navigate to **Groups** tab
2. Click **Create** → **New Group**
3. Enter:
   - **ID**: `Z_NOTIF_ADMIN_GROUP`
   - **Title**: `Notification Administration`
   - **Description**: `Notification management and analytics tools`

4. Assign tiles:
   - Add existing admin tile (notification management)
   - Add new analytics tile

5. Click **Save**

### Step 5: Assign to Role

1. Navigate to **Roles** tab
2. Find role `Z_NOTIFICATION_ADMIN`
3. Click **Edit**
4. In **Catalogs** section:
   - Add `Z_NOTIF_ANALYTICS_CAT`
5. In **Groups** section:
   - Add `Z_NOTIF_ADMIN_GROUP`
6. Click **Save**

---

## Alternative: Direct ICF Service Configuration

If deploying app directly to ABAP stack:

### Step 1: Create ICF Service

Transaction: `SICF`

1. Navigate to: `default_host` → `sap` → `bc` → `ui5_ui5` → `sap`
2. Right-click → **New Sub-Element**
3. Enter:
   - **Service Name**: `analytics`
   - **Description**: `Notification Analytics Dashboard`
   - **Handler List**: `CL_HTTP_EXT_APPLICATION`

4. **Service Data** tab:
   - **GUI Link**: `/sap/bc/ui5_ui5/sap/analytics/index.html`

5. **Error Pages** tab:
   - Use default settings

6. Save and activate

### Step 2: Deploy UI5 Application

1. Build application:
   ```bash
   cd app/analytics
   npm run build
   ```

2. Upload to ABAP server:
   - Use `/UI5/UI5_REPOSITORY_LOAD` transaction
   - Or use deployment via BSP application

---

## Authorization Requirements

### Same as Admin Application

The analytics dashboard uses the **same authorization** as the notification admin application:

- **Role**: `Z_NOTIFICATION_ADMIN`
- **Authorization Object**: (same as admin app, if exists)

**No additional authorizations required**.

Users who can access the notification admin app can also access the analytics dashboard.

---

## Testing

### Standalone Testing (Development)

```bash
# From project root
npm run start:analytics

# Or from analytics folder
cd app/analytics
npm start
```

Opens at: `http://localhost:8081/index.html`

### FLP Testing (Integration)

1. Login to Fiori Launchpad
2. Navigate to **Notification Administration** group
3. Click **Notification Analytics** tile
4. Should open analytics dashboard

### Verify Tile Appears

1. User must have role `Z_NOTIFICATION_ADMIN` assigned
2. Tile should appear in assigned group
3. Click tile → Opens analytics dashboard
4. Select notification → Shows analytics data

---

## Troubleshooting

### Tile Not Visible

**Check**:
1. Role `Z_NOTIFICATION_ADMIN` assigned to user?
2. Catalog assigned to role?
3. Group assigned to role?
4. Tile assigned to group?
5. Cache cleared? (logout/login)

### Tile Clicks But No Data

**Check**:
1. Backend REST endpoint active? (`/sap/bc/rest/zcl_notif_analytics/`)
2. ABAP class `ZCL_NOTIF_ANALYTICS` activated?
3. CDS view `ztnotify_messages_analytics` activated?
4. User has data authorization?

### 404 Error When Opening Tile

**Check**:
1. ICF service active? (transaction `SICF`)
2. UI5 application deployed to server?
3. Correct URL in target mapping?
4. BSP application exists?

---

## URL Structure

### Standalone
```
http://<server>:8081/index.html
```

### Via FLP (after deployment)
```
https://<server>/sap/bc/ui5_ui5/sap/analytics/index.html
```

### Via FLP (semantic object)
```
#NotificationAnalytics-display
```

---

## Related Documentation

- [BACKEND_DEPLOYMENT.md](BACKEND_DEPLOYMENT.md) - Deploy ABAP classes
- [FLP_CONFIGURATION.md](FLP_CONFIGURATION.md) - General FLP setup
- [ADMIN_GUIDE.md](ADMIN_GUIDE.md) - Admin application guide

---

## Screenshots

### Expected FLP Tile

```
┌────────────────────────┐
│  📊                    │
│  Notification          │
│  Analytics             │
│                        │
│  Track Acknowledgment  │
│  Compliance            │
└────────────────────────┘
```

### Expected Dashboard Layout

```
┌──────────────────────────────────────────────────┐
│  Notification Acknowledgment Analytics           │
├──────────────────────────────────────────────────┤
│  Select Notification: [Dropdown]        [Refresh]│
│                                                   │
│  ┌─────────┐  ┌──────────────────────┐          │
│  │ 🥧Chart │  │  📊 Statistics       │          │
│  └─────────┘  └──────────────────────┘          │
│                                                   │
│  Filter: [All] [Ack] [Pending]  [Search...]     │
│  ┌──────────────────────────────────────────┐   │
│  │ User Table                               │   │
│  └──────────────────────────────────────────┘   │
└──────────────────────────────────────────────────┘
```

---

**Last Updated**: 2025-10-10
