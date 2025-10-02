# 🧪 Testing Guide - SAP Fiori Global Notification Banner

## Overview

This guide provides comprehensive testing procedures for the SAP Fiori Global Notification Banner application. All tests should be performed after successful deployment to verify functionality.

**Prerequisites**:
- Application deployed to SAP system (ZNOTIFY_BANNER2)
- Backend service running (ZCL_NOTIF_REST)
- Database table exists (ZTNOTIFY_MSGS)
- FLP configuration complete

---

## 📑 Table of Contents

1. [Backend Testing](#backend-testing)
   - [Test 1: Database Table](#test-1-database-table)
   - [Test 2: CDS View](#test-2-cds-view)
   - [Test 3: REST API](#test-3-rest-api)

2. [Frontend Testing](#frontend-testing)
   - [Test 1: Display Modes](#test-1-display-modes)
   - [Test 2: Tile Counter](#test-2-tile-counter)
   - [Test 3: Multi-Notification Navigation](#test-3-multi-notification-navigation)
   - [Test 4: Target Audience Filtering](#test-4-target-audience-filtering-sap-standard-roles-only)

3. [Performance Testing](#performance-testing)
   - [Test 1: Polling Performance](#test-1-polling-performance)
   - [Test 2: Large Data Set](#test-2-large-data-set)
   - [Test 3: Error Recovery](#test-3-error-recovery)

---

## Backend Testing

### Test 1: Database Table

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

**Verification Steps**:
1. Open SE16 transaction
2. Enter table name: `ZTNOTIFY_MSGS`
3. Verify all fields are visible
4. Check F4 help values match expected dropdowns
5. Verify data types: MESSAGE_ID (UUID), ACTIVE (CHAR1), dates (DATS)

---

### Test 2: CDS View

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

**Verification Steps**:
1. Open SE11 transaction
2. Enter CDS view name: `ZTNOTIFY_MESSAGES`
3. Click Display
4. Verify view definition includes filtering logic
5. Use SE16 to view data in SQL view `ZNOTIFY_MSG`
6. Confirm only active notifications within date range are shown

---

### Test 3: REST API

#### GET /sap/bc/rest/zcl_notif_rest/

**Test in Browser Console**:
```javascript
jQuery.ajax({
    url: "/sap/bc/rest/zcl_notif_rest/",
    type: "GET",
    success: function(data) {
        console.log("Notifications:", data);
        // Expected: {"notifications": [...]}
    }
});
```

**Expected Response**:
```json
{
    "notifications": [
        {
            "message_id": "550e8400-e29b-41d4-a716-446655440001",
            "message_type": "URGENT",
            "severity": "HIGH",
            "title": "System Maintenance",
            "message_text": "Planned downtime on Sunday",
            "start_date": "20250401",
            "end_date": "20250430",
            "target_users": "ALL",
            "active": "X",
            "display_mode": "BANNER"
        }
    ]
}
```

---

#### GET /sap/bc/rest/zcl_notif_rest/stats

**Test in Browser Console**:
```javascript
jQuery.ajax({
    url: "/sap/bc/rest/zcl_notif_rest/stats",
    type: "GET",
    success: function(data) {
        console.log("Statistics:", data);
        // Expected: {"total": 10, "high_count": 3, "medium_count": 5, "low_count": 2}
    }
});
```

**Expected Response**:
```json
{
    "total": 10,
    "high_count": 3,
    "medium_count": 5,
    "low_count": 2
}
```

---

#### POST /sap/bc/rest/zcl_notif_rest/

**Test in Browser Console**:
```javascript
jQuery.ajax({
    url: "/sap/bc/rest/zcl_notif_rest/",
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

**Verification**:
1. Open browser console (F12)
2. Execute POST request
3. Check response: `{"success": true, "message_id": "..."}`
4. Verify in SM30: New entry exists in ZTNOTIFY_MSGS
5. Verify in FLP: Toast notification appears

---

## Frontend Testing

### Test 1: Display Modes

Create test notifications with different display modes to verify UI behavior.

#### Test Case 1: BANNER Mode

**Setup**:
```
Transaction: SM30 → ZTNOTIFY_MSGS
Create entry:
- MESSAGE_TYPE: URGENT
- SEVERITY: HIGH
- TITLE: Test Banner
- MESSAGE_TEXT: This is a banner notification
- DISPLAY_MODE: BANNER
- ACTIVE: X
- START_DATE: (today)
- END_DATE: (future date)
- TARGET_USERS: ALL
```

**Expected Result**:
- ✅ Fixed banner appears at top of all Fiori apps
- ✅ Red color (HIGH severity)
- ✅ User must click close button to dismiss
- ✅ Banner stays until manually closed
- ✅ Icon: ⚠️ (warning icon for URGENT type)

---

#### Test Case 2: TOAST Mode

**Setup**:
```
Create entry:
- MESSAGE_TYPE: INFO
- SEVERITY: LOW
- TITLE: Test Toast
- MESSAGE_TEXT: This is a toast notification
- DISPLAY_MODE: TOAST
- ACTIVE: X
- START_DATE: (today)
- END_DATE: (future date)
- TARGET_USERS: ALL
```

**Expected Result**:
- ✅ Toast appears at bottom-right
- ✅ Green color (LOW severity)
- ✅ Auto-dismisses after 5 seconds
- ✅ Slide-in animation
- ✅ Icon: ℹ️ (info icon for INFO type)

---

#### Test Case 3: BOTH Mode

**Setup**:
```
Create entry:
- MESSAGE_TYPE: MAINT
- SEVERITY: MEDIUM
- TITLE: Test Both
- MESSAGE_TEXT: This appears as both banner and toast
- DISPLAY_MODE: BOTH
- ACTIVE: X
- START_DATE: (today)
- END_DATE: (future date)
- TARGET_USERS: ALL
```

**Expected Result**:
- ✅ Both banner AND toast appear simultaneously
- ✅ Orange color (MEDIUM severity)
- ✅ Banner stays until closed, toast auto-dismisses
- ✅ Icon: 🔧 (wrench icon for MAINT type)

---

#### Test Case 4: SILENT Mode

**Setup**:
```
Create entry:
- MESSAGE_TYPE: TIP
- SEVERITY: LOW
- TITLE: Test Silent
- MESSAGE_TEXT: This is logged but not displayed
- DISPLAY_MODE: SILENT
- ACTIVE: X
- START_DATE: (today)
- END_DATE: (future date)
- TARGET_USERS: ALL
```

**Expected Result**:
- ✅ No UI display (banner or toast)
- ✅ Check browser console: "Silent notification logged: Test Silent"
- ✅ Verify with: `GET /sap/bc/rest/zcl_notif_rest/log`

---

### Test 2: Tile Counter

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

### Test 3: Multi-Notification Navigation

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

**Verification**:
- ✅ Navigation arrows visible when multiple notifications exist
- ✅ Counter shows current position (e.g., "2 / 3")
- ✅ Arrows wrap around (clicking next on last goes to first)
- ✅ Severity color changes per notification

---

### Test 4: Target Audience Filtering (SAP Standard Roles Only)

#### Test Case 1: Public Notification

**Setup**:
```
Transaction: SM30 → ZTNOTIFY_MSGS → Create entry
TARGET_USERS: ALL (select from F4 dropdown - 3 values available)
```
**Expected**: All users see this notification (no role check)

---

#### Test Case 2: Administrators Only

**Setup**:
```
Transaction: SM30 → ZTNOTIFY_MSGS → Create entry
TARGET_USERS: ADMIN (select from F4 dropdown)
```
**Expected**: Only users with **SAP_ALL** role see this (exact match)

**Verification**:
- Check AGR_USERS table: `SELECT * FROM agr_users WHERE uname = 'USERNAME' AND agr_name = 'SAP_ALL'`
- SU01 → User → Roles tab → Should have SAP_ALL role

---

#### Test Case 3: Developers Only

**Setup**:
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

## Performance Testing

### Test 1: Polling Performance

**Monitor**:
- Open Chrome DevTools → Network tab
- Refresh Fiori app
- Observe polling requests every 30 seconds

**Expected**:
- Request URL: `/sap/bc/rest/zcl_notif_rest/`
- Interval: 30 seconds
- Response Time: < 500ms
- Response Size: < 5KB

**Verification Steps**:
1. Open Chrome DevTools (F12)
2. Go to Network tab
3. Filter by XHR requests
4. Wait for polling requests
5. Check request details:
   - Method: GET
   - Status: 200 OK
   - Size: < 5KB
   - Time: < 500ms

---

### Test 2: Large Data Set

**Setup**:
Create 50 active notifications using SM30 or REST API

**Expected**:
- ✅ All 50 notifications load successfully
- ✅ Navigation arrows work smoothly
- ✅ No UI freeze or lag
- ✅ Memory usage stable

**Test Steps**:
1. Create 50 active notifications (use REST API POST for bulk creation)
2. Open Fiori app
3. Verify banner displays first notification
4. Navigate through all 50 using arrows
5. Monitor browser performance:
   - Open Chrome DevTools → Performance tab
   - Record session while navigating
   - Check for frame drops or memory leaks

---

### Test 3: Error Recovery

**Simulate Backend Downtime**:

**Test Steps**:
1. SICF → Deactivate `ZCL_NOTIF_REST` service
2. Open Fiori app
3. Expected: Silent failure, no error popup
4. Check console: "Circuit breaker opened after 5 failures"
5. SICF → Reactivate service
6. Wait 60 seconds
7. Expected: Circuit breaker closes, polling resumes

**Circuit Breaker Verification**:
- ✅ No intrusive error messages to user
- ✅ Console logs errors gracefully
- ✅ Polling stops after 5 consecutive failures
- ✅ Polling resumes automatically when service recovers

---

## ✅ Testing Checklist

Use this checklist to track testing progress:

### Backend Testing
- [ ] Database table structure verified (SE16)
- [ ] F4 value helps working (SM30)
- [ ] CDS view filtering correctly (SE11)
- [ ] REST API GET endpoint returns data
- [ ] REST API stats endpoint returns statistics
- [ ] REST API POST endpoint creates notifications

### Frontend Testing
- [ ] BANNER mode displays correctly
- [ ] TOAST mode auto-dismisses after 5 seconds
- [ ] BOTH mode shows banner + toast
- [ ] SILENT mode logs without UI display
- [ ] Tile counter shows correct count
- [ ] Tile counter updates every 60 seconds
- [ ] Tile color coding matches severity rules
- [ ] Multi-notification navigation works
- [ ] Target audience filtering (ALL users)
- [ ] Target audience filtering (ADMIN only)
- [ ] Target audience filtering (DEVELOPER only)

### Performance Testing
- [ ] Polling every 30 seconds confirmed
- [ ] Response time < 500ms
- [ ] Response size < 5KB
- [ ] Large data set (50 notifications) performs well
- [ ] Circuit breaker activates on backend failure
- [ ] Circuit breaker recovers when service returns

---

## 🐛 Known Issues

### Issue 1: Polling Stops After Long Idle Time
**Symptom**: Notifications stop updating after browser tab inactive for >30 minutes
**Workaround**: Refresh page to resume polling
**Fix**: Implement visibility API to pause/resume polling

### Issue 2: Tile Counter Delay on First Load
**Symptom**: Tile shows "0 Active" for 2-3 seconds on FLP startup
**Root Cause**: Tile counter API initializes before backend service ready
**Workaround**: Wait for initial load (max 5 seconds)

---

## 📞 Support

For issues or questions:
- Check [TROUBLESHOOTING.md](./TROUBLESHOOTING.md)
- Review [DEPLOYMENT_GUIDE.md](./DEPLOYMENT_GUIDE.md)
- Check SAP Community: https://community.sap.com
- SAP Help Portal: https://help.sap.com

---

**Generated**: January 2025
**Version**: 1.2.0
**Last Updated**: January 2025
