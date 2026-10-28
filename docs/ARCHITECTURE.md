# 🏗️ System Architecture - SAP Fiori Global Notification Banner

**Version**: 1.3.0
**Last Updated**: October 2025
**Architects**: Gabriele Rendina & Ileana Scaglia

---

## 📋 Table of Contents

1. [High-Level Architectural Diagram](#-high-level-architectural-diagram)
2. [Data Flow - Notification Display](#-data-flow---notification-display)
3. [Security and Authorization](#-security-and-authorization)
4. [Data Model](#-data-model)
5. [Component Structure](#-component-structure)
6. [Deployment Architecture](#-deployment-architecture)
7. [Performance Optimization (v1.2.0)](#-performance-optimization-v120)
8. [API Endpoints](#-api-endpoints)
9. [What's New in v1.2.0](#-whats-new-in-v120)
10. [Stakeholders](#-stakeholders)
11. [Technical References](#-technical-references)

---

## 📊 High-Level Architectural Diagram

```mermaid
graph TB
    subgraph FLP["🚀 SAP Fiori Launchpad"]
        Plugin["FLP Plugin<br/>sap.flp config<br/>Auto-loads for all users"]
    end

    subgraph Frontend["💻 Frontend Layer (UI5)"]
        Component["Component.js<br/>• Global initialization<br/>• Polling every 30s<br/>• Memory management"]
        Banner["NotificationBanner.js<br/>• Display modes (BANNER/TOAST/BOTH/SILENT)<br/>• Dismissed notifications (localStorage)<br/>• Navigation & user interactions"]
        Csrf["CsrfAjax.js<br/>• X-CSRF-Token handshake<br/>• Retry on expired token"]
        View["Admin UI (View1.controller.js)<br/>• CRUD operations<br/>• Filters (Active/Severity)<br/>• Display mode selector"]
    end

    subgraph Backend["🗄️ Backend Layer (ABAP)"]
        REST["ZCL_NOTIFICATION_REST<br/>• REST endpoints<br/>• CSRF validation<br/>• JSON serialization"]
        Manager["ZCL_NOTIFICATION_MANAGER<br/>• Business logic<br/>• Authorization checks<br/>• Data validation"]
        CDS["ZT_NOTIFY_MESSAGES<br/>• CDS View with filters<br/>• Date range logic"]
        DB["ZTNOTIFY_MSGS<br/>• Custom domains<br/>• Audit trail fields"]
    end

    FLP --> Plugin
    Plugin --> Component
    Component --> Banner
    Component --> View

    Banner -->|REST API| REST
    Banner --> Csrf
    View --> Csrf
    Csrf -->|POST/PUT/DELETE with token| REST
    View -->|GET ?all=X| REST
    FLP -->|Dynamic tile: /stats| REST

    REST --> Manager
    Manager --> CDS
    CDS --> DB

    style FLP fill:#e3f2fd
    style Frontend fill:#fff3e0
    style Backend fill:#e8f5e9
```

---

## 🔄 Data Flow - Notification Display

```mermaid
sequenceDiagram
    actor User
    participant FLP as Fiori Launchpad
    participant Comp as Component.js
    participant Banner as NotificationBanner.js
    participant REST as REST Service
    participant Backend as ABAP Backend
    participant DB as Database

    User->>FLP: Opens Fiori App
    activate FLP
    FLP->>Comp: Initialize via FLP Plugin
    activate Comp
    Comp->>Banner: Create NotificationBanner
    activate Banner
    Comp->>Banner: Start polling (30s)

    loop Every 30 seconds
        Banner->>REST: GET /sap/bc/rest/zcl_notif_rest/
        activate REST
        REST->>Backend: Validate authorization
        activate Backend
        Backend->>DB: Query active notifications
        activate DB
        DB->>DB: Filter: active='X'<br/>Filter: date range<br/>Sort by severity
        DB-->>Backend: Return rows
        deactivate DB
        Backend-->>REST: JSON Response
        deactivate Backend
        REST-->>Banner: [{message_id, title, severity, display_mode...}]
        deactivate REST

        Banner->>Banner: Check localStorage (dismissed)
        Banner->>Banner: Group by display_mode

        alt BANNER mode
            Banner->>FLP: Show MessageStrip (top)
        end

        alt TOAST mode
            Banner->>FLP: Show MessageToast (bottom-right, 5s)
        end

        alt BOTH mode
            Banner->>FLP: Show both Banner + Toast
        end

        alt SILENT mode
            Banner->>Banner: Log only (no UI)
        end
    end

    User->>Banner: Click ❌ (Dismiss)
    Banner->>Banner: Save to localStorage (1h timeout)
    Banner->>FLP: Hide notification

    deactivate Banner
    deactivate Comp
    deactivate FLP
```

---

## 🔐 Security and Authorization

```mermaid
flowchart TD
    Start([User Request]) --> Auth[SAP Authentication]
    Auth -->|SAP Logon Ticket<br/>Basic Auth (dev)<br/>SSO Token (prod)| SICF[SICF Service Check]

    SICF -->|Service: /sap/bc/rest/zcl_notif_rest/<br/>Handler: ZCL_NOTIFICATION_REST| AuthCheck{Authorization Check}

    AuthCheck -->|Z_BR_ADMINISTRATOR role| BypassAuth[✅ Admin - Full Access]
    AuthCheck -->|No Z_BR_ADMINISTRATOR| CheckNotify[Check Z_NOTIFY object]

    CheckNotify -->|Has Z_NOTIFY| Activities[Check Activity]
    CheckNotify -->|No Z_NOTIFY| Deny[❌ 401 Unauthorized]

    Activities -->|GET| Display[03 - Display]
    Activities -->|POST| Create[01 - Create]
    Activities -->|PUT| Change[02 - Change]
    Activities -->|DELETE| Delete[06 - Delete]

    BypassAuth --> CSRF[CSRF Token Validation]
    Display --> CSRF
    Create --> CSRF
    Change --> CSRF
    Delete --> CSRF

    CSRF -->|Valid token| BizLogic[Execute Business Logic]
    CSRF -->|Invalid/missing| Deny

    BizLogic -->|Success| Success[200/201 OK + JSON]
    BizLogic -->|Error| ServerError[500 Internal Server Error]

    style Start fill:#e3f2fd
    style Auth fill:#fff3e0
    style BypassAuth fill:#c8e6c9
    style Success fill:#c8e6c9
    style Deny fill:#ffccbc
    style ServerError fill:#ffccbc
```

**Authorization Strategy:**
1. **Z_BR_ADMINISTRATOR role** → Full access (bypass Z_NOTIFY check)
2. **Z_NOTIFY authorization object** → Activity-based permissions
3. **Target Audience filtering** → Role-based notification visibility (ALL, ADMIN, DEVELOPER)

---

## 💾 Data Model

```mermaid
erDiagram
    ZTNOTIFY_MSGS ||--o{ ZT_NOTIFY_MESSAGES : "filtered by"
    ZTNOTIFY_MSGS ||--o{ ZNOTIFY_ACK_LOG : "tracked by"
    ZNOTIF_MATRIX ||--o{ ZTNOTIFY_MSGS : "validates"

    ZTNOTIFY_MSGS {
        char32 MESSAGE_ID PK "UUID"
        char12 MESSAGE_TYPE "Domain: ZDOMAIN_MSG_TYPE (6 values)"
        char8 SEVERITY "Domain: ZDOMAIN_SEVERITY (3 values)"
        char255 TITLE "Required"
        char255 MESSAGE_TEXT "Required"
        dats START_DATE "Required"
        dats END_DATE "Required"
        char10 TARGET_USERS "Domain: ZDOMAIN_TARGET_USERS (3 values)"
        char1 ACTIVE "X or blank"
        char10 DISPLAY_MODE "Domain: ZDOMAIN_DISPLAY_MODE (4 values)"
        char1 REQUIRES_ACK "X or blank (v1.3.0)"
        syuname CREATED_BY "Audit trail"
        timestampl CREATED_AT "Audit trail"
        syuname CHANGED_BY "Audit trail"
        timestampl CHANGED_AT "Audit trail"
    }

    ZNOTIFY_ACK_LOG {
        char3 MANDT PK "Client"
        char32 MESSAGE_ID PK "FK to ZTNOTIFY_MSGS"
        char12 USERID PK "User who acknowledged"
        timestampl ACK_TIMESTAMP "When acknowledged"
        char255 CLIENT_INFO "Browser/device info"
    }

    ZT_NOTIFY_MESSAGES {
        char32 MESSAGE_ID "From ZTNOTIFY_MSGS"
        char12 MESSAGE_TYPE
        char8 SEVERITY
        char255 TITLE
        char255 MESSAGE_TEXT
        dats START_DATE
        dats END_DATE
        char10 TARGET_USERS
        char1 ACTIVE
        char10 DISPLAY_MODE
        char1 REQUIRES_ACK "(v1.3.0)"
    }

    ZNOTIF_MATRIX {
        char8 SEVERITY PK
        char12 MESSAGE_TYPE PK
        char10 DISPLAY_MODE PK
        char1 VALID "X or blank"
        char1 REQUIRES_ACK "Auto-set for HIGH+BANNER, URGENT+BANNER"
    }
```

### Custom Domains (v1.1.0+)

| Domain | Data Element | Values | F4 Help |
|--------|-------------|--------|---------|
| ZDOMAIN_MSG_TYPE | ZNOTIFY_MSG_TYPE | URGENT, INFO, TIP, SUCCESS, MAINT, WARNING | ✅ Yes |
| ZDOMAIN_SEVERITY | ZNOTIFY_SEVERITY | HIGH, MEDIUM, LOW | ✅ Yes |
| ZDOMAIN_DISPLAY_MODE | ZNOTIFY_DISP_MODE | BANNER, TOAST, BOTH, SILENT | ✅ Yes |
| ZDOMAIN_TARGET_USERS | ZNOTIFY_TARGET_USERS | ALL, ADMIN, DEVELOPER | ✅ Yes |

**CDS View Query:**
```sql
SELECT * FROM ZTNOTIFY_MSGS
WHERE ACTIVE = 'X'
  AND START_DATE <= $session.system_date
  AND END_DATE >= $session.system_date
```

---

## 📦 Component Structure

### Frontend (UI5)

```
webapp/
├── Component.js                    [Entry Point - Memory Safe]
│   ├── init()                     ← UIComponent lifecycle
│   ├── _initializeNotificationBanner()
│   ├── _startNotificationPolling() ← Stores interval ID
│   └── exit()                     ← Cleanup with clearInterval()
│
├── controller/
│   ├── NotificationBanner.js      [Core Logic - v1.2.0]
│   │   ├── loadNotifications()    ← API call with retry/circuit breaker
│   │   ├── _processNotifications() ← Filter dismissed (localStorage)
│   │   ├── _displayNotifications() ← Group by display_mode
│   │   ├── _showBanner()          ← MessageStrip (top)
│   │   ├── _showToast()           ← MessageToast (bottom-right, 5s)
│   │   ├── _dismissNotification()  ← Save to localStorage (1h timeout)
│   │   └── _getDismissedNotifications() ← Read from localStorage
│   │
│   └── View1.controller.js        [Admin UI - v1.2.0]
│       ├── onCreateNotification()
│       ├── onUpdateNotification()
│       ├── onDeleteNotification()
│       ├── onFilterChange()       ← 3-state filter (All/Active/Inactive)
│       └── onDisplayModeSelect()  ← BANNER/TOAST/BOTH/SILENT
│
├── view/
│   └── View1.view.xml             [Admin UI View]
│       ├── Table (CRUD operations)
│       ├── Select (Active filter - 3 states)
│       ├── Select (Severity filter)
│       └── Select (Display mode selector)
│
├── model/
│   └── models.js                  [Data Models]
│
├── css/
│   └── style.css                  [Styling]
│
└── i18n/
    └── i18n.properties            [Translations]
```

### Backend (ABAP)

```
abap/
├── zcl_notification_manager.clas.abap [Business Logic]
│   ├── get_active_notifications()
│   ├── create_notification()
│   ├── update_notification()
│   ├── deactivate_notification()
│   ├── check_user_authorization()  ← Z_BR_ADMINISTRATOR or Z_NOTIFY
│   └── check_target_audience()     ← ALL, ADMIN, DEVELOPER filtering
│
├── zcl_notification_rest.clas.abap [REST Handler]
│   ├── if_http_extension~handle_request()
│   ├── handle_get_notifications()
│   ├── handle_create_notification()
│   ├── handle_update_notification()
│   ├── handle_delete_notification()
│   ├── handle_get_stats()          ← /stats endpoint (tile counter)
│   └── handle_get_log()            ← /log endpoint (SILENT notifications)
│
├── ztnotify_msgs.se11             [Database Table]
├── ztnotify_messages.ddls         [CDS View]
│
└── domains/
    ├── zdomain_msg_type.se11      [6 fixed values]
    ├── zdomain_severity.se11      [3 fixed values]
    ├── zdomain_display_mode.se11  [4 fixed values]
    └── zdomain_target_users.se11  [3 fixed values]
```

---

## 🚀 Deployment Architecture

### Modern SAP Deployment (v1.2.0)

```mermaid
flowchart LR
    subgraph Dev["Local Development"]
        Code[Source Code]
        NPM[npm install<br/>npm run build]
        FioriTools[SAP Fiori Tools]
    end

    subgraph SAP["SAP S/4HANA System"]
        BACKEND[ABAP Backend<br/>Tables, Classes, REST]
        BSP[BSP Application<br/>ZNOTIFY_BANNER2]
        FLP[Fiori Launchpad<br/>Plugin Configuration]
    end

    Code --> NPM
    NPM --> FioriTools
    FioriTools -->|Automated Deployment| BSP
    BSP --> FLP

    FLP -.->|Loads globally| BACKEND

    style Dev fill:#e3f2fd
    style SAP fill:#e8f5e9
```

**Deployment Methods:**
- **Frontend**: Automated deployment with SAP Fiori Tools (Option A)
- **Backend**: Transport-based ABAP deployment
- **FLP Configuration**:
  - Spaces and Pages (S/4HANA 2020+)
  - FLP Plugin (Global loading for all users)

---

## ⚡ Performance Optimization (v1.2.0)

### Memory Management

**Critical Fix (v1.2.0):**
- ✅ `setInterval` cleanup in Component.exit()
- ✅ Proper resource disposal (banner, tile counter)
- ✅ Null assignments to prevent memory leaks

```javascript
// Component.js (v1.2.0)
exit: function() {
    if (this._pollingInterval) {
        clearInterval(this._pollingInterval);  // ← Memory leak fix
        this._pollingInterval = null;
    }
    if (this._notificationBanner) {
        this._notificationBanner.destroy();
        this._notificationBanner = null;
    }
    if (this._tileCounter) {
        this._tileCounter.destroy();
        this._tileCounter = null;
    }
}
```

### Performance Metrics

| Metric | Target | Actual (v1.2.0) |
|--------|--------|-----------------|
| **API Response Time** | < 200ms | 50-100ms ✅ |
| **UI Render Time** | < 100ms | 30-50ms ✅ |
| **Polling Interval** | 30s | 30s ✅ |
| **Tile Update** | 60s | 60s ✅ |
| **Memory Leak** | None | Fixed ✅ |
| **localStorage Usage** | < 1MB | < 100KB ✅ |

---

## 📊 API Endpoints

| Method | Endpoint | Description | Version |
|--------|----------|-------------|---------|
| `GET` | `/sap/bc/rest/zcl_notif_rest/` | Get active notifications | v1.0.0 |
| `GET` | `/sap/bc/rest/zcl_notif_rest/stats` | Get tile statistics (OData format) | v1.1.0 |
| `GET` | `/sap/bc/rest/zcl_notif_rest/log` | Get SILENT notifications log | v1.1.0 |
| `POST` | `/sap/bc/rest/zcl_notif_rest/` | Create notification | v1.0.0 |
| `POST` | `/sap/bc/rest/zcl_notif_rest/acknowledge` | Record user acknowledgment | v1.3.0 |
| `PUT` | `/sap/bc/rest/zcl_notif_rest/` | Update notification | v1.0.0 |
| `DELETE` | `/sap/bc/rest/zcl_notif_rest/` | Delete notification | v1.0.0 |

### Acknowledgment Endpoint (v1.3.0)

**POST** `/sap/bc/rest/zcl_notif_rest/acknowledge`

**Request Body**:
```json
{
  "message_id": "abc123",
  "client_info": "Mozilla/5.0 (Windows NT 10.0; Win64; x64)..."
}
```

**Response Codes**:
- `200 OK`: Acknowledgment recorded successfully
- `400 Bad Request`: Missing message_id
- `409 Conflict`: Already acknowledged by this user
- `500 Internal Server Error`: Database or system error

**Response Body** (Success):
```json
{
  "success": true,
  "timestamp": "20251010T143022"
}
```

**Response Body** (Error):
```json
{
  "success": false,
  "error": "Already acknowledged"
}
```

---

## 🆕 What's New in v1.3.0

### User Acknowledgment Tracking System

**🎯 Feature**: Critical notifications now require explicit user acknowledgment before dismissal.

**Key Components**:

1. **Frontend (NotificationBanner.js)**
   - ✅ "OK" button replaces "X" for `requires_ack='X'` notifications
   - ✅ POST to `/acknowledge` endpoint with message_id and browser info
   - ✅ localStorage tracking for already-acknowledged notifications
   - ✅ Acknowledged notifications don't reappear for same user

2. **Backend (ZCL_NOTIFICATION_MANAGER)**
   - ✅ `has_user_acknowledged()` - Check if user already acknowledged
   - ✅ `record_acknowledgment()` - INSERT into ZNOTIFY_ACK_LOG with duplicate prevention
   - ✅ `get_acknowledgments()` - Query acknowledgment history for reporting
   - ✅ Updated `get_active_notifications()` to filter out acknowledged messages

3. **REST API (ZCL_NOTIF_REST)**
   - ✅ `handle_acknowledge()` - New POST /acknowledge endpoint
   - ✅ JSON parsing (message_id + client_info)
   - ✅ Status codes: 200 (success), 409 (duplicate), 400 (bad request), 500 (error)

4. **Database**
   - ✅ New table: `ZNOTIFY_ACK_LOG` (composite PK: MANDT + MESSAGE_ID + USERID)
   - ✅ New field: `ZTNOTIFY_MSGS.REQUIRES_ACK` (CHAR1)
   - ✅ Updated CDS view: `ztnotify_messages` exposes requires_ack field
   - ✅ Integration with ZNOTIF_MATRIX for auto-setting requires_ack

**Business Rules**:
- `requires_ack='X'` automatically set for:
  - HIGH severity + BANNER/BOTH display mode
  - URGENT message type + BANNER/BOTH display mode
- Acknowledgment log records:
  - Who acknowledged (userid)
  - When acknowledged (timestamp with milliseconds)
  - Browser/device info (user agent string)

**Benefits**:
- ✅ Audit trail for critical notifications
- ✅ Compliance reporting (who read what and when)
- ✅ Better UX (clear "OK" vs ambiguous "X")
- ✅ Smart filtering (don't re-show acknowledged notifications)

---

## 🆕 What's New in v1.2.0

### Code Quality & Best Practices

1. **SAP Logging Standard**
   - ✅ Replaced `console.log` with `sap/base/Log`
   - ✅ Proper logging levels (Info, Warning, Error)

2. **Memory Leak Prevention**
   - ✅ Fixed `setInterval` cleanup
   - ✅ Proper component lifecycle management
   - ✅ Resource disposal in exit() method

3. **Documentation Restructure**
   - ✅ Separated backend/frontend deployment guides
   - ✅ Modern SAP approaches only (Spaces & Pages, FLP Plugin)
   - ✅ Removed ~3,000 lines of obsolete documentation
   - ✅ Created BACKEND_DEPLOYMENT.md (967 lines)
   - ✅ Streamlined DEPLOYMENT_GUIDE.md (510 lines)
   - ✅ Simplified FLP_CONFIGURATION.md (514 lines)

4. **Code Cleanup**
   - ✅ Removed deploy-sap/ folder (11 files, ~950 lines)
   - ✅ Removed TESTING_GUIDE.md (545 lines)
   - ✅ Removed SE80_IMPORT_CHECKLIST.md
   - ✅ Fixed ESLint warnings

---

## 👥 Stakeholders

| Role | Responsibilities | Contact |
|------|------------------|---------|
| **Architects** | Architecture, system design | Gabriele Rendina & Ileana Scaglia |
| **Technical Lead** | Backend code, ABAP development | Gabriele Rendina |
| **Frontend Lead** | UI5, UX, responsive design | Ileana Scaglia |
| **SAP Basis** | Infrastructure, authorizations | Basis Team |

---

## 📚 Technical References

- **UI5 Documentation**: https://ui5.sap.com/
- **ABAP Development**: SAP NetWeaver AS ABAP
- **CDS Views**: ABAP Core Data Services
- **REST Services**: ICF/SICF Framework
- **Authorization**: SAP Authorization Concept

---

**Architecture v1.3.0 - User Acknowledgment Tracking**
*Designed by Gabriele Rendina and Ileana Scaglia*
*Last updated: October 10, 2025*
