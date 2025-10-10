# 📊 Notification Matrix Customizing - Detailed Analysis

## Document Information
- **Purpose**: Constrain Display Mode based on Severity and Message Type
- **Proposed By**: User requirement analysis
- **Date**: 2025-10-05
- **Status**: ANALYSIS PHASE

---

## 🎯 Business Problem

### Current Situation
The application currently allows **any combination** of:
- **Severity**: HIGH, MEDIUM, LOW
- **Message Type**: URGENT, INFO, TIP, WARNING, MAINT
- **Display Mode**: BANNER, TOAST, BOTH, SILENT

**Problem**: No validation prevents illogical combinations like:
- ❌ HIGH severity + TOAST mode (disappears after 5s - critical message lost!)
- ❌ LOW severity + BANNER mode (forces user to close non-critical message)
- ❌ URGENT message + SILENT mode (defeats the purpose of urgency)

### Business Impact
- **User Experience**: Inconsistent notification behavior confuses users
- **Compliance Risk**: Critical messages may not be properly acknowledged
- **Operational Inefficiency**: Admins must manually ensure correct combinations

---

## 📋 Current Value Analysis

### Severity Levels (3 values)
| Value | Color | Current Usage Pattern | Recommended Display Modes |
|-------|-------|----------------------|---------------------------|
| **HIGH** 🔴 | Red | Critical system failures, security alerts | BANNER, BOTH (force acknowledgment) |
| **MEDIUM** 🟠 | Orange | Important updates, warnings | BANNER, TOAST, BOTH |
| **LOW** 🟢 | Green | Informational, tips, reminders | TOAST, SILENT (non-intrusive) |

### Message Types (5 values)
| Value | Description | Typical Severity | Recommended Display Modes |
|-------|-------------|------------------|---------------------------|
| **URGENT** ⚠️ | Emergency actions required | HIGH, MEDIUM | BANNER, BOTH (never TOAST/SILENT) |
| **WARNING** ⚡ | Cautionary messages | MEDIUM, LOW | BANNER, TOAST, BOTH |
| **MAINT** 🔧 | Maintenance announcements | MEDIUM, LOW | BANNER, TOAST, BOTH, SILENT |
| **INFO** ℹ️ | General information | LOW, MEDIUM | TOAST, BANNER, BOTH, SILENT |
| **TIP** 💡 | Helpful suggestions | LOW | TOAST, SILENT (least intrusive) |

### Display Modes (4 values)
| Value | Behavior | Auto-Dismiss | Use Case | Compatible Severity |
|-------|----------|--------------|----------|---------------------|
| **BANNER** 🎯 | Fixed top bar, user must close | No | Critical announcements | HIGH, MEDIUM |
| **TOAST** 💬 | Bottom-right, auto-disappears 5s | Yes | Non-critical updates | LOW, (MEDIUM sometimes) |
| **BOTH** 🎪 | Banner + Toast simultaneously | Banner: No, Toast: Yes | Maximum visibility | HIGH, MEDIUM |
| **SILENT** 🔇 | Logged only, no UI | N/A | Audit trail, analytics | LOW, MEDIUM (for tracking) |

---

## 🔧 Proposed Solution: Customizing Table

### Table Structure: `ZNOTIF_MATRIX`

```abap
*----------------------------------------------------------------------*
* Table: ZNOTIF_MATRIX - Notification Combination Matrix
* Purpose: Define valid combinations of Severity, Message Type, Display Mode
* Delivery: One-time load via transport, customizable via SM30
*----------------------------------------------------------------------*
@EndUserText.label : 'Notification Matrix Customizing'
@AbapCatalog.deliveryClass : #C  "Customizing table, client-dependent"
@AbapCatalog.dataMaintenance : #ALLOWED  "SM30 maintenance allowed"

define table znotif_matrix {
  key client        : mandt;
  key severity      : char8;           "HIGH, MEDIUM, LOW
  key message_type  : char12;          "URGENT, INFO, TIP, WARNING, MAINT
  key display_mode  : char10;          "BANNER, TOAST, BOTH, SILENT

  @EndUserText.label : 'Active'
  active            : abap_bool;       "X = allowed combination

  @EndUserText.label : 'Default'
  is_default        : abap_bool;       "X = default for this severity+type

  @EndUserText.label : 'Requires Acknowledgment'
  requires_ack      : abap_bool;       "X = must track user acknowledgment

  @EndUserText.label : 'Priority'
  sort_order        : numc2;           "Display order in UI dropdown (01-99)

  @EndUserText.label : 'Description'
  description       : char255;         "Human-readable explanation

  @EndUserText.label : 'Changed By'
  changed_by        : syuname;

  @EndUserText.label : 'Changed At'
  changed_at        : timestampl;
}
```

### Alternative: Simpler Structure (KISS Principle)

```abap
*----------------------------------------------------------------------*
* Simplified table if you prefer less fields
*----------------------------------------------------------------------*
@EndUserText.label : 'Notification Matrix (Simple)'
define table znotif_matrix {
  key client        : mandt;
  key severity      : char8;
  key message_type  : char12;
  key display_mode  : char10;
  is_default        : abap_bool;       "X = default for this combination
  requires_ack      : abap_bool;       "X = track acknowledgment
  changed_by        : syuname;
  changed_at        : timestampl;
}
```

**Note**: In simplified version, if a combination exists in table → valid. If not exists → invalid.

---

## 🎨 Proposed Matrix - Default Configuration

### Matrix Table (✅ = Valid, 🔵 = Default, 🔒 = Requires Acknowledgment)

#### **HIGH Severity** 🔴

| Message Type | BANNER | TOAST | BOTH | SILENT | Rationale |
|--------------|--------|-------|------|--------|-----------|
| **URGENT** | ✅🔵🔒 | ❌ | ✅🔒 | ❌ | Critical messages need visible acknowledgment, no auto-dismiss |
| **WARNING** | ✅🔵🔒 | ❌ | ✅🔒 | ❌ | High warnings must be acknowledged |
| **MAINT** | ✅🔵🔒 | ❌ | ✅ | ❌ | Critical maintenance = must see banner |
| **INFO** | ✅🔵 | ❌ | ✅ | ✅ | High-severity info should be persistent |
| **TIP** | ❌ | ❌ | ❌ | ❌ | Tips are never high severity (illogical) |

#### **MEDIUM Severity** 🟠

| Message Type | BANNER | TOAST | BOTH | SILENT | Rationale |
|--------------|--------|-------|------|--------|-----------|
| **URGENT** | ✅🔵🔒 | ❌ | ✅🔒 | ❌ | Urgent messages need banner even at medium level |
| **WARNING** | ✅🔵 | ✅ | ✅ | ❌ | Flexible - admin chooses visibility level |
| **MAINT** | ✅🔵 | ✅ | ✅ | ✅ | Planned maintenance can be less intrusive |
| **INFO** | ✅ | ✅🔵 | ✅ | ✅ | Medium info = toast is often sufficient |
| **TIP** | ✅ | ✅🔵 | ❌ | ✅ | Medium tips can use toast or banner |

#### **LOW Severity** 🟢

| Message Type | BANNER | TOAST | BOTH | SILENT | Rationale |
|--------------|--------|-------|------|--------|-----------|
| **URGENT** | ❌ | ❌ | ❌ | ❌ | Urgent is never low severity (contradiction) |
| **WARNING** | ❌ | ✅🔵 | ❌ | ✅ | Low warnings = non-intrusive toast |
| **MAINT** | ❌ | ✅🔵 | ❌ | ✅ | Low-priority maintenance = toast or silent |
| **INFO** | ❌ | ✅🔵 | ❌ | ✅ | Informational messages = toast preferred |
| **TIP** | ❌ | ✅🔵 | ❌ | ✅ | Tips should never block user (toast/silent only) |

### Summary Statistics
- **Total Valid Combinations**: 31 out of 60 possible (51.7% allowed)
- **Default Selections**: 15 (one per valid Severity+MessageType pair)
- **Requires Acknowledgment**: 8 combinations (all HIGH/URGENT + some MEDIUM)
- **Invalid Combinations**: 29 (prevented by validation)

---

## 🔄 User Acknowledgment Tracking

### Proposed Rule: Track Acknowledgment for BANNER Only

**Rationale**:
- **BANNER**: User must explicitly close → perfect for tracking
- **TOAST**: Auto-disappears after 5s → unreliable for tracking
- **BOTH**: Track banner part only
- **SILENT**: No UI, nothing to acknowledge

**Implementation**:
```abap
" In ZNOTIF_MATRIX table: requires_ack field
" Applies to combinations where:
"   - display_mode IN ('BANNER', 'BOTH')
"   - severity = 'HIGH'
"   OR
"   - message_type = 'URGENT'
```

**Enhanced Table: ZNOTIF_TRACKING** (from Enhancement #2 proposal)
```abap
TYPES: BEGIN OF ty_tracking,
         tracking_id       TYPE sysuuid_x16,
         notification_id   TYPE sysuuid_x16,
         user_id           TYPE syuname,
         status            TYPE char1,   "S=Sent, R=Read, A=Acknowledged
         read_timestamp    TYPE timestamp,
         ack_timestamp     TYPE timestamp,
         display_mode_used TYPE char10,  "Which mode was shown to user
       END OF ty_tracking.
```

**Analytics Benefits**:
- Track which users saw critical messages
- Measure acknowledgment compliance rates
- Identify users who never acknowledge (training need)
- Prove regulatory compliance (e.g., security policy acknowledgment)

---

## 💻 Implementation Impact

### 1. ABAP Backend Changes

#### **New Validation Method**
```abap
CLASS-METHODS: validate_notification_combination
                 IMPORTING
                   iv_severity TYPE char8
                   iv_message_type TYPE char12
                   iv_display_mode TYPE char10
                 EXPORTING
                   ev_valid TYPE abap_bool
                   ev_requires_ack TYPE abap_bool
                   ev_error_message TYPE string
                 RETURNING
                   VALUE(rv_success) TYPE abap_bool.
```

**Implementation**:
```abap
METHOD validate_notification_combination.
  DATA: ls_matrix TYPE znotif_matrix.

  rv_success = abap_false.
  ev_valid = abap_false.
  ev_requires_ack = abap_false.

  " Check if combination exists in customizing table
  SELECT SINGLE *
    FROM znotif_matrix
    INTO @ls_matrix
    WHERE severity = @iv_severity
      AND message_type = @iv_message_type
      AND display_mode = @iv_display_mode.

  IF sy-subrc = 0.
    " Combination found - check if active
    IF ls_matrix-active = abap_true.
      ev_valid = abap_true.
      ev_requires_ack = ls_matrix-requires_ack.
      rv_success = abap_true.
    ELSE.
      ev_error_message = |Combination disabled in customizing|.
    ENDIF.
  ELSE.
    " Combination not found = invalid
    ev_error_message = |Invalid combination: {iv_severity}/{iv_message_type}/{iv_display_mode}|.
  ENDIF.

ENDMETHOD.
```

#### **Modified Create/Update Methods**
```abap
METHOD create_notification.
  " ... existing code ...

  " NEW: Validate combination
  DATA: lv_valid TYPE abap_bool,
        lv_requires_ack TYPE abap_bool,
        lv_error TYPE string.

  validate_notification_combination(
    EXPORTING
      iv_severity = is_notification-severity
      iv_message_type = is_notification-message_type
      iv_display_mode = is_notification-display_mode
    IMPORTING
      ev_valid = lv_valid
      ev_requires_ack = lv_requires_ack
      ev_error_message = lv_error ).

  IF lv_valid = abap_false.
    " Reject creation
    rv_success = abap_false.
    " Could raise exception with lv_error message
    RETURN.
  ENDIF.

  " Store requires_ack flag in notification record
  ls_notification-requires_ack = lv_requires_ack.

  " ... continue with existing creation logic ...
ENDMETHOD.
```

#### **New REST Endpoint: Get Valid Display Modes**
```abap
METHOD handle_get_valid_modes.
  " GET /api/notifications/valid-modes?severity=HIGH&message_type=URGENT
  " Returns: ["BANNER", "BOTH"]

  DATA: lv_severity TYPE char8,
        lv_message_type TYPE char12,
        lt_valid_modes TYPE string_table,
        lt_matrix TYPE STANDARD TABLE OF znotif_matrix.

  lv_severity = mo_server->request->get_form_field( 'severity' ).
  lv_message_type = mo_server->request->get_form_field( 'message_type' ).

  " Query customizing table for valid display modes
  SELECT display_mode
    FROM znotif_matrix
    INTO TABLE @lt_matrix
    WHERE severity = @lv_severity
      AND message_type = @lv_message_type
      AND active = @abap_true
    ORDER BY sort_order.

  LOOP AT lt_matrix INTO DATA(ls_matrix).
    APPEND ls_matrix-display_mode TO lt_valid_modes.
  ENDLOOP.

  " Return JSON array
  DATA(lv_json) = /ui2/cl_json=>serialize( lt_valid_modes ).
  mo_server->response->set_cdata( lv_json ).
ENDMETHOD.
```

### 2. Fiori UI Changes

#### **Conditional Dropdowns - Cascading Selection**
```xml
<!-- Modified: webapp/view/View1.view.xml -->

<!-- Step 1: Select Severity -->
<Label text="Severity" required="true"/>
<Select id="selectSeverity"
        selectedKey="{editMode>/severity}"
        change=".onSeverityChange">  <!-- NEW: triggers cascade -->
    <items>
        <core:Item key="HIGH" text="High"/>
        <core:Item key="MEDIUM" text="Medium"/>
        <core:Item key="LOW" text="Low"/>
    </items>
</Select>

<!-- Step 2: Select Message Type -->
<Label text="Message Type" required="true"/>
<Select id="selectMessageType"
        selectedKey="{editMode>/message_type}"
        change=".onMessageTypeChange">  <!-- NEW: triggers display mode reload -->
    <items>
        <core:Item key="URGENT" text="Urgent"/>
        <core:Item key="WARNING" text="Warning"/>
        <core:Item key="MAINT" text="Maintenance"/>
        <core:Item key="INFO" text="Information"/>
        <core:Item key="TIP" text="Tip"/>
    </items>
</Select>

<!-- Step 3: Select Display Mode (Dynamic Items) -->
<Label text="Display Mode" required="true"/>
<Select id="selectDisplayMode"
        selectedKey="{editMode>/display_mode}"
        enabled="{= ${editMode>/severity} &amp;&amp; ${editMode>/message_type} }"
        items="{editMode>/validDisplayModes}">  <!-- NEW: dynamic binding -->
    <core:Item key="{editMode>key}" text="{editMode>text}"/>
</Select>

<!-- Helper Text -->
<FormattedText htmlText="{editMode>/matrixHelpText}" class="sapUiTinyMarginTop"/>
```

**Controller Logic**:
```javascript
// webapp/controller/View1.controller.js

onSeverityChange: function() {
    // Clear dependent fields when severity changes
    var oEditModel = this.getView().getModel("editMode");
    oEditModel.setProperty("/display_mode", "");
    oEditModel.setProperty("/validDisplayModes", []);

    // Reload valid display modes if message_type is already selected
    if (oEditModel.getProperty("/message_type")) {
        this._loadValidDisplayModes();
    }
},

onMessageTypeChange: function() {
    // Reload valid display modes
    this._loadValidDisplayModes();
},

_loadValidDisplayModes: function() {
    var oEditModel = this.getView().getModel("editMode");
    var severity = oEditModel.getProperty("/severity");
    var messageType = oEditModel.getProperty("/message_type");

    if (!severity || !messageType) {
        return;
    }

    // Call backend to get valid display modes
    jQuery.ajax({
        url: "/sap/bc/rest/zcl_notif_rest/valid-modes",
        method: "GET",
        data: {
            severity: severity,
            message_type: messageType
        },
        success: function(aValidModes) {
            // Convert array to dropdown items
            var aItems = aValidModes.map(function(mode) {
                return {
                    key: mode,
                    text: this._getDisplayModeText(mode)
                };
            }.bind(this));

            oEditModel.setProperty("/validDisplayModes", aItems);

            // Auto-select default if only one option
            if (aItems.length === 1) {
                oEditModel.setProperty("/display_mode", aItems[0].key);
            }

            // Show help text
            var helpText = aItems.length === 0
                ? "<strong style='color:red;'>No valid display modes for this combination</strong>"
                : "Available modes: " + aItems.length;
            oEditModel.setProperty("/matrixHelpText", helpText);
        }.bind(this),
        error: function() {
            MessageToast.show("Error loading valid display modes");
        }
    });
},

_getDisplayModeText: function(mode) {
    var texts = {
        "BANNER": "Banner (fixed top bar)",
        "TOAST": "Toast (auto-dismiss 5s)",
        "BOTH": "Both (banner + toast)",
        "SILENT": "Silent (audit only)"
    };
    return texts[mode] || mode;
}
```

### 3. Data Migration Strategy

#### **Scenario A: Existing Notifications with Invalid Combinations**

**Option 1: Automatic Correction** (Recommended)
```abap
REPORT z_migrate_notification_matrix.

DATA: lt_notifications TYPE STANDARD TABLE OF ztnotify_msgs,
      lv_corrected TYPE i,
      lv_invalid TYPE i.

" Find all notifications with invalid combinations
SELECT * FROM ztnotify_msgs INTO TABLE @lt_notifications.

LOOP AT lt_notifications INTO DATA(ls_notif).

  " Check if current combination is valid
  SELECT SINGLE @abap_true
    FROM znotif_matrix
    INTO @DATA(lv_exists)
    WHERE severity = @ls_notif-severity
      AND message_type = @ls_notif-message_type
      AND display_mode = @ls_notif-display_mode
      AND active = @abap_true.

  IF sy-subrc <> 0.
    " Invalid combination - find default for this severity+type
    SELECT SINGLE display_mode
      FROM znotif_matrix
      INTO @DATA(lv_default_mode)
      WHERE severity = @ls_notif-severity
        AND message_type = @ls_notif-message_type
        AND is_default = @abap_true.

    IF sy-subrc = 0.
      " Update to default mode
      UPDATE ztnotify_msgs
        SET display_mode = @lv_default_mode
        WHERE message_id = @ls_notif-message_id.
      ADD 1 TO lv_corrected.

      WRITE: / 'Corrected:', ls_notif-message_id, ls_notif-display_mode, '->', lv_default_mode.
    ELSE.
      " No default found - mark for manual review
      ADD 1 TO lv_invalid.
      WRITE: / 'MANUAL REVIEW:', ls_notif-message_id.
    ENDIF.
  ENDIF.

ENDLOOP.

WRITE: / 'Migration complete:', lv_corrected, 'corrected,', lv_invalid, 'require manual review'.
```

**Option 2: Report + Manual Correction**
```abap
" Generate Excel report of invalid combinations for admin to review
REPORT z_report_invalid_combinations.
" Export to XLSX with recommendations
" Admin manually corrects via SM30 or admin UI
```

#### **Scenario B: Pre-Loading ZNOTIF_MATRIX Table**

**Step 1: Create Transport with Default Matrix**
```abap
" Transport: DEVK9xxxxx - Notification Matrix Default Config
" Contains ~31 entries as per proposed matrix above

" Delivery via table maintenance generator (SM30)
" Or via ABAP program that inserts default values
```

**Step 2: Initial Load Program**
```abap
REPORT z_load_notif_matrix_defaults.

DATA: lt_matrix TYPE STANDARD TABLE OF znotif_matrix.

" HIGH + URGENT
APPEND VALUE #(
  client = sy-mandt
  severity = 'HIGH'
  message_type = 'URGENT'
  display_mode = 'BANNER'
  active = abap_true
  is_default = abap_true
  requires_ack = abap_true
  sort_order = '01'
  description = 'Critical urgent messages require banner with acknowledgment'
) TO lt_matrix.

APPEND VALUE #(
  severity = 'HIGH'
  message_type = 'URGENT'
  display_mode = 'BOTH'
  active = abap_true
  requires_ack = abap_true
  sort_order = '02'
  description = 'Maximum visibility for critical urgent messages'
) TO lt_matrix.

" ... repeat for all 31 valid combinations ...

" Insert into table
INSERT znotif_matrix FROM TABLE lt_matrix.

IF sy-subrc = 0.
  COMMIT WORK.
  WRITE: / 'Successfully loaded', lines( lt_matrix ), 'matrix entries'.
ELSE.
  ROLLBACK WORK.
  WRITE: / 'Error loading matrix entries'.
ENDIF.
```

---

## 🎯 Benefits Analysis

### Immediate Benefits
1. **Consistency** ✅
   - Prevents illogical combinations (HIGH + TOAST, etc.)
   - Ensures critical messages always use appropriate display modes
   - Standardizes notification behavior across the organization

2. **User Experience** ✅
   - Users see predictable notification patterns
   - Critical messages never auto-dismiss
   - Non-critical messages don't block workflow

3. **Compliance** ✅
   - Tracks acknowledgment for critical messages only
   - Provides audit trail for HIGH severity notifications
   - Demonstrates regulatory compliance (e.g., security policies)

4. **Operational Efficiency** ✅
   - Admins guided by system to correct choices
   - Reduces training time (UI enforces rules)
   - Prevents notification "spam" from incorrect settings

### Long-term Benefits
1. **Analytics Foundation** ✅
   - Acknowledgment tracking enables analytics app
   - Measure notification effectiveness by combination
   - Identify optimal patterns for future notifications

2. **Flexibility** ✅
   - Customizing table = business can adjust rules without code changes
   - Can enable/disable combinations as policies evolve
   - Can add new severity levels or message types later

3. **Maintainability** ✅
   - Single source of truth for valid combinations
   - Easy to update via SM30 (no code deployment needed)
   - Self-documenting (description field explains why)

---

## ⚠️ Risks & Mitigation

| Risk | Impact | Probability | Mitigation |
|------|--------|-------------|------------|
| **Existing notifications become invalid** | High | High | Run migration report before activation, auto-correct to defaults |
| **Users frustrated by disabled options** | Medium | Medium | Show helpful error messages explaining why combination is invalid |
| **Customizing table incorrectly configured** | High | Low | Provide comprehensive default load, add validation checks in SM30 |
| **Performance impact from extra SELECT** | Low | Low | Add database index on (severity, message_type, display_mode), cache results in memory |

---

## 📝 Implementation Recommendation

### Phased Approach (3 Sprints)

**Sprint 1: Foundation (5 days)**
- Create ZNOTIF_MATRIX table
- Implement validation method in zcl_notification_manager
- Load default matrix via transport
- Create SM30 maintenance view

**Sprint 2: Backend Integration (3 days)**
- Add validation to create/update notification methods
- Implement GET /valid-modes REST endpoint
- Run migration report on existing notifications
- Add requires_ack field to ZTNOTIFY_MSGS table

**Sprint 3: Frontend Integration (4 days)**
- Implement cascading dropdowns in UI
- Add onSeverityChange/onMessageTypeChange handlers
- Show helpful validation messages
- Update admin guide documentation

**Total Estimate**: ~12 days development + 3 days testing = **3 weeks**

---

## 🔍 Alternative Approaches Considered

### Alternative 1: Hardcoded Validation (NOT RECOMMENDED)
```abap
" Problems:
" - Code change required for any rule update
" - No flexibility for customer-specific requirements
" - Difficult to maintain as rules evolve
```

### Alternative 2: JSON Configuration File (NOT RECOMMENDED FOR SAP)
```javascript
// Problems:
// - Not standard SAP approach
// - Difficult to transport across systems
// - No transaction code for maintenance
// - Version control issues
```

### Alternative 3: Validation Only (No Matrix Table) (PARTIAL)
```abap
" Could implement simple IF/ELSE validation without customizing table
" Pros: Simpler, faster to implement
" Cons: Less flexible, requires code changes to modify rules
```

**Decision**: **Customizing table approach is best** for SAP standard, flexibility, and maintainability.

---

## ✅ Conclusion & Recommendation

**Recommendation**: **PROCEED with ZNOTIF_MATRIX customizing table approach**

**Key Points**:
1. ✅ Solves real business problem (prevents illogical combinations)
2. ✅ Enables user acknowledgment tracking for analytics
3. ✅ Follows SAP standard patterns (customizing table + SM30)
4. ✅ Provides flexibility without code changes
5. ✅ Clear migration path for existing data
6. ✅ Reasonable implementation effort (3 weeks)

**Next Steps**:
1. Review this analysis with stakeholders
2. Approve proposed matrix configuration
3. Decide on migration strategy (auto-correct vs. manual review)
4. Create development task in backlog
5. Implement in 3-sprint approach

---

## 📎 Appendix: Full Matrix Configuration (CSV Export)

```csv
Severity,MessageType,DisplayMode,Active,IsDefault,RequiresAck,SortOrder,Description
HIGH,URGENT,BANNER,X,X,X,01,"Critical urgent messages with mandatory acknowledgment"
HIGH,URGENT,BOTH,X,,X,02,"Maximum visibility for critical urgent messages"
HIGH,WARNING,BANNER,X,X,X,03,"High severity warnings require banner acknowledgment"
HIGH,WARNING,BOTH,X,,X,04,"High warnings with dual display"
HIGH,MAINT,BANNER,X,X,X,05,"Critical maintenance must use banner"
HIGH,MAINT,BOTH,X,,,06,"Critical maintenance with toast supplement"
HIGH,INFO,BANNER,X,X,,07,"High severity information persistent display"
HIGH,INFO,BOTH,X,,,08,"High info with dual visibility"
HIGH,INFO,SILENT,X,,,09,"High info for audit trail only"
MEDIUM,URGENT,BANNER,X,X,X,10,"Medium urgent still requires banner acknowledgment"
MEDIUM,URGENT,BOTH,X,,X,11,"Medium urgent with dual display"
MEDIUM,WARNING,BANNER,X,X,,12,"Medium warnings default to banner"
MEDIUM,WARNING,TOAST,X,,,13,"Medium warnings can use toast if less critical"
MEDIUM,WARNING,BOTH,X,,,14,"Medium warnings with dual display"
MEDIUM,MAINT,BANNER,X,X,,15,"Planned maintenance banner display"
MEDIUM,MAINT,TOAST,X,,,16,"Minor maintenance as toast"
MEDIUM,MAINT,BOTH,X,,,17,"Maintenance with dual display"
MEDIUM,MAINT,SILENT,X,,,18,"Maintenance for audit purposes"
MEDIUM,INFO,BANNER,X,,,19,"Medium info can use banner for visibility"
MEDIUM,INFO,TOAST,X,X,,20,"Medium info default to toast"
MEDIUM,INFO,BOTH,X,,,21,"Medium info with dual display"
MEDIUM,INFO,SILENT,X,,,22,"Medium info for tracking only"
MEDIUM,TIP,BANNER,X,,,23,"Medium tips can use banner if important"
MEDIUM,TIP,TOAST,X,X,,24,"Medium tips default to toast"
MEDIUM,TIP,SILENT,X,,,25,"Medium tips for reference only"
LOW,WARNING,TOAST,X,X,,26,"Low warnings as non-intrusive toast"
LOW,WARNING,SILENT,X,,,27,"Low warnings for audit trail"
LOW,MAINT,TOAST,X,X,,28,"Low maintenance announcements as toast"
LOW,MAINT,SILENT,X,,,29,"Low maintenance for logging"
LOW,INFO,TOAST,X,X,,30,"Low information as toast notification"
LOW,INFO,SILENT,X,,,31,"Low information for tracking purposes"
LOW,TIP,TOAST,X,X,,32,"Tips displayed as toast"
LOW,TIP,SILENT,X,,,33,"Tips logged for reference"
```

**Note**: X in Active column = combination is allowed. X in IsDefault column = auto-selected when user picks that severity+message_type pair.

---

**Document Status**: Ready for stakeholder review
**Implementation Priority**: HIGH (foundational for user tracking analytics)
**Estimated Effort**: 3 weeks (development + testing)
