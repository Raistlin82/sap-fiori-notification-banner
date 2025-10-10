# ✅ ABAP Implementation Checklist - Acknowledgment System

## Quick Reference

**File con tutto il codice**: `/abap/ACKNOWLEDGMENT_ABAP_CODE.abap`

**Tempo stimato**: 2-3 ore

---

## 📋 Step-by-Step Checklist

### ✅ STEP 1: Database Setup (30 minuti)

#### 1.1 Verifica campo REQUIRES_ACK

```sql
-- Esegui questa query in HANA Studio
SELECT requires_ack FROM ztnotify_msgs WHERE rownum < 1;
```

**Se funziona** → Campo esiste! ✅ Vai a 1.2
**Se errore** → Campo non esiste, esegui:

```sql
ALTER TABLE ztnotify_msgs ADD (
  requires_ack CHAR(1) DEFAULT '' NULL
);
COMMIT;
```

#### 1.2 Popola REQUIRES_ACK per notifiche esistenti

```sql
-- Imposta requires_ack='X' per notifiche critiche
UPDATE ztnotify_msgs
SET requires_ack = 'X'
WHERE severity = 'HIGH'
  AND display_mode IN ('BANNER', 'BOTH')
  AND (requires_ack IS NULL OR requires_ack = '');

UPDATE ztnotify_msgs
SET requires_ack = 'X'
WHERE message_type = 'URGENT'
  AND display_mode IN ('BANNER', 'BOTH')
  AND (requires_ack IS NULL OR requires_ack = '');

COMMIT;
```

**Verifica**:
```sql
SELECT requires_ack, COUNT(*) as count
FROM ztnotify_msgs
GROUP BY requires_ack;
```

Expected output:
```
REQUIRES_ACK | COUNT
-------------+------
X            | 5     (o più - notifiche critiche)
(blank)      | 15    (o più - notifiche normali)
```

#### 1.3 Crea tabella ZNOTIFY_ACK_LOG

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

COMMIT;
```

**Verifica**:
```sql
SELECT * FROM znotify_ack_log WHERE rownum < 1;
```

Expected: No rows, no errors ✅

---

### ✅ STEP 2: Update View Entity (5 minuti)

**File**: `/abap/ztnotify_messages.ddls`

**Status**: ✅ GIÀ FATTO! (campo `requires_ack` aggiunto alla select list)

Verifica:
```abap
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Global Notification Messages'

define view entity ztnotify_messages
  as select from ztnotify_msgs
{
  key message_id,
      ...
      display_mode,
      requires_ack,  ← Deve esserci questa riga!
      created_by,
      ...
}
```

**Activate** la view entity in Eclipse/SE80.

---

### ✅ STEP 3: ZCL_NOTIFICATION_MANAGER Class (60 minuti)

**File sorgente**: `/abap/ACKNOWLEDGMENT_ABAP_CODE.abap` (Section 2 & 3)

#### 3.1 Aggiungi Type Definitions (PUBLIC SECTION)

Apri `ZCL_NOTIFICATION_MANAGER` in SE80/Eclipse.

Nel **PUBLIC SECTION**, dopo `tt_notifications`, aggiungi:

```abap
TYPES: BEGIN OF ty_acknowledgment,
         message_id    TYPE char32,
         userid        TYPE syuname,
         ack_timestamp TYPE timestampl,
         client_info   TYPE char255,
       END OF ty_acknowledgment.

TYPES: tt_acknowledgments TYPE STANDARD TABLE OF ty_acknowledgment WITH DEFAULT KEY.
```

#### 3.2 Aggiungi Method Definitions (PUBLIC SECTION)

Dopo i method esistenti, aggiungi:

```abap
CLASS-METHODS: has_user_acknowledged
                 IMPORTING
                   iv_message_id TYPE char32
                   iv_user_id TYPE sy-uname DEFAULT sy-uname
                 RETURNING
                   VALUE(rv_acknowledged) TYPE abap_bool,

               record_acknowledgment
                 IMPORTING
                   iv_message_id TYPE char32
                   iv_user_id TYPE sy-uname DEFAULT sy-uname
                   iv_client_info TYPE char255 OPTIONAL
                 RETURNING
                   VALUE(rv_success) TYPE abap_bool,

               get_acknowledgments
                 IMPORTING
                   iv_message_id TYPE char32
                 RETURNING
                   VALUE(rt_acknowledgments) TYPE tt_acknowledgments.
```

#### 3.3 Implementa i 3 metodi

**Copia dal file** `/abap/ACKNOWLEDGMENT_ABAP_CODE.abap` (Section 3):
- `has_user_acknowledged` implementation (30 righe)
- `record_acknowledgment` implementation (50 righe)
- `get_acknowledgments` implementation (25 righe)

#### 3.4 Modifica get_active_notifications

**Trova** il metodo `get_active_notifications`.

**Alla fine**, prima del `ENDMETHOD`, aggiungi il filtro:

```abap
" Filter out acknowledged notifications for this user
DATA(lv_user) = COND #( WHEN iv_user_id IS NOT INITIAL
                        THEN iv_user_id
                        ELSE sy-uname ).

LOOP AT rt_notifications ASSIGNING FIELD-SYMBOL(<notif>).
  IF <notif>-requires_ack = 'X'.
    IF has_user_acknowledged( iv_message_id = <notif>-message_id
                             iv_user_id = lv_user ) = abap_true.
      DELETE rt_notifications.
    ENDIF.
  ENDIF.
ENDLOOP.
```

**Save & Activate** ✅

---

### ✅ STEP 4: ZCL_NOTIF_REST Class (60 minuti)

**File sorgente**: `/abap/ACKNOWLEDGMENT_ABAP_CODE.abap` (Section 4, 5, 6)

#### 4.1 Aggiungi Method Definition (PRIVATE SECTION)

```abap
METHODS: handle_acknowledge
           IMPORTING
             request TYPE REF TO if_rest_request
             response TYPE REF TO if_rest_response.
```

Optional (per query acknowledgments):
```abap
METHODS: handle_get_acknowledgments
           IMPORTING
             request TYPE REF TO if_rest_request
             response TYPE REF TO if_rest_response.
```

#### 4.2 Implementa handle_acknowledge

**Copia completo** dal file `/abap/ACKNOWLEDGMENT_ABAP_CODE.abap` (Section 4):
- ~80 righe di codice
- Gestisce POST /acknowledge
- Parse JSON request
- Chiama record_acknowledgment
- Ritorna 200/409/500 responses

#### 4.3 Modifica if_rest_resource~post (Routing)

**Trova** il metodo `if_rest_resource~post`.

**Sostituisci** la logica di routing:

```abap
METHOD if_rest_resource~post.
  DATA: lv_path      TYPE string,
        lt_segments  TYPE string_table,
        lv_segment   TYPE string.

  " Get URL path
  lv_path = request->get_header_field( '~path_info' ).

  " Parse segments
  SPLIT lv_path AT '/' INTO TABLE lt_segments.
  READ TABLE lt_segments INDEX 2 INTO lv_segment.

  " Route based on endpoint
  CASE lv_segment.
    WHEN 'acknowledge'.
      handle_acknowledge(
        request  = request
        response = response
      ).

    WHEN OTHERS.
      " Existing create notification logic
      handle_create_notification(
        request  = request
        response = response
      ).
  ENDCASE.
ENDMETHOD.
```

**Save & Activate** ✅

---

### ✅ STEP 5: Testing (30 minuti)

#### Test 1: Crea notifica test

Via SQL:
```sql
INSERT INTO ztnotify_msgs VALUES (
  '100', 'test_ack_123', 'URGENT', 'HIGH',
  'TEST: Please Acknowledge',
  'Click OK to acknowledge this notification',
  '20251009', '20251231', 'ALL', 'X', 'BANNER', 'X',
  'TESTUSER', NULL, NULL, NULL
);
COMMIT;
```

Via ABAP (usa report in Section 7 del file):
```abap
REPORT z_test_ack_create.
" ... copy code from ACKNOWLEDGMENT_ABAP_CODE.abap Section 7 ...
```

#### Test 2: Verifica in Fiori Launchpad

1. Login come test user
2. Vai alla FLP
3. Dovresti vedere banner con **"OK - I Understand"** button
4. Click OK
5. Toast: "Notification acknowledged"
6. Banner scompare

#### Test 3: Verifica database

```sql
-- Check acknowledgment log
SELECT * FROM znotify_ack_log
WHERE message_id = 'test_ack_123';

-- Expected:
-- MANDT | MESSAGE_ID    | USERID   | ACK_TIMESTAMP      | CLIENT_INFO
-- 100   | test_ack_123  | TESTUSER | 20251009123045.123 | Mozilla/5.0...
```

#### Test 4: Verifica non riappare

1. Refresh browser
2. Banner NON deve riapparire ✅
3. Check console logs: "Already acknowledged"

---

## 🔍 Troubleshooting

### Issue: Campo requires_ack non esiste

**Sintomo**: Error "Column REQUIRES_ACK not found"

**Fix**:
```sql
ALTER TABLE ztnotify_msgs ADD (requires_ack CHAR(1) DEFAULT '' NULL);
COMMIT;
```

### Issue: Tabella ZNOTIFY_ACK_LOG non esiste

**Sintomo**: Error "Table ZNOTIFY_ACK_LOG does not exist"

**Fix**: Esegui CREATE TABLE from Step 1.3

### Issue: POST /acknowledge returns 500

**Check**:
1. ZNOTIFY_ACK_LOG table exists?
2. record_acknowledgment method implemented?
3. ST22 for ABAP dump?
4. Check JSON parsing in handle_acknowledge

### Issue: OK button non appare

**Check**:
1. requires_ack='X' nel database?
2. View entity includes requires_ack field?
3. Frontend deployed (NotificationBanner.js)?
4. Browser cache cleared?

### Issue: Notification keeps reappearing

**Check**:
1. Entry in ZNOTIFY_ACK_LOG exists?
2. has_user_acknowledged() implemented?
3. Filter logic in get_active_notifications?
4. Check sy-uname matches USERID in table

---

## 📊 Verification Queries

### Query 1: Notifications requiring ack
```sql
SELECT message_id, title, severity, message_type, requires_ack
FROM ztnotify_msgs
WHERE requires_ack = 'X' AND active = 'X';
```

### Query 2: Recent acknowledgments
```sql
SELECT *
FROM znotify_ack_log
ORDER BY ack_timestamp DESC
FETCH FIRST 10 ROWS ONLY;
```

### Query 3: Acknowledgment rate
```sql
SELECT
  m.message_id,
  m.title,
  COUNT(DISTINCT a.userid) AS ack_count
FROM ztnotify_msgs m
LEFT JOIN znotify_ack_log a ON m.message_id = a.message_id
WHERE m.requires_ack = 'X' AND m.active = 'X'
GROUP BY m.message_id, m.title
ORDER BY ack_count DESC;
```

### Query 4: Users who haven't acknowledged
```sql
SELECT u.bname
FROM usr02 u
WHERE u.ustyp = 'A'
  AND u.bname NOT IN (
    SELECT userid FROM znotify_ack_log
    WHERE message_id = 'YOUR_MESSAGE_ID'
  )
ORDER BY u.bname;
```

---

## ✅ Final Checklist

- [ ] **Database**: REQUIRES_ACK field exists and populated
- [ ] **Database**: ZNOTIFY_ACK_LOG table created with indexes
- [ ] **View**: ztnotify_messages includes requires_ack field
- [ ] **Class**: ZCL_NOTIFICATION_MANAGER has 3 new methods
- [ ] **Class**: ZCL_NOTIFICATION_MANAGER filters acknowledged notifications
- [ ] **REST**: ZCL_NOTIF_REST has handle_acknowledge method
- [ ] **REST**: if_rest_resource~post routes /acknowledge correctly
- [ ] **Test**: Created test notification with requires_ack='X'
- [ ] **Test**: Verified OK button appears in FLP
- [ ] **Test**: Clicked OK and verified entry in ZNOTIFY_ACK_LOG
- [ ] **Test**: Verified notification doesn't reappear after acknowledgment

---

## 📚 Files Reference

| File | Purpose |
|------|---------|
| `/abap/ACKNOWLEDGMENT_ABAP_CODE.abap` | Complete ABAP code (copy-paste ready) |
| `/abap/ACKNOWLEDGMENT_SETUP_GUIDE.md` | Detailed setup guide |
| `/ACKNOWLEDGMENT_IMPLEMENTATION_SUMMARY.md` | Quick reference |
| `/IMPLEMENTATION_STATUS.md` | Current status |
| `webapp/controller/NotificationBanner.js` | Frontend (already done ✅) |

---

**Estimated Time**: 2-3 hours total

**Status**: Ready for implementation! 🚀

**Last Updated**: 2025-10-09 23:15 CET
