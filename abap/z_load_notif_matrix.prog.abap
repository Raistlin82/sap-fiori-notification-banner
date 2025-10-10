*&---------------------------------------------------------------------*
*& Report Z_LOAD_NOTIF_MATRIX
*&---------------------------------------------------------------------*
*& Purpose: Load default notification matrix customizing
*& Run once during initial setup to populate ZNOTIF_MATRIX table
*& Based on matrix defined in NOTIFICATION_MATRIX_ANALYSIS.md
*&---------------------------------------------------------------------*
REPORT z_load_notif_matrix.

DATA: lt_matrix TYPE STANDARD TABLE OF znotif_matrix,
      lv_count TYPE i,
      lv_timestamp TYPE timestampl.

GET TIME STAMP FIELD lv_timestamp.

*----------------------------------------------------------------------*
* HIGH Severity Combinations
*----------------------------------------------------------------------*

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
  description = 'Critical urgent messages with mandatory acknowledgment'
  changed_by = sy-uname
  changed_at = lv_timestamp
) TO lt_matrix.

APPEND VALUE #(
  client = sy-mandt
  severity = 'HIGH'
  message_type = 'URGENT'
  display_mode = 'BOTH'
  active = abap_true
  is_default = abap_false
  requires_ack = abap_true
  sort_order = '02'
  description = 'Maximum visibility for critical urgent messages'
  changed_by = sy-uname
  changed_at = lv_timestamp
) TO lt_matrix.

" HIGH + WARNING
APPEND VALUE #(
  client = sy-mandt
  severity = 'HIGH'
  message_type = 'WARNING'
  display_mode = 'BANNER'
  active = abap_true
  is_default = abap_true
  requires_ack = abap_true
  sort_order = '03'
  description = 'High severity warnings require banner acknowledgment'
  changed_by = sy-uname
  changed_at = lv_timestamp
) TO lt_matrix.

APPEND VALUE #(
  client = sy-mandt
  severity = 'HIGH'
  message_type = 'WARNING'
  display_mode = 'BOTH'
  active = abap_true
  is_default = abap_false
  requires_ack = abap_true
  sort_order = '04'
  description = 'High warnings with dual display'
  changed_by = sy-uname
  changed_at = lv_timestamp
) TO lt_matrix.

" HIGH + MAINT
APPEND VALUE #(
  client = sy-mandt
  severity = 'HIGH'
  message_type = 'MAINT'
  display_mode = 'BANNER'
  active = abap_true
  is_default = abap_true
  requires_ack = abap_true
  sort_order = '05'
  description = 'Critical maintenance must use banner'
  changed_by = sy-uname
  changed_at = lv_timestamp
) TO lt_matrix.

APPEND VALUE #(
  client = sy-mandt
  severity = 'HIGH'
  message_type = 'MAINT'
  display_mode = 'BOTH'
  active = abap_true
  is_default = abap_false
  requires_ack = abap_false
  sort_order = '06'
  description = 'Critical maintenance with toast supplement'
  changed_by = sy-uname
  changed_at = lv_timestamp
) TO lt_matrix.

" HIGH + INFO
APPEND VALUE #(
  client = sy-mandt
  severity = 'HIGH'
  message_type = 'INFO'
  display_mode = 'BANNER'
  active = abap_true
  is_default = abap_true
  requires_ack = abap_false
  sort_order = '07'
  description = 'High severity information persistent display'
  changed_by = sy-uname
  changed_at = lv_timestamp
) TO lt_matrix.

APPEND VALUE #(
  client = sy-mandt
  severity = 'HIGH'
  message_type = 'INFO'
  display_mode = 'BOTH'
  active = abap_true
  is_default = abap_false
  requires_ack = abap_false
  sort_order = '08'
  description = 'High info with dual visibility'
  changed_by = sy-uname
  changed_at = lv_timestamp
) TO lt_matrix.

APPEND VALUE #(
  client = sy-mandt
  severity = 'HIGH'
  message_type = 'INFO'
  display_mode = 'SILENT'
  active = abap_true
  is_default = abap_false
  requires_ack = abap_false
  sort_order = '09'
  description = 'High info for audit trail only'
  changed_by = sy-uname
  changed_at = lv_timestamp
) TO lt_matrix.

*----------------------------------------------------------------------*
* MEDIUM Severity Combinations
*----------------------------------------------------------------------*

" MEDIUM + URGENT
APPEND VALUE #(
  client = sy-mandt
  severity = 'MEDIUM'
  message_type = 'URGENT'
  display_mode = 'BANNER'
  active = abap_true
  is_default = abap_true
  requires_ack = abap_true
  sort_order = '10'
  description = 'Medium urgent still requires banner acknowledgment'
  changed_by = sy-uname
  changed_at = lv_timestamp
) TO lt_matrix.

APPEND VALUE #(
  client = sy-mandt
  severity = 'MEDIUM'
  message_type = 'URGENT'
  display_mode = 'BOTH'
  active = abap_true
  is_default = abap_false
  requires_ack = abap_true
  sort_order = '11'
  description = 'Medium urgent with dual display'
  changed_by = sy-uname
  changed_at = lv_timestamp
) TO lt_matrix.

" MEDIUM + WARNING
APPEND VALUE #(
  client = sy-mandt
  severity = 'MEDIUM'
  message_type = 'WARNING'
  display_mode = 'BANNER'
  active = abap_true
  is_default = abap_true
  requires_ack = abap_false
  sort_order = '12'
  description = 'Medium warnings default to banner'
  changed_by = sy-uname
  changed_at = lv_timestamp
) TO lt_matrix.

APPEND VALUE #(
  client = sy-mandt
  severity = 'MEDIUM'
  message_type = 'WARNING'
  display_mode = 'TOAST'
  active = abap_true
  is_default = abap_false
  requires_ack = abap_false
  sort_order = '13'
  description = 'Medium warnings can use toast if less critical'
  changed_by = sy-uname
  changed_at = lv_timestamp
) TO lt_matrix.

APPEND VALUE #(
  client = sy-mandt
  severity = 'MEDIUM'
  message_type = 'WARNING'
  display_mode = 'BOTH'
  active = abap_true
  is_default = abap_false
  requires_ack = abap_false
  sort_order = '14'
  description = 'Medium warnings with dual display'
  changed_by = sy-uname
  changed_at = lv_timestamp
) TO lt_matrix.

" MEDIUM + MAINT
APPEND VALUE #(
  client = sy-mandt
  severity = 'MEDIUM'
  message_type = 'MAINT'
  display_mode = 'BANNER'
  active = abap_true
  is_default = abap_true
  requires_ack = abap_false
  sort_order = '15'
  description = 'Planned maintenance banner display'
  changed_by = sy-uname
  changed_at = lv_timestamp
) TO lt_matrix.

APPEND VALUE #(
  client = sy-mandt
  severity = 'MEDIUM'
  message_type = 'MAINT'
  display_mode = 'TOAST'
  active = abap_true
  is_default = abap_false
  requires_ack = abap_false
  sort_order = '16'
  description = 'Minor maintenance as toast'
  changed_by = sy-uname
  changed_at = lv_timestamp
) TO lt_matrix.

APPEND VALUE #(
  client = sy-mandt
  severity = 'MEDIUM'
  message_type = 'MAINT'
  display_mode = 'BOTH'
  active = abap_true
  is_default = abap_false
  requires_ack = abap_false
  sort_order = '17'
  description = 'Maintenance with dual display'
  changed_by = sy-uname
  changed_at = lv_timestamp
) TO lt_matrix.

APPEND VALUE #(
  client = sy-mandt
  severity = 'MEDIUM'
  message_type = 'MAINT'
  display_mode = 'SILENT'
  active = abap_true
  is_default = abap_false
  requires_ack = abap_false
  sort_order = '18'
  description = 'Maintenance for audit purposes'
  changed_by = sy-uname
  changed_at = lv_timestamp
) TO lt_matrix.

" MEDIUM + INFO
APPEND VALUE #(
  client = sy-mandt
  severity = 'MEDIUM'
  message_type = 'INFO'
  display_mode = 'BANNER'
  active = abap_true
  is_default = abap_false
  requires_ack = abap_false
  sort_order = '19'
  description = 'Medium info can use banner for visibility'
  changed_by = sy-uname
  changed_at = lv_timestamp
) TO lt_matrix.

APPEND VALUE #(
  client = sy-mandt
  severity = 'MEDIUM'
  message_type = 'INFO'
  display_mode = 'TOAST'
  active = abap_true
  is_default = abap_true
  requires_ack = abap_false
  sort_order = '20'
  description = 'Medium info default to toast'
  changed_by = sy-uname
  changed_at = lv_timestamp
) TO lt_matrix.

APPEND VALUE #(
  client = sy-mandt
  severity = 'MEDIUM'
  message_type = 'INFO'
  display_mode = 'BOTH'
  active = abap_true
  is_default = abap_false
  requires_ack = abap_false
  sort_order = '21'
  description = 'Medium info with dual display'
  changed_by = sy-uname
  changed_at = lv_timestamp
) TO lt_matrix.

APPEND VALUE #(
  client = sy-mandt
  severity = 'MEDIUM'
  message_type = 'INFO'
  display_mode = 'SILENT'
  active = abap_true
  is_default = abap_false
  requires_ack = abap_false
  sort_order = '22'
  description = 'Medium info for tracking only'
  changed_by = sy-uname
  changed_at = lv_timestamp
) TO lt_matrix.

" MEDIUM + TIP
APPEND VALUE #(
  client = sy-mandt
  severity = 'MEDIUM'
  message_type = 'TIP'
  display_mode = 'BANNER'
  active = abap_true
  is_default = abap_false
  requires_ack = abap_false
  sort_order = '23'
  description = 'Medium tips can use banner if important'
  changed_by = sy-uname
  changed_at = lv_timestamp
) TO lt_matrix.

APPEND VALUE #(
  client = sy-mandt
  severity = 'MEDIUM'
  message_type = 'TIP'
  display_mode = 'TOAST'
  active = abap_true
  is_default = abap_true
  requires_ack = abap_false
  sort_order = '24'
  description = 'Medium tips default to toast'
  changed_by = sy-uname
  changed_at = lv_timestamp
) TO lt_matrix.

APPEND VALUE #(
  client = sy-mandt
  severity = 'MEDIUM'
  message_type = 'TIP'
  display_mode = 'SILENT'
  active = abap_true
  is_default = abap_false
  requires_ack = abap_false
  sort_order = '25'
  description = 'Medium tips for reference only'
  changed_by = sy-uname
  changed_at = lv_timestamp
) TO lt_matrix.

*----------------------------------------------------------------------*
* LOW Severity Combinations
*----------------------------------------------------------------------*

" LOW + WARNING
APPEND VALUE #(
  client = sy-mandt
  severity = 'LOW'
  message_type = 'WARNING'
  display_mode = 'TOAST'
  active = abap_true
  is_default = abap_true
  requires_ack = abap_false
  sort_order = '26'
  description = 'Low warnings as non-intrusive toast'
  changed_by = sy-uname
  changed_at = lv_timestamp
) TO lt_matrix.

APPEND VALUE #(
  client = sy-mandt
  severity = 'LOW'
  message_type = 'WARNING'
  display_mode = 'SILENT'
  active = abap_true
  is_default = abap_false
  requires_ack = abap_false
  sort_order = '27'
  description = 'Low warnings for audit trail'
  changed_by = sy-uname
  changed_at = lv_timestamp
) TO lt_matrix.

" LOW + MAINT
APPEND VALUE #(
  client = sy-mandt
  severity = 'LOW'
  message_type = 'MAINT'
  display_mode = 'TOAST'
  active = abap_true
  is_default = abap_true
  requires_ack = abap_false
  sort_order = '28'
  description = 'Low maintenance announcements as toast'
  changed_by = sy-uname
  changed_at = lv_timestamp
) TO lt_matrix.

APPEND VALUE #(
  client = sy-mandt
  severity = 'LOW'
  message_type = 'MAINT'
  display_mode = 'SILENT'
  active = abap_true
  is_default = abap_false
  requires_ack = abap_false
  sort_order = '29'
  description = 'Low maintenance for logging'
  changed_by = sy-uname
  changed_at = lv_timestamp
) TO lt_matrix.

" LOW + INFO
APPEND VALUE #(
  client = sy-mandt
  severity = 'LOW'
  message_type = 'INFO'
  display_mode = 'TOAST'
  active = abap_true
  is_default = abap_true
  requires_ack = abap_false
  sort_order = '30'
  description = 'Low information as toast notification'
  changed_by = sy-uname
  changed_at = lv_timestamp
) TO lt_matrix.

APPEND VALUE #(
  client = sy-mandt
  severity = 'LOW'
  message_type = 'INFO'
  display_mode = 'SILENT'
  active = abap_true
  is_default = abap_false
  requires_ack = abap_false
  sort_order = '31'
  description = 'Low information for tracking purposes'
  changed_by = sy-uname
  changed_at = lv_timestamp
) TO lt_matrix.

" LOW + TIP
APPEND VALUE #(
  client = sy-mandt
  severity = 'LOW'
  message_type = 'TIP'
  display_mode = 'TOAST'
  active = abap_true
  is_default = abap_true
  requires_ack = abap_false
  sort_order = '32'
  description = 'Tips displayed as toast'
  changed_by = sy-uname
  changed_at = lv_timestamp
) TO lt_matrix.

APPEND VALUE #(
  client = sy-mandt
  severity = 'LOW'
  message_type = 'TIP'
  display_mode = 'SILENT'
  active = abap_true
  is_default = abap_false
  requires_ack = abap_false
  sort_order = '33'
  description = 'Tips logged for reference'
  changed_by = sy-uname
  changed_at = lv_timestamp
) TO lt_matrix.

*----------------------------------------------------------------------*
* Insert data into table
*----------------------------------------------------------------------*

" Clear existing data first (optional - remove if you want to preserve customizing)
" DELETE FROM znotif_matrix WHERE client = sy-mandt.

INSERT znotif_matrix FROM TABLE lt_matrix.

IF sy-subrc = 0.
  COMMIT WORK.
  lv_count = lines( lt_matrix ).
  WRITE: / 'Success! Loaded', lv_count, 'matrix entries into ZNOTIF_MATRIX'.
  WRITE: / 'Total valid combinations: 33'.
  WRITE: / 'Default combinations: 15 (one per severity+message_type pair)'.
  WRITE: / 'Requires acknowledgment: 8 combinations'.
  WRITE: / ''.
  WRITE: / 'Next steps:'.
  WRITE: / '1. Verify entries via SM30 → ZNOTIF_MATRIX'.
  WRITE: / '2. Run migration report Z_MIGRATE_NOTIF_MATRIX to fix existing data'.
  WRITE: / '3. Test validation in admin UI'.
ELSE.
  ROLLBACK WORK.
  WRITE: / 'Error! Failed to load matrix entries. Check if table ZNOTIF_MATRIX exists.'.
  WRITE: / 'SQL Return Code:', sy-subrc.
ENDIF.
