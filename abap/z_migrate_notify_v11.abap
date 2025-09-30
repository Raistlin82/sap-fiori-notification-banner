*&---------------------------------------------------------------------*
*& Report Z_MIGRATE_NOTIFY_V11
*&---------------------------------------------------------------------*
*& Migration script for ZTNOTIFY_MSGS table v1.0 -> v1.1
*& Adds DISPLAY_MODE field with default value 'BANNER'
*&
*& INSTRUCTIONS:
*& 1. First, adjust table structure in SE11:
*&    - Add DISPLAY_MODE field (CHAR10) after ACTIVE field
*& 2. Execute this report in SE38 to update existing records
*& 3. Verify results in SE16: SELECT * FROM ZTNOTIFY_MSGS
*&---------------------------------------------------------------------*

REPORT z_migrate_notify_v11.

TABLES: ztnotify_msgs.

DATA: lt_notifications TYPE TABLE OF ztnotify_msgs,
      ls_notification  TYPE ztnotify_msgs,
      lv_count         TYPE i,
      lv_updated       TYPE i.

START-OF-SELECTION.

  WRITE: / '========================================'.
  WRITE: / 'Migration: ZTNOTIFY_MSGS v1.0 -> v1.1'.
  WRITE: / '========================================'.
  WRITE: /.

  " Check if DISPLAY_MODE field exists
  TRY.
      SELECT SINGLE * FROM ztnotify_msgs INTO @DATA(ls_test).
      IF sy-subrc = 0.
        " Field exists, proceed
        WRITE: / '✓ DISPLAY_MODE field detected in table'.
      ENDIF.
    CATCH cx_sy_dynamic_osql_error INTO DATA(lx_error).
      WRITE: / '✗ ERROR: DISPLAY_MODE field not found in table!'.
      WRITE: / '  Please add the field in SE11 first:'.
      WRITE: / '  Field: DISPLAY_MODE, Type: CHAR, Length: 10'.
      WRITE: /.
      STOP.
  ENDTRY.

  " Read all existing notifications
  SELECT * FROM ztnotify_msgs INTO TABLE @lt_notifications.

  lv_count = lines( lt_notifications ).

  IF lv_count = 0.
    WRITE: / 'No notifications found in table. Migration not needed.'.
    WRITE: /.
    STOP.
  ENDIF.

  WRITE: / 'Found', lv_count, 'notification(s) to migrate'.
  WRITE: /.

  " Update each record
  LOOP AT lt_notifications INTO ls_notification.

    " Set default display mode if empty
    IF ls_notification-display_mode IS INITIAL.
      ls_notification-display_mode = 'BANNER'.

      " Update changed timestamp and user
      ls_notification-changed_by = sy-uname.
      GET TIME STAMP FIELD ls_notification-changed_at.

      " Update record
      UPDATE ztnotify_msgs FROM ls_notification.

      IF sy-subrc = 0.
        lv_updated = lv_updated + 1.
        WRITE: / '✓ Updated:', ls_notification-message_id+12(4), '-',
                   ls_notification-title(40), '-> BANNER'.
      ELSE.
        WRITE: / '✗ Failed:', ls_notification-message_id+12(4), '-',
                   ls_notification-title(40).
      ENDIF.
    ELSE.
      WRITE: / '- Skipped:', ls_notification-message_id+12(4), '-',
                 ls_notification-title(40), '(already has', ls_notification-display_mode, ')'.
    ENDIF.

  ENDLOOP.

  " Commit changes
  COMMIT WORK.

  " Summary
  WRITE: /.
  WRITE: / '========================================'.
  WRITE: / 'Migration Summary'.
  WRITE: / '========================================'.
  WRITE: / 'Total records processed:', lv_count.
  WRITE: / 'Records updated:        ', lv_updated.
  WRITE: / 'Records skipped:        ', lv_count - lv_updated.
  WRITE: /.

  IF lv_updated > 0.
    WRITE: / '✓ Migration completed successfully!'.
    WRITE: / '  All notifications now have DISPLAY_MODE = BANNER'.
  ELSE.
    WRITE: / '- No updates needed. All records already migrated.'.
  ENDIF.

  WRITE: /.
  WRITE: / 'Next steps:'.
  WRITE: / '1. Verify data: Transaction SE16 -> Table ZTNOTIFY_MSGS'.
  WRITE: / '2. Deploy v1.1.0 frontend code with display mode support'.
  WRITE: / '3. Test new display modes (BANNER, TOAST, BOTH, SILENT)'.
  WRITE: /.

END-OF-SELECTION.