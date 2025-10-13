CLASS zcl_notification_manager DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES: BEGIN OF ty_notification,
             message_id   TYPE char32,
             message_type TYPE char12,
             severity     TYPE char8,
             title        TYPE char255,
             message_text TYPE char255,
             start_date   TYPE dats,
             end_date     TYPE dats,
             target_users TYPE char10,
             active       TYPE char1,
             display_mode TYPE char10,
             requires_ack TYPE char1,
             created_by   TYPE syuname,
             created_at   TYPE timestampl,
             changed_by   TYPE syuname,
             changed_at   TYPE timestampl,
           END OF ty_notification.

    TYPES: tt_notifications TYPE STANDARD TABLE OF ty_notification WITH DEFAULT KEY.

    " Acknowledgment tracking types (v1.3.0)
    TYPES: BEGIN OF ty_acknowledgment,
             message_id    TYPE char32,
             userid        TYPE syuname,
             ack_timestamp TYPE timestampl,
             client_info   TYPE char255,
           END OF ty_acknowledgment.

    TYPES: tt_acknowledgments TYPE STANDARD TABLE OF ty_acknowledgment WITH DEFAULT KEY.

    CLASS-METHODS: get_active_notifications
                     IMPORTING
                       iv_user_id TYPE sy-uname OPTIONAL
                     RETURNING
                       VALUE(rt_notifications) TYPE tt_notifications,

                   create_notification
                     IMPORTING
                       is_notification TYPE ty_notification
                     EXPORTING
                       ev_message_id TYPE char32
                     RETURNING
                       VALUE(rv_success) TYPE abap_bool,

                   create_recurring_notifications
                     IMPORTING
                       is_notification TYPE ty_notification
                       iv_recurrence_type TYPE char1
                       iv_occurrences TYPE i
                     EXPORTING
                       et_message_ids TYPE string_table
                     RETURNING
                       VALUE(rv_success) TYPE abap_bool,

                   update_notification
                     IMPORTING
                       iv_message_id TYPE char32
                       is_notification TYPE ty_notification
                     RETURNING
                       VALUE(rv_success) TYPE abap_bool,

                   deactivate_notification
                     IMPORTING
                       iv_message_id TYPE char32
                     RETURNING
                       VALUE(rv_success) TYPE abap_bool,

                   check_user_authorization
                     IMPORTING
                       iv_user_id TYPE sy-uname OPTIONAL
                     RETURNING
                       VALUE(rv_authorized) TYPE abap_bool,

                   validate_notif_combination
                     IMPORTING
                       iv_severity TYPE char8
                       iv_message_type TYPE char12
                       iv_display_mode TYPE char10
                     EXPORTING
                       ev_valid TYPE abap_bool
                       ev_requires_ack TYPE abap_bool
                       ev_error_message TYPE string
                     RETURNING
                       VALUE(rv_success) TYPE abap_bool,

                   get_valid_display_modes
                     IMPORTING
                       iv_severity TYPE char8
                       iv_message_type TYPE char12
                     EXPORTING
                       et_display_modes TYPE string_table
                       ev_default_mode TYPE char10
                     RETURNING
                       VALUE(rv_success) TYPE abap_bool,

                   get_valid_message_types
                     IMPORTING
                       iv_severity TYPE char8
                     EXPORTING
                       et_message_types TYPE string_table
                       ev_default_type TYPE char12
                     RETURNING
                       VALUE(rv_success) TYPE abap_bool,

                   " Acknowledgment tracking methods (v1.3.0)
                   has_user_acknowledged
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

  PRIVATE SECTION.
    CLASS-METHODS: generate_message_id
                     RETURNING
                       VALUE(rv_message_id) TYPE char32,

                   check_target_audience
                     IMPORTING
                       iv_target_users TYPE char10
                     RETURNING
                       VALUE(rv_authorized) TYPE abap_bool.

ENDCLASS.

CLASS zcl_notification_manager IMPLEMENTATION.

  METHOD get_active_notifications.

    DATA: lv_user_id TYPE sy-uname.

    lv_user_id = COND #( WHEN iv_user_id IS NOT INITIAL
                        THEN iv_user_id
                        ELSE sy-uname ).

    SELECT message_id,
           message_type,
           severity,
           title,
           message_text,
           start_date,
           end_date,
           target_users,
           active,
           display_mode,
           requires_ack,
           created_by,
           created_at,
           changed_by,
           changed_at
      FROM ztnotify_msgs
      INTO TABLE @DATA(lt_all_notifications)
      WHERE active = 'X'
        AND start_date <= @sy-datum
        AND end_date >= @sy-datum.

    " Filter by target audience (role-based authorization)
    LOOP AT lt_all_notifications INTO DATA(ls_notification).
      IF check_target_audience( ls_notification-target_users ) = abap_true.
        APPEND ls_notification TO rt_notifications.
      ENDIF.
    ENDLOOP.

    " Filter out acknowledged notifications for this user (v1.3.0)
    LOOP AT rt_notifications ASSIGNING FIELD-SYMBOL(<notif>).
      IF <notif>-requires_ack = 'X'.
        IF has_user_acknowledged( iv_message_id = <notif>-message_id
                                 iv_user_id = lv_user_id ) = abap_true.
          DELETE rt_notifications.
        ENDIF.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD create_notification.

    DATA: ls_notification TYPE ztnotify_msgs,
          lv_timestamp TYPE timestampl.

    rv_success = abap_false.
    CLEAR ev_message_id.

    " Check authorization
    IF check_user_authorization( ) = abap_false.
      RETURN.
    ENDIF.

    " Apply default display mode if not provided
    DATA: lv_display_mode TYPE char10.
    lv_display_mode = is_notification-display_mode.
    IF lv_display_mode IS INITIAL.
      lv_display_mode = 'BANNER'.  " Default to BANNER
    ENDIF.

    " Validate notification combination against matrix
    DATA: lv_valid TYPE abap_bool,
          lv_requires_ack TYPE abap_bool,
          lv_error_message TYPE string,
          lv_validation_success TYPE abap_bool.

    lv_validation_success = validate_notif_combination(
      EXPORTING
        iv_severity = is_notification-severity
        iv_message_type = is_notification-message_type
        iv_display_mode = lv_display_mode
      IMPORTING
        ev_valid = lv_valid
        ev_requires_ack = lv_requires_ack
        ev_error_message = lv_error_message
    ).

    IF lv_validation_success = abap_false OR lv_valid = abap_false.
      " Invalid combination - reject creation
      RETURN.
    ENDIF.

    " Generate unique message ID
    ls_notification-message_id = generate_message_id( ).
    ev_message_id = ls_notification-message_id.  " Return the generated ID

    ls_notification-message_type = is_notification-message_type.
    ls_notification-severity = is_notification-severity.
    ls_notification-title = is_notification-title.
    ls_notification-message_text = is_notification-message_text.
    ls_notification-start_date = is_notification-start_date.
    ls_notification-end_date = is_notification-end_date.
    ls_notification-target_users = is_notification-target_users.
    ls_notification-active = 'X'.
    ls_notification-display_mode = lv_display_mode.
    ls_notification-created_by = sy-uname.

    GET TIME STAMP FIELD lv_timestamp.
    ls_notification-created_at = lv_timestamp.
    ls_notification-changed_by = sy-uname.
    ls_notification-changed_at = lv_timestamp.

    " Set requires_ack flag from matrix validation
    IF lv_requires_ack = abap_true.
      ls_notification-requires_ack = 'X'.
    ELSE.
      CLEAR ls_notification-requires_ack.
    ENDIF.

    INSERT ztnotify_msgs FROM ls_notification.

    IF sy-subrc = 0.
      COMMIT WORK.
      rv_success = abap_true.
    ELSE.
      ROLLBACK WORK.
    ENDIF.

  ENDMETHOD.

  METHOD create_recurring_notifications.
    "*&---------------------------------------------------------------------*
    "*& Create multiple recurring notifications based on pattern
    "*& Input: Notification template, recurrence type (D/W/M), occurrences
    "*& Output: List of created message IDs, success flag
    "*& Logic: Calculate dates based on recurrence and create N notifications
    "*&---------------------------------------------------------------------*
    DATA: ls_new_notification TYPE ty_notification,
          lv_duration_days TYPE i,
          lv_counter TYPE i,
          lv_offset_days TYPE i,
          lv_message_id TYPE char32,
          lv_success TYPE abap_bool.

    rv_success = abap_false.
    CLEAR et_message_ids.

    " Validation: Check minimum inputs
    IF iv_occurrences < 1 OR iv_occurrences > 365.
      " Limit to max 365 occurrences to prevent accidental mass creation
      RETURN.
    ENDIF.

    IF iv_recurrence_type <> 'D' AND
       iv_recurrence_type <> 'W' AND
       iv_recurrence_type <> 'M'.
      " Invalid recurrence type
      RETURN.
    ENDIF.

    " Calculate duration between start and end date
    lv_duration_days = is_notification-end_date - is_notification-start_date.

    " Create N notifications based on occurrences
    DO iv_occurrences TIMES.
      lv_counter = sy-index.

      " Calculate offset based on recurrence type
      CASE iv_recurrence_type.
        WHEN 'D'.  " Daily
          lv_offset_days = ( lv_counter - 1 ) * 1.
        WHEN 'W'.  " Weekly
          lv_offset_days = ( lv_counter - 1 ) * 7.
        WHEN 'M'.  " Monthly (approximate 30 days)
          lv_offset_days = ( lv_counter - 1 ) * 30.
      ENDCASE.

      " Create new notification with calculated dates
      ls_new_notification = is_notification.
      ls_new_notification-start_date = is_notification-start_date + lv_offset_days.
      ls_new_notification-end_date = ls_new_notification-start_date + lv_duration_days.

      " Create the notification
      lv_success = create_notification(
        EXPORTING
          is_notification = ls_new_notification
        IMPORTING
          ev_message_id = lv_message_id ).

      IF lv_success = abap_true.
        " Append created message ID to result list
        APPEND lv_message_id TO et_message_ids.
      ELSE.
        " If any creation fails, rollback all and return failure
        ROLLBACK WORK.
        CLEAR et_message_ids.
        RETURN.
      ENDIF.
    ENDDO.

    " All notifications created successfully
    COMMIT WORK.
    rv_success = abap_true.

  ENDMETHOD.

  METHOD update_notification.

    DATA: ls_notification TYPE ztnotify_msgs,
          lv_timestamp TYPE timestampl.

    rv_success = abap_false.

    " Check authorization
    IF check_user_authorization( ) = abap_false.
      RETURN.
    ENDIF.

    " Apply default display mode if not provided
    DATA: lv_display_mode TYPE char10.
    lv_display_mode = is_notification-display_mode.
    IF lv_display_mode IS INITIAL.
      lv_display_mode = 'BANNER'.  " Default to BANNER
    ENDIF.

    " Validate notification combination against matrix
    DATA: lv_valid TYPE abap_bool,
          lv_requires_ack TYPE abap_bool,
          lv_error_message TYPE string,
          lv_validation_success TYPE abap_bool.

    lv_validation_success = validate_notif_combination(
      EXPORTING
        iv_severity = is_notification-severity
        iv_message_type = is_notification-message_type
        iv_display_mode = lv_display_mode
      IMPORTING
        ev_valid = lv_valid
        ev_requires_ack = lv_requires_ack
        ev_error_message = lv_error_message
    ).

    IF lv_validation_success = abap_false OR lv_valid = abap_false.
      " Invalid combination - reject update
      RETURN.
    ENDIF.

    " Get existing record
    SELECT SINGLE *
      FROM ztnotify_msgs
      INTO ls_notification
      WHERE message_id = iv_message_id.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    " Update fields
    ls_notification-message_type = is_notification-message_type.
    ls_notification-severity = is_notification-severity.
    ls_notification-title = is_notification-title.
    ls_notification-message_text = is_notification-message_text.
    ls_notification-start_date = is_notification-start_date.
    ls_notification-end_date = is_notification-end_date.
    ls_notification-display_mode = lv_display_mode.
    ls_notification-target_users = is_notification-target_users.
    ls_notification-active = is_notification-active.
    ls_notification-changed_by = sy-uname.

    GET TIME STAMP FIELD lv_timestamp.
    ls_notification-changed_at = lv_timestamp.

    " Set requires_ack flag from matrix validation
    IF lv_requires_ack = abap_true.
      ls_notification-requires_ack = 'X'.
    ELSE.
      CLEAR ls_notification-requires_ack.
    ENDIF.

    UPDATE ztnotify_msgs FROM ls_notification.

    IF sy-subrc = 0.
      COMMIT WORK.
      rv_success = abap_true.
    ELSE.
      ROLLBACK WORK.
    ENDIF.

  ENDMETHOD.

  METHOD deactivate_notification.

    DATA: lv_timestamp TYPE timestampl.

    rv_success = abap_false.

    " Check authorization
    IF check_user_authorization( ) = abap_false.
      RETURN.
    ENDIF.

    GET TIME STAMP FIELD lv_timestamp.

    UPDATE ztnotify_msgs
      SET active = ' ',
          changed_by = @sy-uname,
          changed_at = @lv_timestamp
      WHERE message_id = @iv_message_id.

    IF sy-subrc = 0.
      COMMIT WORK.
      rv_success = abap_true.
    ELSE.
      ROLLBACK WORK.
    ENDIF.

  ENDMETHOD.

  METHOD check_user_authorization.

    DATA: lv_has_admin_role TYPE abap_bool.

    " Check if user has authorization to manage notifications
    " Strategy:
    " 1. If user has Z_BR_ADMINISTRATOR role → authorized (bypass Z_NOTIFY check)
    " 2. Otherwise, check Z_NOTIFY authorization object (if exists)
    " 3. If Z_NOTIFY exists and user has it → authorized
    " 4. If Z_NOTIFY doesn't exist → deny access (must have Z_BR_ADMINISTRATOR)

    rv_authorized = abap_false.

    " FIRST: Check Z_BR_ADMINISTRATOR role (basis admins have full access)
    SELECT SINGLE @abap_true
      FROM agr_users
      INTO @lv_has_admin_role
      WHERE uname = @sy-uname
        AND agr_name = 'Z_BR_ADMINISTRATOR'.

    IF sy-subrc = 0.
      " User has Z_BR_ADMINISTRATOR → authorized without checking Z_NOTIFY
      rv_authorized = abap_true.
      RETURN.
    ENDIF.

    " SECOND: User doesn't have Z_BR_ADMINISTRATOR → check Z_NOTIFY authorization
    AUTHORITY-CHECK OBJECT 'Z_NOTIFY'
             ID 'ACTVT' FIELD '02'. " Change

    IF sy-subrc = 0.
      " User has Z_NOTIFY authorization
      rv_authorized = abap_true.
    ELSEIF sy-subrc = 12.
      " Z_NOTIFY object doesn't exist and user has no Z_BR_ADMINISTRATOR → deny
      rv_authorized = abap_false.
    ELSE.
      " User doesn't have Z_NOTIFY authorization
      rv_authorized = abap_false.
    ENDIF.

  ENDMETHOD.

  METHOD generate_message_id.
    "*&---------------------------------------------------------------------*
    "*& Generate UUID in CHAR32 format (compatible with SM30)
    "*& Returns: 32-char UUID string (e.g., '550e8400e29b41d4a716446655440000')
    "*&---------------------------------------------------------------------*
    DATA: lv_guid TYPE sysuuid_c32.

    CALL FUNCTION 'GUID_CREATE'
      IMPORTING
        ev_guid_32 = lv_guid.

    rv_message_id = lv_guid.  " Return plain UUID (CHAR32 compatible)

  ENDMETHOD.

  METHOD check_target_audience.
    "*&---------------------------------------------------------------------*
    "*& Check if current user is authorized to see notification
    "*& based on TARGET_USERS domain fixed value (3 values: ALL, ADMIN, DEVELOPER)
    "*&
    "*& IMPORTANT: Uses exact role name matching (no LIKE patterns)
    "*& - ADMIN: Z_BR_ADMINISTRATOR (exact match)
    "*& - DEVELOPER: SAP_BR_DEVELOPER (exact match)
    "*&---------------------------------------------------------------------*
    DATA: lv_has_role TYPE abap_bool.

    rv_authorized = abap_false.

    CASE iv_target_users.

      WHEN 'ALL'.
        " Public notification - visible to all users
        rv_authorized = abap_true.

      WHEN 'ADMIN'.
        " Administrators (users with Z_BR_ADMINISTRATOR role - exact match)
        SELECT SINGLE @abap_true
          FROM agr_users
          INTO @lv_has_role
          WHERE uname = @sy-uname
            AND agr_name = 'Z_BR_ADMINISTRATOR'.
        IF sy-subrc = 0.
          rv_authorized = abap_true.
        ENDIF.

      WHEN 'DEVELOPER'.
        " Developers (users with SAP_BR_DEVELOPER role - exact match)
        SELECT SINGLE @abap_true
          FROM agr_users
          INTO @lv_has_role
          WHERE uname = @sy-uname
            AND agr_name = 'SAP_BR_DEVELOPER'.
        IF sy-subrc = 0.
          rv_authorized = abap_true.
        ENDIF.

      WHEN OTHERS.
        " Unknown target audience - deny access for security
        rv_authorized = abap_false.

    ENDCASE.

  ENDMETHOD.

  METHOD validate_notif_combination.
    " Validates if the combination of severity, message_type, and display_mode
    " is allowed according to the notification matrix customizing table
    DATA: ls_matrix TYPE znotif_matrix,
          lv_timestamp TYPE timestampl.

    " Initialize return values
    rv_success = abap_false.
    ev_valid = abap_false.
    ev_requires_ack = abap_false.
    CLEAR ev_error_message.

    " Input validation
    IF iv_severity IS INITIAL OR iv_message_type IS INITIAL OR iv_display_mode IS INITIAL.
      ev_error_message = 'All parameters (severity, message_type, display_mode) are required'.
      RETURN.
    ENDIF.

    " Check if combination exists and is active in matrix
    SELECT SINGLE *
      FROM znotif_matrix
      INTO @ls_matrix
      WHERE severity = @iv_severity
        AND message_type = @iv_message_type
        AND display_mode = @iv_display_mode
        AND active = @abap_true.

    IF sy-subrc = 0.
      " Valid combination found
      ev_valid = abap_true.
      ev_requires_ack = ls_matrix-requires_ack.
      rv_success = abap_true.
    ELSE.
      " Invalid combination
      ev_valid = abap_false.
      ev_error_message = |Combination { iv_severity }/{ iv_message_type }/{ iv_display_mode } is not allowed by notification matrix|.
      rv_success = abap_true.  " Method executed successfully, but combination is invalid
    ENDIF.

  ENDMETHOD.

  METHOD get_valid_display_modes.
    " Returns list of valid display modes for given severity and message_type
    " Ordered by sort_order, with default mode identified
    DATA: lt_matrix TYPE TABLE OF znotif_matrix,
          ls_matrix TYPE znotif_matrix,
          lv_mode TYPE string.

    " Initialize return values
    rv_success = abap_false.
    CLEAR et_display_modes.
    CLEAR ev_default_mode.

    " Input validation
    IF iv_severity IS INITIAL OR iv_message_type IS INITIAL.
      RETURN.
    ENDIF.

    " Get all active display modes for this combination, ordered by sort_order
    SELECT *
      FROM znotif_matrix
      INTO TABLE @lt_matrix
      WHERE severity = @iv_severity
        AND message_type = @iv_message_type
        AND active = @abap_true
      ORDER BY sort_order ASCENDING.

    IF sy-subrc <> 0.
      " No valid combinations found - return empty list
      rv_success = abap_true.
      RETURN.
    ENDIF.

    " Build list of display modes and identify default
    LOOP AT lt_matrix INTO ls_matrix.
      APPEND ls_matrix-display_mode TO et_display_modes.

      " Check if this is the default mode
      IF ls_matrix-is_default = abap_true.
        ev_default_mode = ls_matrix-display_mode.
      ENDIF.
    ENDLOOP.

    " If no default was marked, use first in list as fallback
    IF ev_default_mode IS INITIAL AND lines( et_display_modes ) > 0.
      ev_default_mode = et_display_modes[ 1 ].
    ENDIF.

    rv_success = abap_true.

  ENDMETHOD.

  METHOD get_valid_message_types.
    " Returns list of valid message types for given severity
    " Ordered by sort_order, with default type identified
    DATA: lt_matrix TYPE TABLE OF znotif_matrix,
          ls_matrix TYPE znotif_matrix,
          lt_types_temp TYPE string_table,
          lv_type TYPE string.

    " Initialize return values
    rv_success = abap_false.
    CLEAR et_message_types.
    CLEAR ev_default_type.

    " Input validation
    IF iv_severity IS INITIAL.
      RETURN.
    ENDIF.

    " Get all active message types for this severity, ordered by sort_order
    " We need to get DISTINCT message types
    SELECT *
      FROM znotif_matrix
      INTO TABLE @lt_matrix
      WHERE severity = @iv_severity
        AND active = @abap_true
      ORDER BY sort_order ASCENDING.

    IF sy-subrc <> 0.
      " No valid combinations found - return empty list
      rv_success = abap_true.
      RETURN.
    ENDIF.

    " Build list of unique message types and identify default
    LOOP AT lt_matrix INTO ls_matrix.
      " Check if message type already in list (ensure uniqueness)
      READ TABLE et_message_types TRANSPORTING NO FIELDS WITH KEY table_line = ls_matrix-message_type.
      IF sy-subrc <> 0.
        " Not in list yet, add it
        APPEND ls_matrix-message_type TO et_message_types.

        " Check if this is marked as default and we don't have one yet
        IF ls_matrix-is_default = abap_true AND ev_default_type IS INITIAL.
          ev_default_type = ls_matrix-message_type.
        ENDIF.
      ENDIF.
    ENDLOOP.

    " If no default was marked, use first in list as fallback
    IF ev_default_type IS INITIAL AND lines( et_message_types ) > 0.
      ev_default_type = et_message_types[ 1 ].
    ENDIF.

    rv_success = abap_true.

  ENDMETHOD.

  METHOD has_user_acknowledged.
    "*&---------------------------------------------------------------------*
    "*& Check if a specific user has already acknowledged a notification
    "*& v1.3.0 - Acknowledgment tracking
    "*&---------------------------------------------------------------------*
    DATA: lv_count TYPE i,
          lv_userid TYPE syuname.

    " Default to current user if not specified
    lv_userid = COND #( WHEN iv_user_id IS NOT INITIAL
                        THEN iv_user_id
                        ELSE sy-uname ).

    " Query acknowledgment log
    SELECT COUNT(*)
      FROM znotify_ack_log
      INTO @lv_count
      WHERE mandt = @sy-mandt
        AND message_id = @iv_message_id
        AND userid = @lv_userid.

    " Return true if acknowledgment record exists
    rv_acknowledged = COND #( WHEN lv_count > 0 THEN abap_true ELSE abap_false ).

  ENDMETHOD.

  METHOD record_acknowledgment.
    "*&---------------------------------------------------------------------*
    "*& Record a user's acknowledgment of a notification
    "*& v1.3.0 - Acknowledgment tracking
    "*&---------------------------------------------------------------------*
    DATA: ls_ack_log TYPE znotify_ack_log,
          lv_userid TYPE syuname.

    " Default to current user if not specified
    lv_userid = COND #( WHEN iv_user_id IS NOT INITIAL
                        THEN iv_user_id
                        ELSE sy-uname ).

    " Check if already acknowledged (prevent duplicates)
    IF has_user_acknowledged( iv_message_id = iv_message_id
                             iv_user_id = lv_userid ) = abap_true.
      " Already acknowledged - return false to indicate duplicate
      rv_success = abap_false.
      RETURN.
    ENDIF.

    " Prepare acknowledgment record
    CLEAR ls_ack_log.
    ls_ack_log-mandt = sy-mandt.
    ls_ack_log-message_id = iv_message_id.
    ls_ack_log-userid = lv_userid.
    GET TIME STAMP FIELD ls_ack_log-ack_timestamp.
    ls_ack_log-client_info = iv_client_info.

    " Insert into database
    INSERT znotify_ack_log FROM ls_ack_log.

    IF sy-subrc = 0.
      " Success - commit immediately
      COMMIT WORK AND WAIT.
      rv_success = abap_true.
    ELSE.
      " Failure - rollback
      ROLLBACK WORK.
      rv_success = abap_false.
    ENDIF.

  ENDMETHOD.

  METHOD get_acknowledgments.
    "*&---------------------------------------------------------------------*
    "*& Get all acknowledgments for a specific notification
    "*& v1.3.0 - Acknowledgment tracking
    "*&---------------------------------------------------------------------*
    DATA: lt_acks TYPE STANDARD TABLE OF znotify_ack_log.

    " Query all acknowledgments for this notification
    SELECT *
      FROM znotify_ack_log
      INTO TABLE @lt_acks
      WHERE mandt = @sy-mandt
        AND message_id = @iv_message_id
      ORDER BY ack_timestamp DESCENDING.

    " Convert to output structure
    LOOP AT lt_acks INTO DATA(ls_ack).
      APPEND VALUE #(
        message_id    = ls_ack-message_id
        userid        = ls_ack-userid
        ack_timestamp = ls_ack-ack_timestamp
        client_info   = ls_ack-client_info
      ) TO rt_acknowledgments.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
