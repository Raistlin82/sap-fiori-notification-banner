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
             created_by   TYPE syuname,
             created_at   TYPE timestampl,
             changed_by   TYPE syuname,
             changed_at   TYPE timestampl,
           END OF ty_notification.

    TYPES: tt_notifications TYPE STANDARD TABLE OF ty_notification WITH DEFAULT KEY.

    CLASS-METHODS: get_active_notifications
                     IMPORTING
                       iv_user_id TYPE sy-uname OPTIONAL
                     RETURNING
                       VALUE(rt_notifications) TYPE tt_notifications,

                   create_notification
                     IMPORTING
                       is_notification TYPE ty_notification
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
                       VALUE(rv_authorized) TYPE abap_bool.

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

  ENDMETHOD.

  METHOD create_notification.

    DATA: ls_notification TYPE ztnotify_msgs,
          lv_timestamp TYPE timestampl.

    rv_success = abap_false.

    " Check authorization
    IF check_user_authorization( ) = abap_false.
      RETURN.
    ENDIF.

    " Generate unique message ID
    ls_notification-message_id = generate_message_id( ).
    ls_notification-message_type = is_notification-message_type.
    ls_notification-severity = is_notification-severity.
    ls_notification-title = is_notification-title.
    ls_notification-message_text = is_notification-message_text.
    ls_notification-start_date = is_notification-start_date.
    ls_notification-end_date = is_notification-end_date.
    ls_notification-target_users = is_notification-target_users.
    ls_notification-active = 'X'.
    ls_notification-display_mode = is_notification-display_mode.
    IF ls_notification-display_mode IS INITIAL.
      ls_notification-display_mode = 'BANNER'.  " Default to BANNER
    ENDIF.
    ls_notification-created_by = sy-uname.

    GET TIME STAMP FIELD lv_timestamp.
    ls_notification-created_at = lv_timestamp.
    ls_notification-changed_by = sy-uname.
    ls_notification-changed_at = lv_timestamp.

    INSERT ztnotify_msgs FROM ls_notification.

    IF sy-subrc = 0.
      COMMIT WORK.
      rv_success = abap_true.
    ELSE.
      ROLLBACK WORK.
    ENDIF.

  ENDMETHOD.

  METHOD update_notification.

    DATA: ls_notification TYPE ztnotify_msgs,
          lv_timestamp TYPE timestampl.

    rv_success = abap_false.

    " Check authorization
    IF check_user_authorization( ) = abap_false.
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
    ls_notification-display_mode = is_notification-display_mode.
    IF ls_notification-display_mode IS INITIAL.
      ls_notification-display_mode = 'BANNER'.  " Default to BANNER
    ENDIF.
    ls_notification-target_users = is_notification-target_users.
    ls_notification-changed_by = sy-uname.

    GET TIME STAMP FIELD lv_timestamp.
    ls_notification-changed_at = lv_timestamp.

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

    DATA: lv_has_sap_all TYPE abap_bool.

    " Check if user has authorization to manage notifications
    " Strategy:
    " 1. If user has SAP_ALL → authorized (bypass Z_NOTIFY check)
    " 2. Otherwise, check Z_NOTIFY authorization object (if exists)
    " 3. If Z_NOTIFY exists and user has it → authorized
    " 4. If Z_NOTIFY doesn't exist → deny access (must have SAP_ALL)

    rv_authorized = abap_false.

    " FIRST: Check SAP_ALL role (basis admins have full access)
    SELECT SINGLE @abap_true
      FROM agr_users
      INTO @lv_has_sap_all
      WHERE uname = @sy-uname
        AND agr_name = 'SAP_ALL'.

    IF sy-subrc = 0.
      " User has SAP_ALL → authorized without checking Z_NOTIFY
      rv_authorized = abap_true.
      RETURN.
    ENDIF.

    " SECOND: User doesn't have SAP_ALL → check Z_NOTIFY authorization
    AUTHORITY-CHECK OBJECT 'Z_NOTIFY'
             ID 'ACTVT' FIELD '02'. " Change

    IF sy-subrc = 0.
      " User has Z_NOTIFY authorization
      rv_authorized = abap_true.
    ELSEIF sy-subrc = 12.
      " Z_NOTIFY object doesn't exist and user has no SAP_ALL → deny
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
    "*& - ADMIN: SAP_ALL (exact match)
    "*& - DEVELOPER: SAP_BR_DEVELOPER (exact match)
    "*&---------------------------------------------------------------------*
    DATA: lv_has_role TYPE abap_bool.

    rv_authorized = abap_false.

    CASE iv_target_users.

      WHEN 'ALL'.
        " Public notification - visible to all users
        rv_authorized = abap_true.

      WHEN 'ADMIN'.
        " Administrators (users with SAP_ALL role - exact match)
        SELECT SINGLE @abap_true
          FROM agr_users
          INTO @lv_has_role
          WHERE uname = @sy-uname
            AND agr_name = 'SAP_ALL'.
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

ENDCLASS.
