CLASS zcl_notification_manager DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES: BEGIN OF ty_notification,
             message_id   TYPE string,
             message_type TYPE string,
             severity     TYPE string,
             title        TYPE string,
             message_text TYPE string,
             start_date   TYPE dats,
             end_date     TYPE dats,
             target_users TYPE string,
             active       TYPE char1,
             display_mode TYPE char10,
             created_by   TYPE syuname,
             created_at   TYPE timestampl,
             changed_by   TYPE syuname,
             changed_at   TYPE timestampl,
           END OF ty_notification.

    TYPES: tt_notifications TYPE TABLE OF ty_notification.

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
                       iv_message_id TYPE string
                       is_notification TYPE ty_notification
                     RETURNING
                       VALUE(rv_success) TYPE abap_bool,

                   deactivate_notification
                     IMPORTING
                       iv_message_id TYPE string
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
                       VALUE(rv_message_id) TYPE string,

                   check_target_audience
                     IMPORTING
                       iv_target_users TYPE string
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
        AND start_date <= sy-datum
        AND end_date >= sy-datum.

    " Filter by target audience (role-based authorization)
    LOOP AT lt_all_notifications INTO DATA(ls_notif).
      IF check_target_audience( ls_notif-target_users ) = abap_true.
        APPEND ls_notif TO rt_notifications.
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
          changed_by = sy-uname,
          changed_at = lv_timestamp
      WHERE message_id = iv_message_id.

    IF sy-subrc = 0.
      COMMIT WORK.
      rv_success = abap_true.
    ELSE.
      ROLLBACK WORK.
    ENDIF.

  ENDMETHOD.

  METHOD check_user_authorization.

    " Check if user has authorization to manage notifications
    " Replace with your authorization object
    AUTHORITY-CHECK OBJECT 'Z_NOTIFY'
             ID 'ACTVT' FIELD '02'. " Change

    IF sy-subrc = 0.
      rv_authorized = abap_true.
    ELSE.
      rv_authorized = abap_false.
    ENDIF.

  ENDMETHOD.

  METHOD generate_message_id.

    DATA: lv_guid TYPE sysuuid_c32.

    CALL FUNCTION 'GUID_CREATE'
      IMPORTING
        ev_guid_32 = lv_guid.

    rv_message_id = |MSG_{ lv_guid }|.

  ENDMETHOD.

  METHOD check_target_audience.
    "*&---------------------------------------------------------------------*
    "*& Check if current user is authorized to see notification
    "*& based on TARGET_USERS domain fixed value
    "*&---------------------------------------------------------------------*
    DATA: lv_has_role TYPE abap_bool.

    rv_authorized = abap_false.

    CASE iv_target_users.

      WHEN 'ALL'.
        " Public notification - visible to all users
        rv_authorized = abap_true.

      WHEN 'AUTH'.
        " Any authenticated user (not anonymous)
        IF sy-uname IS NOT INITIAL AND sy-uname <> 'ANONYMOUS'.
          rv_authorized = abap_true.
        ENDIF.

      WHEN 'ADMIN'.
        " Administrators (users with SAP_ALL or Z_ADMIN role)
        SELECT SINGLE @abap_true
          FROM agr_users
          INTO @lv_has_role
          WHERE uname = @sy-uname
            AND ( agr_name = 'SAP_ALL' OR agr_name LIKE 'Z_ADMIN%' ).
        IF sy-subrc = 0.
          rv_authorized = abap_true.
        ENDIF.

      WHEN 'DEVELOPER'.
        " Developers (users with SAP_DEV or Z_DEVELOPER role)
        SELECT SINGLE @abap_true
          FROM agr_users
          INTO @lv_has_role
          WHERE uname = @sy-uname
            AND ( agr_name LIKE 'SAP_DEV%' OR agr_name LIKE 'Z_DEVELOPER%' ).
        IF sy-subrc = 0.
          rv_authorized = abap_true.
        ENDIF.

      WHEN 'FINANCE'.
        " Finance users (users with Z_FINANCE role)
        SELECT SINGLE @abap_true
          FROM agr_users
          INTO @lv_has_role
          WHERE uname = @sy-uname
            AND agr_name LIKE 'Z_FINANCE%'.
        IF sy-subrc = 0.
          rv_authorized = abap_true.
        ENDIF.

      WHEN 'SALES'.
        " Sales users (users with Z_SALES role)
        SELECT SINGLE @abap_true
          FROM agr_users
          INTO @lv_has_role
          WHERE uname = @sy-uname
            AND agr_name LIKE 'Z_SALES%'.
        IF sy-subrc = 0.
          rv_authorized = abap_true.
        ENDIF.

      WHEN 'IT'.
        " IT department users (users with Z_IT role)
        SELECT SINGLE @abap_true
          FROM agr_users
          INTO @lv_has_role
          WHERE uname = @sy-uname
            AND agr_name LIKE 'Z_IT%'.
        IF sy-subrc = 0.
          rv_authorized = abap_true.
        ENDIF.

      WHEN 'MANAGER'.
        " Managers (users with Z_MANAGER role)
        SELECT SINGLE @abap_true
          FROM agr_users
          INTO @lv_has_role
          WHERE uname = @sy-uname
            AND agr_name LIKE 'Z_MANAGER%'.
        IF sy-subrc = 0.
          rv_authorized = abap_true.
        ENDIF.

      WHEN OTHERS.
        " Unknown target audience - deny access for security
        rv_authorized = abap_false.

    ENDCASE.

  ENDMETHOD.

ENDCLASS.