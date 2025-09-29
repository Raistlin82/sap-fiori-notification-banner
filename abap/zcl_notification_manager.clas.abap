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
                       VALUE(rv_message_id) TYPE string.

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
           active
      FROM ztnotify_msgs
      INTO CORRESPONDING FIELDS OF TABLE rt_notifications
      WHERE active = 'X'
        AND start_date <= sy-datum
        AND end_date >= sy-datum
        AND ( target_users = 'ALL'
           OR target_users = 'USER' ).

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

ENDCLASS.