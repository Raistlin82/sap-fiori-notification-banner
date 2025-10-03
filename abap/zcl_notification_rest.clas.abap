CLASS zcl_notification_rest DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES: if_http_extension.

  PRIVATE SECTION.
    DATA: mo_server TYPE REF TO if_http_server.

    METHODS: handle_get_notifications,
             handle_create_notification,
             handle_update_notification,
             handle_delete_notification,
             handle_get_stats,
             handle_get_log,
             serialize_notifications
               IMPORTING
                 it_notifications TYPE zcl_notification_manager=>tt_notifications
               RETURNING
                 VALUE(rv_json) TYPE string,
             deserialize_notification
               IMPORTING
                 iv_json TYPE string
               RETURNING
                 VALUE(rs_notification) TYPE zcl_notification_manager=>ty_notification.

ENDCLASS.

CLASS zcl_notification_rest IMPLEMENTATION.

  METHOD if_http_extension~handle_request.
    DATA: lv_method TYPE string,
          lv_path TYPE string.

    " Store server reference for use in other methods
    mo_server = server.

    " Set CORS headers to allow cross-origin requests from Fiori apps
    server->response->set_header_field( name = 'Access-Control-Allow-Origin' value = '*' ).
    server->response->set_header_field( name = 'Access-Control-Allow-Methods' value = 'GET,POST,PUT,DELETE,OPTIONS' ).
    server->response->set_header_field( name = 'Access-Control-Allow-Headers' value = 'Content-Type,Accept,Authorization,X-Requested-With' ).
    server->response->set_header_field( name = 'Access-Control-Max-Age' value = '3600' ).

    " Get HTTP method and path
    lv_method = server->request->get_header_field( '~request_method' ).
    lv_path = server->request->get_header_field( '~path_info' ).

    " Handle OPTIONS preflight request for CORS
    IF lv_method = 'OPTIONS'.
      server->response->set_status( code = 200 reason = 'OK' ).
      RETURN.
    ENDIF.

    " Route based on HTTP method and path
    CASE lv_method.
      WHEN 'GET'.
        IF lv_path CS '/stats'.
          handle_get_stats( ).
        ELSEIF lv_path CS '/log'.
          handle_get_log( ).
        ELSE.
          handle_get_notifications( ).
        ENDIF.

      WHEN 'POST'.
        handle_create_notification( ).

      WHEN 'PUT'.
        handle_update_notification( ).

      WHEN 'DELETE'.
        handle_delete_notification( ).

      WHEN OTHERS.
        " Method not allowed
        server->response->set_status( code = 405 reason = 'Method Not Allowed' ).
    ENDCASE.

  ENDMETHOD.

  METHOD handle_get_notifications.

    DATA: lt_notifications TYPE zcl_notification_manager=>tt_notifications,
          lv_json TYPE string,
          lv_user_id_str TYPE string,
          lv_user_id TYPE sy-uname,
          lv_today TYPE sy-datum,
          lv_expired_count TYPE i.

    " Get user ID from query parameter
    lv_user_id_str = mo_server->request->get_form_field( 'user_id' ).
    lv_user_id = lv_user_id_str.

    " Auto-expire notifications: Deactivate notifications where end_date has passed
    lv_today = sy-datum.
    UPDATE ztnotify_msgs
      SET active = ''
          changed_by = sy-uname
          changed_at = sy-datum
      WHERE end_date < lv_today
        AND active = 'X'.

    lv_expired_count = sy-dbcnt.

    " Log expired notifications count (optional - for monitoring)
    IF lv_expired_count > 0.
      " Could write to application log here if needed
      " MESSAGE s001(00) WITH 'Auto-expired' lv_expired_count 'notifications'.
    ENDIF.

    " Get active notifications
    lt_notifications = zcl_notification_manager=>get_active_notifications( lv_user_id ).

    " Serialize to JSON
    lv_json = serialize_notifications( lt_notifications ).

    " Set response
    mo_server->response->set_status( code = 200 reason = 'OK' ).
    mo_server->response->set_header_field( name = 'Content-Type' value = 'application/json' ).
    mo_server->response->set_cdata( data = lv_json ).

  ENDMETHOD.

  METHOD handle_create_notification.

    DATA: lv_json TYPE string,
          ls_notification TYPE zcl_notification_manager=>ty_notification,
          lv_success TYPE abap_bool,
          lv_error_msg TYPE string.

    TRY.
        " Get JSON from request body
        lv_json = mo_server->request->get_cdata( ).

        " Deserialize JSON to notification structure
        ls_notification = deserialize_notification( lv_json ).

        " Create notification
        lv_success = zcl_notification_manager=>create_notification( ls_notification ).

        " Set response
        mo_server->response->set_header_field( name = 'Content-Type' value = 'application/json' ).

        IF lv_success = abap_true.
          mo_server->response->set_status( code = 201 reason = 'Created' ).
          mo_server->response->set_cdata( data = '{"success": true, "message": "Notification created"}' ).
        ELSE.
          mo_server->response->set_status( code = 500 reason = 'Internal Server Error' ).
          mo_server->response->set_cdata( data = '{"success": false, "message": "Failed to create notification - check authorization"}' ).
        ENDIF.

      CATCH cx_root INTO DATA(lx_error).
        " Catch any exception and return detailed error message
        lv_error_msg = lx_error->get_text( ).
        mo_server->response->set_header_field( name = 'Content-Type' value = 'application/json' ).
        mo_server->response->set_status( code = 500 reason = 'Internal Server Error' ).
        mo_server->response->set_cdata( data = '{"success": false, "message": "Exception: ' && lv_error_msg && '"}' ).
    ENDTRY.

  ENDMETHOD.

  METHOD handle_update_notification.

    DATA: lv_json TYPE string,
          ls_notification TYPE zcl_notification_manager=>ty_notification,
          lv_message_id_str TYPE string,
          lv_message_id TYPE char32,
          lv_success TYPE abap_bool,
          lv_error_msg TYPE string.

    TRY.
        " Get message ID from query parameter
        lv_message_id_str = mo_server->request->get_form_field( 'message_id' ).
        lv_message_id = lv_message_id_str.

        " Get JSON from request body
        lv_json = mo_server->request->get_cdata( ).

        " Deserialize JSON to notification structure
        ls_notification = deserialize_notification( lv_json ).

        " Update notification
        lv_success = zcl_notification_manager=>update_notification(
          iv_message_id = lv_message_id
          is_notification = ls_notification ).

        " Set response
        mo_server->response->set_header_field( name = 'Content-Type' value = 'application/json' ).

        IF lv_success = abap_true.
          mo_server->response->set_status( code = 200 reason = 'OK' ).
          mo_server->response->set_cdata( data = '{"success": true, "message": "Notification updated"}' ).
        ELSE.
          mo_server->response->set_status( code = 500 reason = 'Internal Server Error' ).
          mo_server->response->set_cdata( data = '{"success": false, "message": "Failed to update notification"}' ).
        ENDIF.

      CATCH cx_root INTO DATA(lx_error).
        " Catch any exception and return detailed error message
        lv_error_msg = lx_error->get_text( ).
        mo_server->response->set_header_field( name = 'Content-Type' value = 'application/json' ).
        mo_server->response->set_status( code = 500 reason = 'Internal Server Error' ).
        mo_server->response->set_cdata( data = '{"success": false, "message": "Exception: ' && lv_error_msg && '"}' ).
    ENDTRY.

  ENDMETHOD.

  METHOD handle_delete_notification.

    DATA: lv_message_id_str TYPE string,
          lv_message_id TYPE char32,
          lv_success TYPE abap_bool,
          lv_error_msg TYPE string.

    TRY.
        " Get message ID from query parameter
        lv_message_id_str = mo_server->request->get_form_field( 'message_id' ).
        lv_message_id = lv_message_id_str.

        " Deactivate notification
        lv_success = zcl_notification_manager=>deactivate_notification( lv_message_id ).

        " Set response
        mo_server->response->set_header_field( name = 'Content-Type' value = 'application/json' ).

        IF lv_success = abap_true.
          mo_server->response->set_status( code = 200 reason = 'OK' ).
          mo_server->response->set_cdata( data = '{"success": true, "message": "Notification deleted"}' ).
        ELSE.
          mo_server->response->set_status( code = 500 reason = 'Internal Server Error' ).
          mo_server->response->set_cdata( data = '{"success": false, "message": "Failed to delete notification"}' ).
        ENDIF.

      CATCH cx_root INTO DATA(lx_error).
        " Catch any exception and return detailed error message
        lv_error_msg = lx_error->get_text( ).
        mo_server->response->set_header_field( name = 'Content-Type' value = 'application/json' ).
        mo_server->response->set_status( code = 500 reason = 'Internal Server Error' ).
        mo_server->response->set_cdata( data = '{"success": false, "message": "Exception: ' && lv_error_msg && '"}' ).
    ENDTRY.

  ENDMETHOD.

  METHOD serialize_notifications.

    rv_json = /ui2/cl_json=>serialize(
      data = it_notifications
      compress = abap_true
      pretty_name = /ui2/cl_json=>pretty_mode-low_case ).

  ENDMETHOD.

  METHOD deserialize_notification.

    /ui2/cl_json=>deserialize(
      EXPORTING
        json = iv_json
        pretty_name = /ui2/cl_json=>pretty_mode-low_case
      CHANGING
        data = rs_notification ).

  ENDMETHOD.

  METHOD handle_get_stats.
    " Provides statistics for the dynamic tile counter
    " Response format: OData format for SAP Fiori dynamic tile

    DATA: lv_high_count   TYPE i,
          lv_medium_count TYPE i,
          lv_low_count    TYPE i,
          lv_total        TYPE i,
          lv_info         TYPE string,
          lv_info_state   TYPE string,
          lv_number       TYPE string,
          lv_json         TYPE string,
          lv_today        TYPE sy-datum.

    lv_today = sy-datum.

    " Count active notifications by severity
    SELECT COUNT(*) FROM ztnotify_msgs
      INTO @lv_high_count
      WHERE active = 'X'
        AND start_date <= @lv_today
        AND end_date >= @lv_today
        AND severity = 'HIGH'.

    SELECT COUNT(*) FROM ztnotify_msgs
      INTO @lv_medium_count
      WHERE active = 'X'
        AND start_date <= @lv_today
        AND end_date >= @lv_today
        AND severity = 'MEDIUM'.

    SELECT COUNT(*) FROM ztnotify_msgs
      INTO @lv_low_count
      WHERE active = 'X'
        AND start_date <= @lv_today
        AND end_date >= @lv_today
        AND severity = 'LOW'.

    lv_total = lv_high_count + lv_medium_count + lv_low_count.

    " Build info string (format: "XH|XM|XL")
    lv_info = lv_high_count && 'H|' && lv_medium_count && 'M|' && lv_low_count && 'L'.

    " Determine info state based on severity (HIGH = Error, MEDIUM = Warning, LOW = Success)
    IF lv_high_count > 0.
      lv_info_state = 'Error'.      " Red
    ELSEIF lv_medium_count > 0.
      lv_info_state = 'Warning'.    " Orange
    ELSE.
      lv_info_state = 'Success'.    " Green
    ENDIF.

    " Convert total to string
    lv_number = lv_total.

    " Build OData-compatible JSON response for dynamic tile
    " Format: { "d": { "number": "X", "numberUnit": "Active", "info": "XH|XM|XL", "infoState": "Error" } }
    lv_json = '{"d":{"number":"' && lv_number && '","numberUnit":"Active","info":"' && lv_info && '","infoState":"' && lv_info_state && '"}}'.

    " Set response
    mo_server->response->set_status( code = 200 reason = 'OK' ).
    mo_server->response->set_header_field( name = 'Content-Type' value = 'application/json' ).
    mo_server->response->set_cdata( data = lv_json ).

  ENDMETHOD.

  METHOD handle_get_log.
    " Returns all notifications that were set to SILENT display mode
    " Used for audit logging and reporting

    DATA: lt_notifications TYPE zcl_notification_manager=>tt_notifications,
          ls_notification TYPE zcl_notification_manager=>ty_notification,
          lt_db_notifications TYPE TABLE OF ztnotify_msgs,
          ls_db_notification TYPE ztnotify_msgs,
          lv_json TYPE string,
          lv_user_id TYPE string,
          lv_today TYPE sy-datum.

    lv_today = sy-datum.

    " Get user ID from query parameter (optional filter)
    lv_user_id = mo_server->request->get_form_field( 'user_id' ).

    " Select all notifications with SILENT display mode
    SELECT * FROM ztnotify_msgs
      INTO TABLE @lt_db_notifications
      WHERE active = 'X'
        AND start_date <= @lv_today
        AND end_date >= @lv_today
        AND display_mode = 'SILENT'
      ORDER BY changed_at DESCENDING.

    " Convert to notification structure
    LOOP AT lt_db_notifications INTO ls_db_notification.
      CLEAR ls_notification.
      ls_notification-message_id = ls_db_notification-message_id.
      ls_notification-message_type = ls_db_notification-message_type.
      ls_notification-severity = ls_db_notification-severity.
      ls_notification-title = ls_db_notification-title.
      ls_notification-message_text = ls_db_notification-message_text.
      ls_notification-start_date = ls_db_notification-start_date.
      ls_notification-end_date = ls_db_notification-end_date.
      ls_notification-target_users = ls_db_notification-target_users.
      ls_notification-active = ls_db_notification-active.
      ls_notification-display_mode = ls_db_notification-display_mode.
      ls_notification-created_by = ls_db_notification-created_by.
      ls_notification-created_at = ls_db_notification-created_at.
      ls_notification-changed_by = ls_db_notification-changed_by.
      ls_notification-changed_at = ls_db_notification-changed_at.

      " Apply user filter if specified
      IF lv_user_id IS NOT INITIAL.
        IF ls_notification-target_users IS INITIAL OR
           ls_notification-target_users CS lv_user_id.
          APPEND ls_notification TO lt_notifications.
        ENDIF.
      ELSE.
        APPEND ls_notification TO lt_notifications.
      ENDIF.
    ENDLOOP.

    " Serialize to JSON
    lv_json = serialize_notifications( lt_notifications ).

    " Set response
    mo_server->response->set_status( code = 200 reason = 'OK' ).
    mo_server->response->set_header_field( name = 'Content-Type' value = 'application/json' ).
    mo_server->response->set_cdata( data = lv_json ).

  ENDMETHOD.

ENDCLASS.
