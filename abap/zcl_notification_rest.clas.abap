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
             handle_get_valid_modes,
             handle_get_valid_types,
             handle_acknowledge,
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
        ELSEIF lv_path CS '/valid-modes'.
          handle_get_valid_modes( ).
        ELSEIF lv_path CS '/valid-types'.
          handle_get_valid_types( ).
        ELSE.
          handle_get_notifications( ).
        ENDIF.

      WHEN 'POST'.
        " Route to acknowledge endpoint or create notification
        IF lv_path CS '/acknowledge'.
          handle_acknowledge( ).
        ELSE.
          handle_create_notification( ).
        ENDIF.

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
          ls_notification TYPE zcl_notification_manager=>ty_notification,
          lt_db_notifications TYPE TABLE OF ztnotify_msgs,
          ls_db_notification TYPE ztnotify_msgs,
          lv_json TYPE string,
          lv_user_id_str TYPE string,
          lv_user_id TYPE sy-uname,
          lv_all_str TYPE string,
          lv_today TYPE sy-datum,
          lv_expired_count TYPE i.

    " Get user ID from query parameter
    lv_user_id_str = mo_server->request->get_form_field( 'user_id' ).
    lv_user_id = lv_user_id_str.

    " Get 'all' parameter - if 'X', return all notifications (for admin app)
    lv_all_str = mo_server->request->get_form_field( 'all' ).

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

    " Admin app: Return ALL notifications (active + inactive + expired)
    IF lv_all_str = 'X'.
      SELECT *
        FROM ztnotify_msgs
        INTO TABLE @lt_db_notifications
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
        ls_notification-requires_ack = ls_db_notification-requires_ack.
        ls_notification-created_by = ls_db_notification-created_by.
        ls_notification-created_at = ls_db_notification-created_at.
        ls_notification-changed_by = ls_db_notification-changed_by.
        ls_notification-changed_at = ls_db_notification-changed_at.
        APPEND ls_notification TO lt_notifications.
      ENDLOOP.

    ELSE.
      " Normal mode: Return only active notifications for current user
      lt_notifications = zcl_notification_manager=>get_active_notifications( lv_user_id ).
    ENDIF.

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
          lv_error_msg TYPE string,
          lv_message_id TYPE char32,
          lt_message_ids TYPE string_table,
          lv_response_json TYPE string.

    " Structure for parsing extended JSON with recurrence fields
    TYPES: BEGIN OF ty_notification_extended,
             message_type TYPE char12,
             severity TYPE char8,
             title TYPE char255,
             message_text TYPE char255,
             start_date TYPE dats,
             end_date TYPE dats,
             target_users TYPE char10,
             active TYPE char1,
             display_mode TYPE char10,
             is_recurring TYPE abap_bool,
             recurrence_type TYPE char1,
             occurrences TYPE i,
           END OF ty_notification_extended.

    " Response structures for JSON serialization
    TYPES: BEGIN OF ty_response_recurring,
             success TYPE abap_bool,
             message TYPE string,
             message_ids TYPE string_table,
           END OF ty_response_recurring.

    TYPES: BEGIN OF ty_response_single,
             success TYPE abap_bool,
             message TYPE string,
             message_id TYPE char32,
           END OF ty_response_single.

    DATA: ls_extended TYPE ty_notification_extended,
          ls_response_recurring TYPE ty_response_recurring,
          ls_response_single TYPE ty_response_single.

    TRY.
        " Get JSON from request body
        lv_json = mo_server->request->get_cdata( ).

        " Deserialize JSON to extended structure (includes recurrence fields)
        /ui2/cl_json=>deserialize(
          EXPORTING
            json = lv_json
            pretty_name = /ui2/cl_json=>pretty_mode-camel_case
          CHANGING
            data = ls_extended ).

        " Map to notification structure
        ls_notification-message_type = ls_extended-message_type.
        ls_notification-severity = ls_extended-severity.
        ls_notification-title = ls_extended-title.
        ls_notification-message_text = ls_extended-message_text.
        ls_notification-start_date = ls_extended-start_date.
        ls_notification-end_date = ls_extended-end_date.
        ls_notification-target_users = ls_extended-target_users.
        ls_notification-active = ls_extended-active.
        ls_notification-display_mode = ls_extended-display_mode.

        " Set response header
        mo_server->response->set_header_field( name = 'Content-Type' value = 'application/json' ).

        " Check if recurring notification requested
        IF ls_extended-is_recurring = abap_true.
          " Create recurring notifications
          lv_success = zcl_notification_manager=>create_recurring_notifications(
            EXPORTING
              is_notification = ls_notification
              iv_recurrence_type = ls_extended-recurrence_type
              iv_occurrences = ls_extended-occurrences
            IMPORTING
              et_message_ids = lt_message_ids ).

          IF lv_success = abap_true.
            " Return list of created message IDs
            ls_response_recurring-success = abap_true.
            ls_response_recurring-message = |Created { lines( lt_message_ids ) } recurring notifications|.
            ls_response_recurring-message_ids = lt_message_ids.

            lv_response_json = /ui2/cl_json=>serialize( data = ls_response_recurring ).

            mo_server->response->set_status( code = 201 reason = 'Created' ).
            mo_server->response->set_cdata( data = lv_response_json ).
          ELSE.
            mo_server->response->set_status( code = 500 reason = 'Internal Server Error' ).
            mo_server->response->set_cdata( data = '{"success": false, "message": "Failed to create recurring notifications"}' ).
          ENDIF.

        ELSE.
          " Create single notification
          lv_success = zcl_notification_manager=>create_notification(
            EXPORTING
              is_notification = ls_notification
            IMPORTING
              ev_message_id = lv_message_id ).

          IF lv_success = abap_true.
            " Return created message ID
            ls_response_single-success = abap_true.
            ls_response_single-message = 'Notification created'.
            ls_response_single-message_id = lv_message_id.

            lv_response_json = /ui2/cl_json=>serialize( data = ls_response_single ).

            mo_server->response->set_status( code = 201 reason = 'Created' ).
            mo_server->response->set_cdata( data = lv_response_json ).
          ELSE.
            mo_server->response->set_status( code = 500 reason = 'Internal Server Error' ).
            mo_server->response->set_cdata( data = '{"success": false, "message": "Failed to create notification - check authorization"}' ).
          ENDIF.
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
          lv_error_msg TYPE string,
          lv_deleted_rows TYPE i.

    TRY.
        " Get message ID from query parameter
        lv_message_id_str = mo_server->request->get_form_field( 'message_id' ).
        lv_message_id = lv_message_id_str.

        " Check authorization
        IF zcl_notification_manager=>check_user_authorization( ) = abap_false.
          mo_server->response->set_header_field( name = 'Content-Type' value = 'application/json' ).
          mo_server->response->set_status( code = 403 reason = 'Forbidden' ).
          mo_server->response->set_cdata( data = '{"success": false, "message": "Unauthorized to delete notifications"}' ).
          RETURN.
        ENDIF.

        " PHYSICAL DELETE from database (not just deactivate)
        DELETE FROM ztnotify_msgs WHERE message_id = @lv_message_id.
        lv_deleted_rows = sy-dbcnt.

        IF lv_deleted_rows > 0.
          COMMIT WORK.
          mo_server->response->set_header_field( name = 'Content-Type' value = 'application/json' ).
          mo_server->response->set_status( code = 200 reason = 'OK' ).
          mo_server->response->set_cdata( data = '{"success": true, "message": "Notification permanently deleted"}' ).
        ELSE.
          " No rows deleted - record not found
          mo_server->response->set_header_field( name = 'Content-Type' value = 'application/json' ).
          mo_server->response->set_status( code = 404 reason = 'Not Found' ).
          mo_server->response->set_cdata( data = '{"success": false, "message": "Notification not found"}' ).
        ENDIF.

      CATCH cx_root INTO DATA(lx_error).
        " Catch any exception and return detailed error message
        ROLLBACK WORK.
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

    " Build info string with colored emoji (format: "🔴 X  🟠 X  🟢 X")
    lv_info = '🔴 ' && lv_high_count && '  🟠 ' && lv_medium_count && '  🟢 ' && lv_low_count.

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
    " Format: { "d": { "number": "X", "numberUnit": "Active", "info": "🔴 X  🟠 X  🟢 X", "infoState": "Error" } }
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
      ls_notification-requires_ack = ls_db_notification-requires_ack.
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

  METHOD handle_get_valid_modes.
    " Returns valid display modes for given severity and message_type
    " Query parameters: severity, message_type
    " Response: { display_modes: [...], default_mode: "..." }
    DATA: lv_severity TYPE char8,
          lv_message_type TYPE char12,
          lt_display_modes TYPE string_table,
          lv_default_mode TYPE char10,
          lv_success TYPE abap_bool,
          lv_json TYPE string,
          lv_mode TYPE string,
          lv_first TYPE abap_bool.

    " Get query parameters
    lv_severity = mo_server->request->get_form_field( 'severity' ).
    lv_message_type = mo_server->request->get_form_field( 'message_type' ).

    " Validate required parameters
    IF lv_severity IS INITIAL OR lv_message_type IS INITIAL.
      lv_json = '{"error":"Missing required parameters: severity and message_type"}'.
      mo_server->response->set_status( code = 400 reason = 'Bad Request' ).
      mo_server->response->set_header_field( name = 'Content-Type' value = 'application/json' ).
      mo_server->response->set_cdata( data = lv_json ).
      RETURN.
    ENDIF.

    " Call validation method
    lv_success = zcl_notification_manager=>get_valid_display_modes(
      EXPORTING
        iv_severity = lv_severity
        iv_message_type = lv_message_type
      IMPORTING
        et_display_modes = lt_display_modes
        ev_default_mode = lv_default_mode
    ).

    IF lv_success = abap_false.
      lv_json = '{"error":"Failed to retrieve valid display modes"}'.
      mo_server->response->set_status( code = 500 reason = 'Internal Server Error' ).
      mo_server->response->set_header_field( name = 'Content-Type' value = 'application/json' ).
      mo_server->response->set_cdata( data = lv_json ).
      RETURN.
    ENDIF.

    " Build JSON response manually
    lv_json = '{"display_modes":['.
    lv_first = abap_true.

    LOOP AT lt_display_modes INTO lv_mode.
      IF lv_first = abap_false.
        CONCATENATE lv_json ',' INTO lv_json.
      ENDIF.
      lv_first = abap_false.
      CONCATENATE lv_json '"' lv_mode '"' INTO lv_json.
    ENDLOOP.

    CONCATENATE lv_json '],"default_mode":"' lv_default_mode '"' INTO lv_json.
    CONCATENATE lv_json '}' INTO lv_json.

    " Set response
    mo_server->response->set_status( code = 200 reason = 'OK' ).
    mo_server->response->set_header_field( name = 'Content-Type' value = 'application/json' ).
    mo_server->response->set_cdata( data = lv_json ).

  ENDMETHOD.

  METHOD handle_get_valid_types.
    " Returns valid message types for given severity
    " Query parameter: severity
    " Response: { message_types: [...], default_type: "..." }
    DATA: lv_severity TYPE char8,
          lt_message_types TYPE string_table,
          lv_default_type TYPE char12,
          lv_success TYPE abap_bool,
          lv_json TYPE string,
          lv_type TYPE string,
          lv_first TYPE abap_bool.

    " Get query parameter
    lv_severity = mo_server->request->get_form_field( 'severity' ).

    " Validate required parameter
    IF lv_severity IS INITIAL.
      lv_json = '{"error":"Missing required parameter: severity"}'.
      mo_server->response->set_status( code = 400 reason = 'Bad Request' ).
      mo_server->response->set_header_field( name = 'Content-Type' value = 'application/json' ).
      mo_server->response->set_cdata( data = lv_json ).
      RETURN.
    ENDIF.

    " Call validation method
    lv_success = zcl_notification_manager=>get_valid_message_types(
      EXPORTING
        iv_severity = lv_severity
      IMPORTING
        et_message_types = lt_message_types
        ev_default_type = lv_default_type
    ).

    IF lv_success = abap_false.
      lv_json = '{"error":"Failed to retrieve valid message types"}'.
      mo_server->response->set_status( code = 500 reason = 'Internal Server Error' ).
      mo_server->response->set_header_field( name = 'Content-Type' value = 'application/json' ).
      mo_server->response->set_cdata( data = lv_json ).
      RETURN.
    ENDIF.

    " Build JSON response manually
    lv_json = '{"message_types":['.
    lv_first = abap_true.

    LOOP AT lt_message_types INTO lv_type.
      IF lv_first = abap_false.
        CONCATENATE lv_json ',' INTO lv_json.
      ENDIF.
      lv_first = abap_false.
      CONCATENATE lv_json '"' lv_type '"' INTO lv_json.
    ENDLOOP.

    CONCATENATE lv_json '],"default_type":"' lv_default_type '"' INTO lv_json.
    CONCATENATE lv_json '}' INTO lv_json.

    " Set response
    mo_server->response->set_status( code = 200 reason = 'OK' ).
    mo_server->response->set_header_field( name = 'Content-Type' value = 'application/json' ).
    mo_server->response->set_cdata( data = lv_json ).

  ENDMETHOD.

  METHOD handle_acknowledge.
    "*----------------------------------------------------------------------*
    "* METHOD handle_acknowledge (v1.3.0 - Acknowledgment tracking)
    "*----------------------------------------------------------------------*
    "* Handle POST /acknowledge endpoint
    "* Records user acknowledgment of a notification
    "*----------------------------------------------------------------------*
    DATA: lv_json_request  TYPE string,
          lv_json_response TYPE string,
          lv_message_id    TYPE char32,
          lv_client_info   TYPE char255,
          lv_success       TYPE abap_bool,
          lv_timestamp     TYPE string.

    TRY.
        " Get request body (JSON)
        lv_json_request = mo_server->request->get_cdata( ).

        " Parse JSON to extract message_id and client_info
        " Format: {"message_id": "abc123", "client_info": "Mozilla/5.0..."}

        " Extract message_id using REGEX
        FIND REGEX '"message_id"\s*:\s*"([^"]+)"' IN lv_json_request
             SUBMATCHES lv_message_id.

        " Extract client_info (optional)
        FIND REGEX '"client_info"\s*:\s*"([^"]+)"' IN lv_json_request
             SUBMATCHES lv_client_info.

        " Validate message_id is provided
        IF lv_message_id IS INITIAL.
          " Bad request - missing message_id
          mo_server->response->set_status( code = 400 reason = 'Bad Request' ).
          lv_json_response = '{"success":false,"error":"message_id is required"}'.
          mo_server->response->set_cdata( data = lv_json_response ).
          mo_server->response->set_header_field( name = 'Content-Type' value = 'application/json' ).
          RETURN.
        ENDIF.

        " Record acknowledgment
        lv_success = zcl_notification_manager=>record_acknowledgment(
          iv_message_id  = lv_message_id
          iv_client_info = lv_client_info
        ).

        " Prepare response
        mo_server->response->set_header_field( name = 'Content-Type' value = 'application/json' ).

        IF lv_success = abap_true.
          " Success - acknowledgment recorded
          mo_server->response->set_status( code = 200 reason = 'OK' ).

          " Get current timestamp for response
          lv_timestamp = |{ sy-datum }T{ sy-uzeit }|.

          lv_json_response = '{"success":true,"timestamp":"' && lv_timestamp && '"}'.
          mo_server->response->set_cdata( data = lv_json_response ).

        ELSE.
          " Failed - check if already acknowledged
          IF zcl_notification_manager=>has_user_acknowledged( lv_message_id ) = abap_true.
            " Already acknowledged - return 409 Conflict
            mo_server->response->set_status( code = 409 reason = 'Conflict' ).
            lv_json_response = '{"success":false,"error":"Already acknowledged"}'.
          ELSE.
            " Generic error - return 500 Internal Server Error
            mo_server->response->set_status( code = 500 reason = 'Internal Server Error' ).
            lv_json_response = '{"success":false,"error":"Failed to record acknowledgment"}'.
          ENDIF.
          mo_server->response->set_cdata( data = lv_json_response ).
        ENDIF.

      CATCH cx_root INTO DATA(lx_error).
        " Exception handling
        mo_server->response->set_status( code = 500 reason = 'Internal Server Error' ).
        lv_json_response = '{"success":false,"error":"' && lx_error->get_text( ) && '"}'.
        mo_server->response->set_cdata( data = lv_json_response ).
        mo_server->response->set_header_field( name = 'Content-Type' value = 'application/json' ).
    ENDTRY.

  ENDMETHOD.

ENDCLASS.
