CLASS zcl_notification_rest DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES: if_http_extension.

  PRIVATE SECTION.
    TYPES: tt_db_notifications TYPE STANDARD TABLE OF ztnotify_msgs WITH DEFAULT KEY.

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
             handle_csrf_token
               IMPORTING
                 iv_method TYPE string
               RETURNING
                 VALUE(rv_continue) TYPE abap_bool,
             send_json
               IMPORTING
                 iv_status TYPE i
                 iv_reason TYPE string
                 iv_json TYPE string,
             send_error
               IMPORTING
                 iv_status TYPE i
                 iv_reason TYPE string
                 iv_message TYPE string,
             map_db_to_notifications
               IMPORTING
                 it_db_notifications TYPE tt_db_notifications
               RETURNING
                 VALUE(rt_notifications) TYPE zcl_notification_manager=>tt_notifications,
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

    " No CORS headers: the Fiori apps are served from the same origin
    " (BSP / HTML5 repository via the same host), so cross-origin access
    " is neither needed nor allowed.

    " Get HTTP method and path
    lv_method = server->request->get_header_field( '~request_method' ).
    lv_path = server->request->get_header_field( '~path_info' ).

    " Handle OPTIONS requests
    IF lv_method = 'OPTIONS'.
      server->response->set_header_field( name = 'Allow' value = 'GET,POST,PUT,DELETE,OPTIONS' ).
      server->response->set_status( code = 200 reason = 'OK' ).
      RETURN.
    ENDIF.

    " CSRF protection: hand out tokens on 'X-CSRF-Token: Fetch' and
    " require a valid token for every modifying request
    IF handle_csrf_token( lv_method ) = abap_false.
      RETURN.
    ENDIF.

    " Route based on HTTP method and path
    CASE lv_method.
      WHEN 'GET' OR 'HEAD'.
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
        send_error( iv_status = 405 iv_reason = 'Method Not Allowed' iv_message = 'Method not allowed' ).
    ENDCASE.

  ENDMETHOD.

  METHOD handle_csrf_token.
    "*----------------------------------------------------------------------*
    "* Standard SAP CSRF handshake (same protocol as OData / CL_REST_HTTP_HANDLER):
    "* - Any request with header 'X-CSRF-Token: Fetch' receives a token
    "* - POST/PUT/DELETE must send that token back, otherwise 403 is returned
    "*   with header 'X-CSRF-Token: Required' so the client can re-fetch
    "*----------------------------------------------------------------------*
    DATA: lv_header TYPE string,
          lv_token TYPE string,
          lv_valid TYPE abap_bool.

    rv_continue = abap_true.

    lv_header = mo_server->request->get_header_field( 'x-csrf-token' ).

    IF to_upper( lv_header ) = 'FETCH'.
      mo_server->get_xsrf_token(
        IMPORTING
          token = lv_token
        EXCEPTIONS
          OTHERS = 1 ).
      IF sy-subrc = 0.
        mo_server->response->set_header_field( name = 'x-csrf-token' value = lv_token ).
      ENDIF.
    ENDIF.

    IF iv_method = 'POST' OR iv_method = 'PUT' OR iv_method = 'DELETE'.
      mo_server->validate_xsrf_token(
        IMPORTING
          successful = lv_valid
        EXCEPTIONS
          OTHERS = 1 ).
      IF sy-subrc <> 0 OR lv_valid <> abap_true.
        mo_server->response->set_header_field( name = 'x-csrf-token' value = 'Required' ).
        send_error( iv_status = 403 iv_reason = 'Forbidden' iv_message = 'CSRF token validation failed' ).
        rv_continue = abap_false.
      ENDIF.
    ENDIF.

  ENDMETHOD.

  METHOD send_json.

    mo_server->response->set_status( code = iv_status reason = iv_reason ).
    mo_server->response->set_header_field( name = 'Content-Type' value = 'application/json' ).
    mo_server->response->set_cdata( data = iv_json ).

  ENDMETHOD.

  METHOD send_error.
    " Serialize via /ui2/cl_json so quotes or backslashes in the message
    " (e.g. exception texts) never break the JSON response
    TYPES: BEGIN OF ty_error,
             success TYPE abap_bool,
             message TYPE string,
             error TYPE string,
           END OF ty_error.

    DATA(ls_error) = VALUE ty_error( success = abap_false
                                     message = iv_message
                                     error = iv_message ).

    send_json( iv_status = iv_status
               iv_reason = iv_reason
               iv_json = /ui2/cl_json=>serialize( data = ls_error
                                                  pretty_name = /ui2/cl_json=>pretty_mode-low_case ) ).

  ENDMETHOD.

  METHOD handle_get_notifications.

    DATA: lt_notifications TYPE zcl_notification_manager=>tt_notifications,
          lt_db_notifications TYPE tt_db_notifications,
          lv_all_str TYPE string.

    " Get 'all' parameter - if 'X', return all notifications (for admin app)
    lv_all_str = mo_server->request->get_form_field( 'all' ).

    " Admin app: Return ALL notifications (active + inactive + expired)
    IF lv_all_str = 'X'.

      " Only notification administrators may see inactive/expired entries
      IF zcl_notification_manager=>check_user_authorization( ) = abap_false.
        send_error( iv_status = 403 iv_reason = 'Forbidden'
                    iv_message = 'Unauthorized to list all notifications' ).
        RETURN.
      ENDIF.

      " Auto-expire: deactivate notifications whose end_date has passed.
      " Done here (admin list only) instead of on every end-user poll;
      " end users are not affected because they only get notifications
      " within their validity period anyway.
      zcl_notification_manager=>expire_outdated_notifications( ).

      SELECT *
        FROM ztnotify_msgs
        INTO TABLE @lt_db_notifications
        ORDER BY changed_at DESCENDING.

      lt_notifications = map_db_to_notifications( lt_db_notifications ).

    ELSE.
      " Normal mode: Return only active notifications for the logged-on user.
      " The user is always taken from the session (sy-uname), never from
      " a request parameter.
      lt_notifications = zcl_notification_manager=>get_active_notifications( ).
    ENDIF.

    send_json( iv_status = 200 iv_reason = 'OK' iv_json = serialize_notifications( lt_notifications ) ).

  ENDMETHOD.

  METHOD handle_create_notification.

    DATA: lv_json TYPE string,
          ls_notification TYPE zcl_notification_manager=>ty_notification,
          lv_success TYPE abap_bool,
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
        " Check authorization first so that a 403 is not reported as a validation error
        IF zcl_notification_manager=>check_user_authorization( ) = abap_false.
          send_error( iv_status = 403 iv_reason = 'Forbidden'
                      iv_message = 'Unauthorized to create notifications' ).
          RETURN.
        ENDIF.

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

        " Check if recurring notification requested
        IF ls_extended-is_recurring = abap_true.
          " Create recurring notifications (all or nothing)
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

            lv_response_json = /ui2/cl_json=>serialize( data = ls_response_recurring
                                                        pretty_name = /ui2/cl_json=>pretty_mode-low_case ).
            send_json( iv_status = 201 iv_reason = 'Created' iv_json = lv_response_json ).
          ELSE.
            send_error( iv_status = 400 iv_reason = 'Bad Request'
                        iv_message = 'Failed to create recurring notifications - check recurrence settings '
                                  && 'and the severity/type/display mode combination' ).
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

            lv_response_json = /ui2/cl_json=>serialize( data = ls_response_single
                                                        pretty_name = /ui2/cl_json=>pretty_mode-low_case ).
            send_json( iv_status = 201 iv_reason = 'Created' iv_json = lv_response_json ).
          ELSE.
            send_error( iv_status = 400 iv_reason = 'Bad Request'
                        iv_message = 'Failed to create notification - severity/type/display mode '
                                  && 'combination not allowed by notification matrix' ).
          ENDIF.
        ENDIF.

      CATCH cx_root INTO DATA(lx_error).
        " Catch any exception and return detailed error message
        send_error( iv_status = 500 iv_reason = 'Internal Server Error'
                    iv_message = |Exception: { lx_error->get_text( ) }| ).
    ENDTRY.

  ENDMETHOD.

  METHOD handle_update_notification.

    DATA: lv_json TYPE string,
          ls_notification TYPE zcl_notification_manager=>ty_notification,
          lv_message_id_str TYPE string,
          lv_message_id TYPE char32,
          lv_success TYPE abap_bool.

    TRY.
        " Check authorization first so that a 403 is not reported as a validation error
        IF zcl_notification_manager=>check_user_authorization( ) = abap_false.
          send_error( iv_status = 403 iv_reason = 'Forbidden'
                      iv_message = 'Unauthorized to update notifications' ).
          RETURN.
        ENDIF.

        " Get message ID from query parameter
        lv_message_id_str = mo_server->request->get_form_field( 'message_id' ).
        lv_message_id = lv_message_id_str.

        IF lv_message_id IS INITIAL.
          send_error( iv_status = 400 iv_reason = 'Bad Request' iv_message = 'message_id is required' ).
          RETURN.
        ENDIF.

        " Get JSON from request body
        lv_json = mo_server->request->get_cdata( ).

        " Deserialize JSON to notification structure
        ls_notification = deserialize_notification( lv_json ).

        " Update notification
        lv_success = zcl_notification_manager=>update_notification(
          iv_message_id = lv_message_id
          is_notification = ls_notification ).

        IF lv_success = abap_true.
          send_json( iv_status = 200 iv_reason = 'OK'
                     iv_json = '{"success": true, "message": "Notification updated"}' ).
        ELSE.
          send_error( iv_status = 400 iv_reason = 'Bad Request'
                      iv_message = 'Failed to update notification - not found or combination '
                                && 'not allowed by notification matrix' ).
        ENDIF.

      CATCH cx_root INTO DATA(lx_error).
        " Catch any exception and return detailed error message
        send_error( iv_status = 500 iv_reason = 'Internal Server Error'
                    iv_message = |Exception: { lx_error->get_text( ) }| ).
    ENDTRY.

  ENDMETHOD.

  METHOD handle_delete_notification.

    DATA: lv_message_id_str TYPE string,
          lv_message_id TYPE char32,
          lv_deleted_rows TYPE i.

    TRY.
        " Get message ID from query parameter
        lv_message_id_str = mo_server->request->get_form_field( 'message_id' ).
        lv_message_id = lv_message_id_str.

        " Check authorization
        IF zcl_notification_manager=>check_user_authorization( ) = abap_false.
          send_error( iv_status = 403 iv_reason = 'Forbidden'
                      iv_message = 'Unauthorized to delete notifications' ).
          RETURN.
        ENDIF.

        " PHYSICAL DELETE from database (not just deactivate)
        DELETE FROM ztnotify_msgs WHERE message_id = @lv_message_id.
        lv_deleted_rows = sy-dbcnt.

        IF lv_deleted_rows > 0.
          " Remove acknowledgment records of the deleted notification
          " so no orphaned rows remain in ZNOTIFY_ACK_LOG
          DELETE FROM znotify_ack_log WHERE message_id = @lv_message_id.
          COMMIT WORK.
          send_json( iv_status = 200 iv_reason = 'OK'
                     iv_json = '{"success": true, "message": "Notification permanently deleted"}' ).
        ELSE.
          " No rows deleted - record not found
          send_error( iv_status = 404 iv_reason = 'Not Found' iv_message = 'Notification not found' ).
        ENDIF.

      CATCH cx_root INTO DATA(lx_error).
        " Catch any exception and return detailed error message
        ROLLBACK WORK.
        send_error( iv_status = 500 iv_reason = 'Internal Server Error'
                    iv_message = |Exception: { lx_error->get_text( ) }| ).
    ENDTRY.

  ENDMETHOD.

  METHOD map_db_to_notifications.

    LOOP AT it_db_notifications INTO DATA(ls_db_notification).
      APPEND CORRESPONDING #( ls_db_notification ) TO rt_notifications.
    ENDLOOP.

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

    send_json( iv_status = 200 iv_reason = 'OK' iv_json = lv_json ).

  ENDMETHOD.

  METHOD handle_get_log.
    " Returns all notifications that were set to SILENT display mode
    " Used for audit logging and reporting

    DATA: lt_notifications TYPE zcl_notification_manager=>tt_notifications,
          lt_db_notifications TYPE tt_db_notifications,
          lv_user_id_str TYPE string,
          lv_user_id TYPE sy-uname,
          lv_today TYPE sy-datum.

    lv_today = sy-datum.

    " Optional filter: only notifications visible to this user (target audience).
    " Querying the visibility of another user is reserved for administrators.
    lv_user_id_str = mo_server->request->get_form_field( 'user_id' ).
    lv_user_id = to_upper( lv_user_id_str ).

    IF lv_user_id IS NOT INITIAL AND lv_user_id <> sy-uname
       AND zcl_notification_manager=>check_user_authorization( ) = abap_false.
      send_error( iv_status = 403 iv_reason = 'Forbidden'
                  iv_message = 'Unauthorized to query the log of other users' ).
      RETURN.
    ENDIF.

    " Select all notifications with SILENT display mode
    SELECT * FROM ztnotify_msgs
      INTO TABLE @lt_db_notifications
      WHERE active = 'X'
        AND start_date <= @lv_today
        AND end_date >= @lv_today
        AND display_mode = 'SILENT'
      ORDER BY changed_at DESCENDING.

    lt_notifications = map_db_to_notifications( lt_db_notifications ).

    " Apply user filter if specified (based on TARGET_USERS role mapping)
    IF lv_user_id IS NOT INITIAL.
      LOOP AT lt_notifications ASSIGNING FIELD-SYMBOL(<ls_notification>).
        IF zcl_notification_manager=>check_target_audience(
             iv_target_users = <ls_notification>-target_users
             iv_user_id = lv_user_id ) = abap_false.
          DELETE lt_notifications.
        ENDIF.
      ENDLOOP.
    ENDIF.

    send_json( iv_status = 200 iv_reason = 'OK' iv_json = serialize_notifications( lt_notifications ) ).

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
      send_error( iv_status = 400 iv_reason = 'Bad Request'
                  iv_message = 'Missing required parameters: severity and message_type' ).
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
      send_error( iv_status = 500 iv_reason = 'Internal Server Error'
                  iv_message = 'Failed to retrieve valid display modes' ).
      RETURN.
    ENDIF.

    " Build JSON response manually (values come from domain fixed values)
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

    send_json( iv_status = 200 iv_reason = 'OK' iv_json = lv_json ).

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
      send_error( iv_status = 400 iv_reason = 'Bad Request'
                  iv_message = 'Missing required parameter: severity' ).
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
      send_error( iv_status = 500 iv_reason = 'Internal Server Error'
                  iv_message = 'Failed to retrieve valid message types' ).
      RETURN.
    ENDIF.

    " Build JSON response manually (values come from domain fixed values)
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

    send_json( iv_status = 200 iv_reason = 'OK' iv_json = lv_json ).

  ENDMETHOD.

  METHOD handle_acknowledge.
    "*----------------------------------------------------------------------*
    "* METHOD handle_acknowledge (v1.3.0 - Acknowledgment tracking)
    "*----------------------------------------------------------------------*
    "* Handle POST /acknowledge endpoint
    "* Records user acknowledgment of a notification
    "* Request: {"message_id": "abc123", "client_info": "Mozilla/5.0..."}
    "*----------------------------------------------------------------------*
    TYPES: BEGIN OF ty_ack_request,
             message_id  TYPE char32,
             client_info TYPE char255,
           END OF ty_ack_request.

    DATA: ls_request       TYPE ty_ack_request,
          lv_json_response TYPE string,
          lv_success       TYPE abap_bool,
          lv_exists        TYPE abap_bool,
          lv_timestamp     TYPE string.

    TRY.
        " Parse JSON body with the standard serializer (handles escaped
        " characters in the user agent string correctly)
        /ui2/cl_json=>deserialize(
          EXPORTING
            json = mo_server->request->get_cdata( )
            pretty_name = /ui2/cl_json=>pretty_mode-low_case
          CHANGING
            data = ls_request ).

        " Validate message_id is provided
        IF ls_request-message_id IS INITIAL.
          send_error( iv_status = 400 iv_reason = 'Bad Request' iv_message = 'message_id is required' ).
          RETURN.
        ENDIF.

        " Only existing notifications can be acknowledged
        SELECT SINGLE @abap_true
          FROM ztnotify_msgs
          INTO @lv_exists
          WHERE message_id = @ls_request-message_id.

        IF sy-subrc <> 0.
          send_error( iv_status = 404 iv_reason = 'Not Found' iv_message = 'Notification not found' ).
          RETURN.
        ENDIF.

        " Record acknowledgment (always for the logged-on user)
        lv_success = zcl_notification_manager=>record_acknowledgment(
          iv_message_id  = ls_request-message_id
          iv_client_info = ls_request-client_info
        ).

        IF lv_success = abap_true.
          " Success - acknowledgment recorded
          lv_timestamp = |{ sy-datum }T{ sy-uzeit }|.
          lv_json_response = '{"success":true,"timestamp":"' && lv_timestamp && '"}'.
          send_json( iv_status = 200 iv_reason = 'OK' iv_json = lv_json_response ).

        ELSEIF zcl_notification_manager=>has_user_acknowledged( ls_request-message_id ) = abap_true.
          " Already acknowledged - return 409 Conflict
          send_error( iv_status = 409 iv_reason = 'Conflict' iv_message = 'Already acknowledged' ).

        ELSE.
          " Generic error - return 500 Internal Server Error
          send_error( iv_status = 500 iv_reason = 'Internal Server Error'
                      iv_message = 'Failed to record acknowledgment' ).
        ENDIF.

      CATCH cx_root INTO DATA(lx_error).
        send_error( iv_status = 500 iv_reason = 'Internal Server Error' iv_message = lx_error->get_text( ) ).
    ENDTRY.

  ENDMETHOD.

ENDCLASS.
