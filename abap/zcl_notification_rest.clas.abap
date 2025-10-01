CLASS zcl_notification_rest DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC
  INHERITING FROM cl_rest_resource.

  PUBLIC SECTION.
    METHODS: if_rest_resource~post REDEFINITION,
             if_rest_resource~get REDEFINITION,
             if_rest_resource~put REDEFINITION,
             if_rest_resource~delete REDEFINITION.

  PRIVATE SECTION.
    METHODS: handle_get_notifications,
             handle_create_notification,
             handle_update_notification,
             handle_delete_notification,
             handle_get_stats,
             handle_get_log.

ENDCLASS.

CLASS zcl_notification_rest IMPLEMENTATION.

  METHOD if_rest_resource~get.
    DATA: lv_path TYPE string.

    " Get request path to route to correct handler
    lv_path = mo_request->get_uri_path( ).

    CASE lv_path.
      WHEN '/stats'.
        handle_get_stats( ).
      WHEN '/log'.
        handle_get_log( ).
      WHEN OTHERS.
        handle_get_notifications( ).
    ENDCASE.
  ENDMETHOD.

  METHOD if_rest_resource~post.
    handle_create_notification( ).
  ENDMETHOD.

  METHOD if_rest_resource~put.
    handle_update_notification( ).
  ENDMETHOD.

  METHOD if_rest_resource~delete.
    handle_delete_notification( ).
  ENDMETHOD.

  METHOD handle_get_notifications.

    DATA: lt_notifications TYPE zcl_notification_manager=>tt_notifications,
          lv_json TYPE string,
          lv_user_id TYPE sy-uname.

    " Get user ID from query parameter
    lv_user_id = mo_request->get_uri_query_parameter( 'user_id' ).

    " Get active notifications
    lt_notifications = zcl_notification_manager=>get_active_notifications( lv_user_id ).

    " Serialize to JSON
    lv_json = serialize_notifications( lt_notifications ).

    " Set response
    mo_response->create_entity( )->set_string_data( lv_json ).
    mo_response->set_status( cl_rest_status_code=>gc_success_ok ).
    mo_response->set_header_field( name = 'Content-Type' value = 'application/json' ).

  ENDMETHOD.

  METHOD handle_create_notification.

    DATA: lv_json TYPE string,
          ls_notification TYPE zcl_notification_manager=>ty_notification,
          lv_success TYPE abap_bool.

    " Get JSON from request body
    lv_json = mo_request->get_entity( )->get_string_data( ).

    " Deserialize JSON to notification structure
    ls_notification = deserialize_notification( lv_json ).

    " Create notification
    lv_success = zcl_notification_manager=>create_notification( ls_notification ).

    " Set response
    IF lv_success = abap_true.
      mo_response->create_entity( )->set_string_data( '{"success": true, "message": "Notification created"}' ).
      mo_response->set_status( cl_rest_status_code=>gc_success_created ).
    ELSE.
      mo_response->create_entity( )->set_string_data( '{"success": false, "message": "Failed to create notification"}' ).
      mo_response->set_status( cl_rest_status_code=>gc_server_error_internal ).
    ENDIF.

    mo_response->set_header_field( name = 'Content-Type' value = 'application/json' ).

  ENDMETHOD.

  METHOD handle_update_notification.

    DATA: lv_json TYPE string,
          ls_notification TYPE zcl_notification_manager=>ty_notification,
          lv_message_id TYPE string,
          lv_success TYPE abap_bool.

    " Get message ID from URI
    lv_message_id = mo_request->get_uri_query_parameter( 'message_id' ).

    " Get JSON from request body
    lv_json = mo_request->get_entity( )->get_string_data( ).

    " Deserialize JSON to notification structure
    ls_notification = deserialize_notification( lv_json ).

    " Update notification
    lv_success = zcl_notification_manager=>update_notification(
      iv_message_id = lv_message_id
      is_notification = ls_notification ).

    " Set response
    IF lv_success = abap_true.
      mo_response->create_entity( )->set_string_data( '{"success": true, "message": "Notification updated"}' ).
      mo_response->set_status( cl_rest_status_code=>gc_success_ok ).
    ELSE.
      mo_response->create_entity( )->set_string_data( '{"success": false, "message": "Failed to update notification"}' ).
      mo_response->set_status( cl_rest_status_code=>gc_server_error_internal ).
    ENDIF.

    mo_response->set_header_field( name = 'Content-Type' value = 'application/json' ).

  ENDMETHOD.

  METHOD handle_delete_notification.

    DATA: lv_message_id TYPE string,
          lv_success TYPE abap_bool.

    " Get message ID from URI
    lv_message_id = mo_request->get_uri_query_parameter( 'message_id' ).

    " Deactivate notification
    lv_success = zcl_notification_manager=>deactivate_notification( lv_message_id ).

    " Set response
    IF lv_success = abap_true.
      mo_response->create_entity( )->set_string_data( '{"success": true, "message": "Notification deleted"}' ).
      mo_response->set_status( cl_rest_status_code=>gc_success_ok ).
    ELSE.
      mo_response->create_entity( )->set_string_data( '{"success": false, "message": "Failed to delete notification"}' ).
      mo_response->set_status( cl_rest_status_code=>gc_server_error_internal ).
    ENDIF.

    mo_response->set_header_field( name = 'Content-Type' value = 'application/json' ).

  ENDMETHOD.

  METHOD serialize_notifications.

    DATA: lo_json TYPE REF TO cl_trex_json_serializer.

    CREATE OBJECT lo_json.
    rv_json = lo_json->serialize( data = it_notifications ).

  ENDMETHOD.

  METHOD deserialize_notification.

    DATA: lo_json TYPE REF TO cl_trex_json_deserializer.

    CREATE OBJECT lo_json.
    CALL METHOD lo_json->deserialize
      EXPORTING
        json = iv_json
      CHANGING
        data = rs_notification.

  ENDMETHOD.

  METHOD handle_get_stats.
    " Provides statistics for the dynamic tile counter
    " Response format: { total: N, high_count: N, medium_count: N, low_count: N }

    DATA: lt_notifications TYPE TABLE OF ztnotify_msgs,
          lv_json TYPE string,
          lv_total TYPE i,
          lv_high TYPE i,
          lv_medium TYPE i,
          lv_low TYPE i,
          lv_today TYPE sy-datum.

    lv_today = sy-datum.

    " Count active notifications by severity
    SELECT COUNT(*) FROM ztnotify_msgs
      INTO @lv_high
      WHERE active = 'X'
        AND start_date <= @lv_today
        AND end_date >= @lv_today
        AND severity = 'HIGH'.

    SELECT COUNT(*) FROM ztnotify_msgs
      INTO @lv_medium
      WHERE active = 'X'
        AND start_date <= @lv_today
        AND end_date >= @lv_today
        AND severity = 'MEDIUM'.

    SELECT COUNT(*) FROM ztnotify_msgs
      INTO @lv_low
      WHERE active = 'X'
        AND start_date <= @lv_today
        AND end_date >= @lv_today
        AND severity = 'LOW'.

    lv_total = lv_high + lv_medium + lv_low.

    " Build JSON response
    CONCATENATE '{'
                  '"total":' lv_total ','
                  '"high_count":' lv_high ','
                  '"medium_count":' lv_medium ','
                  '"low_count":' lv_low
                '}'
    INTO lv_json.

    " Set response
    mo_response->create_entity( )->set_string_data( lv_json ).
    mo_response->set_status( cl_rest_status_code=>gc_success_ok ).
    mo_response->set_header_field( name = 'Content-Type' value = 'application/json' ).

  ENDMETHOD.

  METHOD handle_get_log.
    " Returns all notifications that were set to SILENT display mode
    " Used for audit logging and reporting

    DATA: lt_notifications TYPE zcl_notification_manager=>tt_notifications,
          ls_notification TYPE zcl_notification_manager=>ty_notification,
          lt_db_notifications TYPE TABLE OF ztnotify_msgs,
          ls_db_notification TYPE ztnotify_msgs,
          lv_json TYPE string,
          lv_user_id TYPE sy-uname,
          lv_today TYPE sy-datum.

    lv_today = sy-datum.

    " Get user ID from query parameter (optional filter)
    lv_user_id = mo_request->get_uri_query_parameter( 'user_id' ).

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
    mo_response->create_entity( )->set_string_data( lv_json ).
    mo_response->set_status( cl_rest_status_code=>gc_success_ok ).
    mo_response->set_header_field( name = 'Content-Type' value = 'application/json' ).

  ENDMETHOD.

ENDCLASS.