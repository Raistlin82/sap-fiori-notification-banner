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

  METHOD if_rest_resource~get.
    handle_get_notifications( ).
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

ENDCLASS.