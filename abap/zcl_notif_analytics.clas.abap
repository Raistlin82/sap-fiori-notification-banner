CLASS zcl_notif_analytics DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES: if_http_extension.

  PRIVATE SECTION.
    DATA: mo_server TYPE REF TO if_http_server.

    TYPES: BEGIN OF ty_notification_summary,
             message_id       TYPE char32,
             title            TYPE char100,
             severity         TYPE char10,
             message_type     TYPE char20,
             target_users     TYPE char20,
             created_at       TYPE datum,
             total_targets    TYPE i,
             acknowledged     TYPE i,
             pending          TYPE i,
             compliance_pct   TYPE p LENGTH 5 DECIMALS 2,
           END OF ty_notification_summary.

    TYPES: tt_notification_summary TYPE STANDARD TABLE OF ty_notification_summary WITH DEFAULT KEY.

    TYPES: BEGIN OF ty_user_ack_status,
             userid           TYPE syuname,
             user_name        TYPE char40,
             acknowledged     TYPE abap_bool,
             ack_timestamp    TYPE timestampl,
             client_info      TYPE char255,
             days_pending     TYPE i,
           END OF ty_user_ack_status.

    TYPES: tt_user_ack_status TYPE STANDARD TABLE OF ty_user_ack_status WITH DEFAULT KEY.

    METHODS: handle_get_notifications,
             handle_get_analytics,
             get_notification_list
               RETURNING
                 VALUE(rt_notifications) TYPE tt_notification_summary,
             get_analytics_details
               IMPORTING
                 iv_message_id TYPE char32
               RETURNING
                 VALUE(rs_analytics) TYPE ty_notification_summary,
             get_user_status_list
               IMPORTING
                 iv_message_id        TYPE char32
               RETURNING
                 VALUE(rt_users) TYPE tt_user_ack_status,
             get_dialog_users
               RETURNING
                 VALUE(rt_users) TYPE tt_user_ack_status,
             serialize_json
               IMPORTING
                 iv_data        TYPE any
               RETURNING
                 VALUE(rv_json) TYPE string.

ENDCLASS.

CLASS zcl_notif_analytics IMPLEMENTATION.

  METHOD if_http_extension~handle_request.
    DATA: lv_method TYPE string,
          lv_path   TYPE string.

    " Store server reference
    mo_server = server.

    " CRITICAL: Set CORS headers for Fiori apps
    server->response->set_header_field(
      name  = 'Access-Control-Allow-Origin'
      value = '*' ).
    server->response->set_header_field(
      name  = 'Access-Control-Allow-Methods'
      value = 'GET,OPTIONS' ).
    server->response->set_header_field(
      name  = 'Access-Control-Allow-Headers'
      value = 'Content-Type,Accept,Authorization' ).

    " Get HTTP method and path
    lv_method = server->request->get_header_field( '~request_method' ).
    lv_path   = server->request->get_header_field( '~path_info' ).

    " Handle CORS preflight
    IF lv_method = 'OPTIONS'.
      server->response->set_status( code = 200 reason = 'OK' ).
      RETURN.
    ENDIF.

    " Route requests
    CASE lv_method.
      WHEN 'GET'.
        IF lv_path CS '/analytics/'.
          " GET /analytics/{message_id}
          handle_get_analytics( ).
        ELSE.
          " GET /notifications
          handle_get_notifications( ).
        ENDIF.

      WHEN OTHERS.
        " Method not allowed
        server->response->set_status( code = 405 reason = 'Method Not Allowed' ).
    ENDCASE.

  ENDMETHOD.

  METHOD handle_get_notifications.
    DATA: lt_notifications TYPE tt_notification_summary,
          lv_json          TYPE string.

    " Get list of notifications requiring acknowledgment
    lt_notifications = get_notification_list( ).

    " Serialize to JSON
    lv_json = serialize_json( lt_notifications ).

    " Set response
    mo_server->response->set_status( code = 200 reason = 'OK' ).
    mo_server->response->set_header_field(
      name  = 'Content-Type'
      value = 'application/json' ).
    mo_server->response->set_cdata( lv_json ).

  ENDMETHOD.

  METHOD handle_get_analytics.
    DATA: lv_path       TYPE string,
          lt_segments   TYPE string_table,
          lv_message_id TYPE char32,
          rs_analytics  TYPE ty_notification_summary,
          lt_users      TYPE tt_user_ack_status,
          lv_json       TYPE string.

    " Extract message_id from URL path
    " Example: /sap/bc/rest/zcl_notif_analytics/analytics/MSG_001
    lv_path = mo_server->request->get_header_field( '~path_info' ).
    SPLIT lv_path AT '/' INTO TABLE lt_segments.

    " Get last segment (message_id)
    READ TABLE lt_segments INDEX lines( lt_segments ) INTO lv_message_id.

    IF lv_message_id IS INITIAL.
      mo_server->response->set_status( code = 400 reason = 'Bad Request' ).
      mo_server->response->set_cdata( '{"error":"message_id required"}' ).
      RETURN.
    ENDIF.

    " Get analytics details
    rs_analytics = get_analytics_details( lv_message_id ).

    IF rs_analytics-message_id IS INITIAL.
      mo_server->response->set_status( code = 404 reason = 'Not Found' ).
      mo_server->response->set_cdata( '{"error":"Notification not found"}' ).
      RETURN.
    ENDIF.

    " Get user status list
    lt_users = get_user_status_list( lv_message_id ).

    " Build JSON response (simplified - use proper JSON serializer in production)
    CONCATENATE '{'
                '"message_id":"' rs_analytics-message_id '",'
                '"title":"' rs_analytics-title '",'
                '"severity":"' rs_analytics-severity '",'
                '"total_targets":' rs_analytics-total_targets ','
                '"acknowledged":' rs_analytics-acknowledged ','
                '"pending":' rs_analytics-pending ','
                '"compliance_pct":' rs_analytics-compliance_pct ','
                '"users":' serialize_json( lt_users )
                '}' INTO lv_json.

    " Set response
    mo_server->response->set_status( code = 200 reason = 'OK' ).
    mo_server->response->set_header_field(
      name  = 'Content-Type'
      value = 'application/json' ).
    mo_server->response->set_cdata( lv_json ).

  ENDMETHOD.

  METHOD get_notification_list.
    DATA: lt_notifications TYPE TABLE OF ztnotify_msgs,
          ls_notification  TYPE ztnotify_msgs,
          ls_summary       TYPE ty_notification_summary,
          lv_ack_count     TYPE i,
          lv_total_users   TYPE i.

    " Select all notifications requiring acknowledgment from analytics CDS view
    SELECT * FROM ztnotify_messages_analytics
      INTO CORRESPONDING FIELDS OF TABLE @lt_notifications
      ORDER BY created_at DESCENDING.

    " Calculate compliance for each notification
    LOOP AT lt_notifications INTO ls_notification.

      " Get total dialog users (target_users = ALL)
      " In real implementation, handle different target_users types
      SELECT COUNT( * )
        FROM i_user
        INTO @lv_total_users
        WHERE usertype = 'A'
          AND useraccountisblocked = ''.

      " Get acknowledged count
      SELECT COUNT( DISTINCT userid )
        FROM znotify_ack_log
        INTO @lv_ack_count
        WHERE message_id = @ls_notification-message_id.

      " Build summary
      ls_summary-message_id     = ls_notification-message_id.
      ls_summary-title          = ls_notification-title.
      ls_summary-severity       = ls_notification-severity.
      ls_summary-message_type   = ls_notification-message_type.
      ls_summary-target_users   = ls_notification-target_users.
      ls_summary-created_at     = ls_notification-created_at.
      ls_summary-total_targets  = lv_total_users.
      ls_summary-acknowledged   = lv_ack_count.
      ls_summary-pending        = lv_total_users - lv_ack_count.

      " Calculate compliance percentage
      IF lv_total_users > 0.
        ls_summary-compliance_pct = ( lv_ack_count * 100 ) / lv_total_users.
      ELSE.
        ls_summary-compliance_pct = 0.
      ENDIF.

      APPEND ls_summary TO rt_notifications.
      CLEAR: ls_summary, lv_ack_count, lv_total_users.

    ENDLOOP.

  ENDMETHOD.

  METHOD get_analytics_details.
    DATA: ls_notification TYPE ztnotify_msgs,
          lv_ack_count    TYPE i,
          lv_total_users  TYPE i.

    " Get notification details
    SELECT SINGLE * FROM ztnotify_messages_analytics
      INTO CORRESPONDING FIELDS OF @ls_notification
      WHERE message_id = @iv_message_id.

    IF sy-subrc <> 0.
      " Notification not found
      RETURN.
    ENDIF.

    " Get total dialog users
    SELECT COUNT( * )
      FROM i_user
      INTO @lv_total_users
      WHERE usertype = 'A'
        AND useraccountisblocked = ''.

    " Get acknowledged count
    SELECT COUNT( DISTINCT userid )
      FROM znotify_ack_log
      INTO @lv_ack_count
      WHERE message_id = @iv_message_id.

    " Build analytics summary
    rs_analytics-message_id     = ls_notification-message_id.
    rs_analytics-title          = ls_notification-title.
    rs_analytics-severity       = ls_notification-severity.
    rs_analytics-message_type   = ls_notification-message_type.
    rs_analytics-target_users   = ls_notification-target_users.
    rs_analytics-created_at     = ls_notification-created_at.
    rs_analytics-total_targets  = lv_total_users.
    rs_analytics-acknowledged   = lv_ack_count.
    rs_analytics-pending        = lv_total_users - lv_ack_count.

    " Calculate compliance percentage
    IF lv_total_users > 0.
      rs_analytics-compliance_pct = ( lv_ack_count * 100 ) / lv_total_users.
    ELSE.
      rs_analytics-compliance_pct = 0.
    ENDIF.

  ENDMETHOD.

  METHOD get_user_status_list.
    DATA: lt_all_users    TYPE TABLE OF i_user,
          ls_user         TYPE i_user,
          ls_ack_log      TYPE znotify_ack_log,
          ls_user_status  TYPE ty_user_ack_status,
          lv_created_date TYPE datum.

    " Get all dialog users from I_User CDS view
    SELECT userid, personfullname
      FROM i_user
      INTO CORRESPONDING FIELDS OF TABLE @lt_all_users
      WHERE usertype = 'A'
        AND useraccountisblocked = ''.

    " Get notification created date for days_pending calculation
    SELECT SINGLE created_at
      FROM ztnotify_msgs
      INTO @lv_created_date
      WHERE message_id = @iv_message_id.

    " Check acknowledgment status for each user
    LOOP AT lt_all_users INTO ls_user.

      ls_user_status-userid     = ls_user-userid.
      ls_user_status-user_name  = ls_user-personfullname.

      " Check if user has acknowledged
      SELECT SINGLE *
        FROM znotify_ack_log
        INTO @ls_ack_log
        WHERE message_id = @iv_message_id
          AND userid     = @ls_user-userid.

      IF sy-subrc = 0.
        " User has acknowledged
        ls_user_status-acknowledged   = abap_true.
        ls_user_status-ack_timestamp  = ls_ack_log-ack_timestamp.
        ls_user_status-client_info    = ls_ack_log-client_info.
        ls_user_status-days_pending   = 0.
      ELSE.
        " User has NOT acknowledged
        ls_user_status-acknowledged   = abap_false.
        CLEAR: ls_user_status-ack_timestamp, ls_user_status-client_info.

        " Calculate days pending
        IF lv_created_date IS NOT INITIAL.
          ls_user_status-days_pending = sy-datum - lv_created_date.
        ENDIF.
      ENDIF.

      APPEND ls_user_status TO rt_users.
      CLEAR: ls_user_status, ls_ack_log.

    ENDLOOP.

  ENDMETHOD.

  METHOD get_dialog_users.
    " Return all dialog users (for future use)
    SELECT userid, personfullname
      FROM i_user
      INTO CORRESPONDING FIELDS OF TABLE @rt_users
      WHERE usertype = 'A'
        AND useraccountisblocked = ''.
  ENDMETHOD.

  METHOD serialize_json.
    " Simplified JSON serialization
    " In production, use /UI2/CL_JSON or similar
    DATA: lv_json TYPE string.

    " This is a placeholder - implement proper JSON serialization
    " For now, return empty array
    rv_json = '[]'.

    " TODO: Implement proper JSON serialization
    " Example using /UI2/CL_JSON:
    " /ui2/cl_json=>serialize(
    "   data        = iv_data
    "   pretty_name = /ui2/cl_json=>pretty_mode-low_case ).

  ENDMETHOD.

ENDCLASS.
