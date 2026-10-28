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
             title            TYPE char255,
             severity         TYPE char8,
             message_type     TYPE char12,
             target_users     TYPE char10,
             created_at       TYPE datum,
             total_targets    TYPE i,
             acknowledged     TYPE i,
             pending          TYPE i,
             compliance_pct   TYPE p LENGTH 5 DECIMALS 2,
           END OF ty_notification_summary.

    TYPES: tt_notification_summary TYPE STANDARD TABLE OF ty_notification_summary WITH DEFAULT KEY.

    TYPES: BEGIN OF ty_user_ack_status,
             userid           TYPE syuname,
             user_name        TYPE char80,
             acknowledged     TYPE abap_bool,
             ack_timestamp    TYPE timestampl,
             client_info      TYPE char255,
             days_pending     TYPE i,
           END OF ty_user_ack_status.

    TYPES: tt_user_ack_status TYPE STANDARD TABLE OF ty_user_ack_status WITH DEFAULT KEY.

    " Response of GET /notifications
    TYPES: BEGIN OF ty_list_response,
             notifications TYPE tt_notification_summary,
           END OF ty_list_response.

    " Response of GET /analytics/{message_id}
    TYPES: BEGIN OF ty_analytics_response.
             INCLUDE TYPE ty_notification_summary.
    TYPES:   users TYPE tt_user_ack_status,
           END OF ty_analytics_response.

    " Target user lists are cached per TARGET_USERS value within one request
    TYPES: BEGIN OF ty_target_cache,
             target_users TYPE char10,
             users        TYPE tt_user_ack_status,
           END OF ty_target_cache.

    DATA: mt_target_cache TYPE HASHED TABLE OF ty_target_cache WITH UNIQUE KEY target_users.

    METHODS: handle_get_notifications,
             handle_get_analytics,
             get_notification_list
               RETURNING
                 VALUE(rt_notifications) TYPE tt_notification_summary,
             get_user_status_list
               IMPORTING
                 is_notification TYPE ztnotify_msgs
               RETURNING
                 VALUE(rt_users) TYPE tt_user_ack_status,
             build_summary
               IMPORTING
                 is_notification TYPE ztnotify_msgs
                 it_users        TYPE tt_user_ack_status
               RETURNING
                 VALUE(rs_summary) TYPE ty_notification_summary,
             get_target_users
               IMPORTING
                 iv_target_users TYPE char10
               RETURNING
                 VALUE(rt_users) TYPE tt_user_ack_status,
             get_dialog_users
               RETURNING
                 VALUE(rt_users) TYPE tt_user_ack_status,
             to_date
               IMPORTING
                 iv_timestamp   TYPE timestampl
               RETURNING
                 VALUE(rv_date) TYPE datum,
             send_json
               IMPORTING
                 iv_status TYPE i
                 iv_reason TYPE string
                 iv_json   TYPE string,
             send_error
               IMPORTING
                 iv_status  TYPE i
                 iv_reason  TYPE string
                 iv_message TYPE string,
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
    CLEAR mt_target_cache.

    " No CORS headers: the dashboard is served from the same origin.

    " Get HTTP method and path
    lv_method = server->request->get_header_field( '~request_method' ).
    lv_path   = server->request->get_header_field( '~path_info' ).

    " Handle OPTIONS requests
    IF lv_method = 'OPTIONS'.
      server->response->set_header_field( name = 'Allow' value = 'GET,OPTIONS' ).
      server->response->set_status( code = 200 reason = 'OK' ).
      RETURN.
    ENDIF.

    " Acknowledgment data (who read what, browser info) is restricted
    " to notification administrators
    IF zcl_notification_manager=>check_user_authorization( ) = abap_false.
      send_error( iv_status = 403 iv_reason = 'Forbidden'
                  iv_message = 'Unauthorized to view notification analytics' ).
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
        send_error( iv_status = 405 iv_reason = 'Method Not Allowed' iv_message = 'Method not allowed' ).
    ENDCASE.

  ENDMETHOD.

  METHOD handle_get_notifications.
    " Response: { "notifications": [ {summary}, ... ] }
    DATA: ls_response TYPE ty_list_response.

    ls_response-notifications = get_notification_list( ).

    send_json( iv_status = 200 iv_reason = 'OK' iv_json = serialize_json( ls_response ) ).

  ENDMETHOD.

  METHOD handle_get_analytics.
    " Response: { summary fields..., "users": [ {user status}, ... ] }
    DATA: lv_path         TYPE string,
          lt_segments     TYPE string_table,
          lv_segment      TYPE string,
          lv_message_id   TYPE char32,
          ls_notification TYPE ztnotify_msgs,
          ls_response     TYPE ty_analytics_response.

    " Extract message_id from URL path
    " Example: /sap/bc/rest/zcl_notif_analytics/analytics/MSG_001
    lv_path = mo_server->request->get_header_field( '~path_info' ).
    SPLIT lv_path AT '/' INTO TABLE lt_segments.

    " Get last segment (message_id)
    IF lines( lt_segments ) > 0.
      lv_segment = lt_segments[ lines( lt_segments ) ].
      lv_message_id = lv_segment.
    ENDIF.

    IF lv_message_id IS INITIAL.
      send_error( iv_status = 400 iv_reason = 'Bad Request' iv_message = 'message_id required' ).
      RETURN.
    ENDIF.

    " Get notification details (only notifications requiring acknowledgment)
    SELECT SINGLE * FROM ztnotify_messages_analytics
      INTO CORRESPONDING FIELDS OF @ls_notification
      WHERE message_id = @lv_message_id.

    IF sy-subrc <> 0.
      send_error( iv_status = 404 iv_reason = 'Not Found' iv_message = 'Notification not found' ).
      RETURN.
    ENDIF.

    ls_response-users = get_user_status_list( ls_notification ).
    DATA(ls_summary) = build_summary( is_notification = ls_notification
                                      it_users        = ls_response-users ).
    MOVE-CORRESPONDING ls_summary TO ls_response.

    send_json( iv_status = 200 iv_reason = 'OK' iv_json = serialize_json( ls_response ) ).

  ENDMETHOD.

  METHOD get_notification_list.
    DATA: lt_notifications TYPE TABLE OF ztnotify_msgs.

    " Select all notifications requiring acknowledgment from analytics CDS view
    SELECT * FROM ztnotify_messages_analytics
      INTO CORRESPONDING FIELDS OF TABLE @lt_notifications
      ORDER BY created_at DESCENDING.

    " Calculate compliance for each notification
    LOOP AT lt_notifications INTO DATA(ls_notification).
      APPEND build_summary( is_notification = ls_notification
                            it_users        = get_user_status_list( ls_notification ) )
        TO rt_notifications.
    ENDLOOP.

  ENDMETHOD.

  METHOD build_summary.
    DATA: lv_ack_count TYPE i.

    LOOP AT it_users TRANSPORTING NO FIELDS WHERE acknowledged = abap_true.
      lv_ack_count = lv_ack_count + 1.
    ENDLOOP.

    rs_summary-message_id    = is_notification-message_id.
    rs_summary-title         = is_notification-title.
    rs_summary-severity      = is_notification-severity.
    rs_summary-message_type  = is_notification-message_type.
    rs_summary-target_users  = is_notification-target_users.
    rs_summary-created_at    = to_date( is_notification-created_at ).
    rs_summary-total_targets = lines( it_users ).
    rs_summary-acknowledged  = lv_ack_count.
    rs_summary-pending       = rs_summary-total_targets - lv_ack_count.

    " Calculate compliance percentage
    IF rs_summary-total_targets > 0.
      rs_summary-compliance_pct = ( lv_ack_count * 100 ) / rs_summary-total_targets.
    ENDIF.

  ENDMETHOD.

  METHOD get_user_status_list.
    " Acknowledgment status for every user in the notification's target audience
    DATA: lt_ack_log      TYPE HASHED TABLE OF znotify_ack_log WITH UNIQUE KEY message_id userid,
          lv_created_date TYPE datum.

    rt_users = get_target_users( is_notification-target_users ).

    " Read all acknowledgments of this notification at once
    SELECT *
      FROM znotify_ack_log
      INTO TABLE @lt_ack_log
      WHERE message_id = @is_notification-message_id.

    lv_created_date = to_date( is_notification-created_at ).

    LOOP AT rt_users ASSIGNING FIELD-SYMBOL(<ls_user>).
      READ TABLE lt_ack_log INTO DATA(ls_ack_log)
        WITH TABLE KEY message_id = is_notification-message_id
                       userid     = <ls_user>-userid.

      IF sy-subrc = 0.
        " User has acknowledged
        <ls_user>-acknowledged  = abap_true.
        <ls_user>-ack_timestamp = ls_ack_log-ack_timestamp.
        <ls_user>-client_info   = ls_ack_log-client_info.
        <ls_user>-days_pending  = 0.
      ELSE.
        " User has NOT acknowledged - days since the notification was created
        <ls_user>-acknowledged = abap_false.
        IF lv_created_date IS NOT INITIAL.
          <ls_user>-days_pending = sy-datum - lv_created_date.
        ENDIF.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD get_target_users.
    " Users that are expected to see the notification, mirroring
    " zcl_notification_manager=>check_target_audience:
    "   ALL / blank -> all active dialog users
    "   ADMIN       -> dialog users with role Z_BR_ADMINISTRATOR
    "   DEVELOPER   -> dialog users with role SAP_BR_DEVELOPER
    DATA: lv_role       TYPE agr_name,
          lt_role_users TYPE SORTED TABLE OF syuname WITH UNIQUE KEY table_line.

    READ TABLE mt_target_cache INTO DATA(ls_cache)
      WITH TABLE KEY target_users = iv_target_users.
    IF sy-subrc = 0.
      rt_users = ls_cache-users.
      RETURN.
    ENDIF.

    rt_users = get_dialog_users( ).

    CASE iv_target_users.
      WHEN 'ALL' OR space.
        CLEAR lv_role.
      WHEN 'ADMIN'.
        lv_role = 'Z_BR_ADMINISTRATOR'.
      WHEN 'DEVELOPER'.
        lv_role = 'SAP_BR_DEVELOPER'.
      WHEN OTHERS.
        " Unknown audience is never shown to anybody
        CLEAR rt_users.
    ENDCASE.

    IF lv_role IS NOT INITIAL.
      SELECT DISTINCT uname
        FROM agr_users
        WHERE agr_name = @lv_role
        INTO TABLE @lt_role_users.

      LOOP AT rt_users ASSIGNING FIELD-SYMBOL(<ls_user>).
        IF NOT line_exists( lt_role_users[ table_line = <ls_user>-userid ] ).
          DELETE rt_users.
        ENDIF.
      ENDLOOP.
    ENDIF.

    INSERT VALUE #( target_users = iv_target_users users = rt_users ) INTO TABLE mt_target_cache.

  ENDMETHOD.

  METHOD get_dialog_users.
    " All dialog users (type A) that are not locked and still valid
    SELECT u~bname AS userid,
           a~name_textc AS user_name
      FROM usr02 AS u
      LEFT OUTER JOIN user_addr AS a
        ON a~bname = u~bname
      WHERE u~ustyp = 'A'
        AND u~uflag = 0
        AND ( u~gltgb = '00000000' OR u~gltgb >= @sy-datum )
      ORDER BY u~bname
      INTO CORRESPONDING FIELDS OF TABLE @rt_users.
  ENDMETHOD.

  METHOD to_date.
    " Convert a UTC timestamp to a date in the user's time zone
    IF iv_timestamp IS INITIAL.
      RETURN.
    ENDIF.

    CONVERT TIME STAMP iv_timestamp TIME ZONE sy-zonlo
      INTO DATE rv_date.
  ENDMETHOD.

  METHOD send_json.

    mo_server->response->set_status( code = iv_status reason = iv_reason ).
    mo_server->response->set_header_field( name = 'Content-Type' value = 'application/json' ).
    mo_server->response->set_cdata( iv_json ).

  ENDMETHOD.

  METHOD send_error.
    TYPES: BEGIN OF ty_error,
             error TYPE string,
           END OF ty_error.

    send_json( iv_status = iv_status
               iv_reason = iv_reason
               iv_json   = serialize_json( VALUE ty_error( error = iv_message ) ) ).

  ENDMETHOD.

  METHOD serialize_json.
    " Field names in lower case with underscores (e.g. compliance_pct).
    " No compression: FALSE/0 values must stay in the output because the
    " dashboard filters on acknowledged = true/false.
    rv_json = /ui2/cl_json=>serialize(
      data        = iv_data
      pretty_name = /ui2/cl_json=>pretty_mode-low_case ).
  ENDMETHOD.

ENDCLASS.
