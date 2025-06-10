*&---------------------------------------------------------------------*
*& Include          ZTEC_AJUSTES_INTEGRACAO_F01
*&---------------------------------------------------------------------*

FORM f_processo_cancelamento.

  PERFORM: f_get_data,
           f_set_data,
           f_cancel_remessa,
           f_change_ov,
           f_print_remessa.

ENDFORM.

FORM f_get_data.

  PERFORM: f_get_vbfa,
           f_get_vbap,
           f_get_vbuk.

ENDFORM.

FORM f_set_data.

  DATA: ls_ov TYPE ty_vbfa.

  IF gt_docven[] IS NOT INITIAL.

    LOOP AT gt_docven ASSIGNING FIELD-SYMBOL(<lf_doc>).

      ls_ov-vbelv = <lf_doc>-vbeln.
      ls_ov-posnv = <lf_doc>-posnr.

      APPEND ls_ov TO gt_ov.

    ENDLOOP.

  ENDIF.

ENDFORM.

FORM f_get_vbfa.

  IF s_can_re IS NOT INITIAL.

    SELECT
      vbelv
      posnv
      FROM vbfa
      INTO TABLE gt_ov
      WHERE vbeln IN s_can_re
        AND vbtyp_v = 'C'.

  ENDIF.

ENDFORM.

FORM f_get_vbap.

  IF s_can_ov IS NOT INITIAL.

    SELECT
      vbeln
      posnr
      FROM vbap
      INTO TABLE gt_docven
      WHERE vbeln IN s_can_ov.

  ENDIF.

ENDFORM.

FORM f_get_vbuk.

  IF s_can_re IS NOT INITIAL.

    SELECT
      vbeln,
      wbstk
      FROM v_vbuk_s4
      INTO TABLE @gt_remessa
      WHERE vbeln IN @s_can_re.

  ENDIF.

ENDFORM.

FORM f_cancel_remessa.

  DATA: ls_header         TYPE  bapiobdlvhdrchg,
        ls_header_control TYPE  bapiobdlvhdrctrlchg,
        lv_delivery       TYPE  bapiobdlvhdrchg-deliv_numb,
        lt_return         TYPE STANDARD TABLE OF  bapiret2.

  IF gt_remessa[] IS NOT INITIAL.

    SORT gt_remessa BY wbstk.

    LOOP AT gt_remessa ASSIGNING FIELD-SYMBOL(<lf_rem>)
      WHERE wbstk = 'A'.                                 "#EC CI_STDSEQ

      lv_delivery                  = <lf_rem>-vbeln.
      ls_header-deliv_numb         = <lf_rem>-vbeln.
      ls_header_control-deliv_numb = <lf_rem>-vbeln.
      ls_header_control-dlv_del    = 'X'.

      CALL FUNCTION 'BAPI_OUTB_DELIVERY_CHANGE'
        EXPORTING
          header_data    = ls_header
          header_control = ls_header_control
          delivery       = lv_delivery
        TABLES
          return         = lt_return.

      READ TABLE lt_return TRANSPORTING NO FIELDS
                           WITH KEY type = 'E'.          "#EC CI_STDSEQ

      IF sy-subrc = 0.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      ELSE.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
      ENDIF.

      CLEAR: lv_delivery,
             ls_header,
             ls_header_control,
             lt_return[].

    ENDLOOP.

  ENDIF.

ENDFORM.

FORM f_change_ov.

  DATA: lt_ov_aux         TYPE ty_t_vbfa,
        ls_orderheaderinx TYPE bapisdh1x,
        ls_orderheaderin  TYPE bapisdh1,
        lt_return         TYPE STANDARD TABLE OF bapiret2,
        lt_order_item_in  TYPE STANDARD TABLE OF bapisditm,
        lt_order_item_inx TYPE STANDARD TABLE OF bapisditmx.

  lt_ov_aux[] = gt_ov[].

  SORT lt_ov_aux BY vbelv.

  DELETE ADJACENT DUPLICATES FROM lt_ov_aux COMPARING vbelv.

  IF lt_ov_aux[] IS NOT INITIAL.

    LOOP AT lt_ov_aux ASSIGNING FIELD-SYMBOL(<lf_aux>).

      PERFORM f_prepar_item USING <lf_aux>-vbelv
                         CHANGING lt_order_item_in
                                  lt_order_item_inx.

      ls_orderheaderin-dlv_block  = '50'.
      ls_orderheaderinx-updateflag = 'U' .
      ls_orderheaderinx-dlv_block = 'X'.

      CALL FUNCTION 'BAPI_SALESORDER_CHANGE'
        EXPORTING
          salesdocument    = <lf_aux>-vbelv
          order_header_in  = ls_orderheaderin
          order_header_inx = ls_orderheaderinx
        TABLES
          return           = lt_return
          order_item_in    = lt_order_item_in
          order_item_inx   = lt_order_item_inx.

      READ TABLE lt_return TRANSPORTING NO FIELDS
                           WITH KEY type = 'E'.          "#EC CI_STDSEQ

      IF sy-subrc = 0.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      ELSE.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.

        PERFORM f_updat_z_table USING <lf_aux>-vbelv.

      ENDIF.

      CLEAR: ls_orderheaderin,
             ls_orderheaderinx,
             lt_return[],
             lt_order_item_in[],
             lt_order_item_inx[].

    ENDLOOP.

  ENDIF.

ENDFORM.

FORM f_prepar_item USING    iv_vbeln          TYPE vbap-vbeln
                   CHANGING ct_ordem_item_in  TYPE ty_t_ordem_item_in
                            ct_ordem_item_inx TYPE ty_t_ordem_item_inx.

  DATA: ls_order_item_in  TYPE bapisditm,
        ls_order_item_inx TYPE bapisditmx.

  SORT gt_ov BY vbelv posnv.

  DELETE ADJACENT DUPLICATES FROM gt_ov COMPARING vbelv posnv.

  LOOP AT gt_ov ASSIGNING FIELD-SYMBOL(<lf_ov>)
    WHERE vbelv = iv_vbeln.                              "#EC CI_STDSEQ

    ls_order_item_in-itm_number = <lf_ov>-posnv.
    ls_order_item_in-reason_rej = 'Z0'.

    ls_order_item_inx-itm_number = <lf_ov>-posnv.
    ls_order_item_inx-updateflag = 'U'.
    ls_order_item_inx-reason_rej = 'X'.

    APPEND ls_order_item_in TO ct_ordem_item_in.
    APPEND ls_order_item_inx TO ct_ordem_item_inx.

    CLEAR: ls_order_item_in,
           ls_order_item_inx.

  ENDLOOP.

ENDFORM.

FORM f_updat_z_table USING iv_vbeln TYPE vbap-vbeln.

  IF iv_vbeln IS NOT INITIAL.
    UPDATE zz1_11cdff4448f3 SET devol_cancel = abap_true WHERE ordemvenda = iv_vbeln. "#EC CI_NOFIRST

    IF sy-subrc EQ 0.
      COMMIT WORK.
    ENDIF.
  ENDIF.

ENDFORM.

FORM f_print_remessa.

  DATA ls_report TYPE ty_report.

  LOOP AT gt_remessa ASSIGNING FIELD-SYMBOL(<lf_re>)
      WHERE wbstk <> 'A'.                                "#EC CI_STDSEQ

    ls_report-vbeln = <lf_re>-vbeln.
    ls_report-pdtxt = TEXT-007.

    APPEND ls_report TO gt_report.

  ENDLOOP.

  IF gt_report[] IS NOT INITIAL.
    PERFORM f_exibir_dados.
  ELSE.
    MESSAGE TEXT-011 TYPE 'S'.
  ENDIF.

ENDFORM.

FORM f_exibir_dados .
  IF gt_report[] IS NOT INITIAL.

    PERFORM: f_inicializa_alv,
             f_setar_layout_alv,
             f_setar_colunas_alv.

    go_salv->display( ).

  ENDIF.

ENDFORM.

FORM f_inicializa_alv.
  TRY.
      cl_salv_table=>factory(
        IMPORTING
          r_salv_table  = go_salv
        CHANGING
          t_table       = gt_report ).
    CATCH cx_salv_msg INTO gx_message.

  ENDTRY.
ENDFORM.

FORM f_setar_layout_alv.

  go_layout_settings   = go_salv->get_layout( ).
  gs_layout_key-report = sy-repid.
  go_layout_settings->set_key( gs_layout_key ).
  go_layout_settings->set_save_restriction( if_salv_c_layout=>restrict_none ).

  go_display_settings = go_salv->get_display_settings( ).
  go_display_settings->set_striped_pattern( if_salv_c_bool_sap=>true ).

ENDFORM.

FORM f_setar_colunas_alv.

  go_columns = go_salv->get_columns( ).
  go_columns->set_optimize( ).

  TRY.

      go_column = go_columns->get_column( columnname = 'VBELN' ).
      go_column->set_long_text( value   = TEXT-009 ).
      go_column->set_medium_text( value = TEXT-009 ).
      go_column->set_short_text( value  = TEXT-009 ).

      go_column = go_columns->get_column( columnname = 'PDTXT' ).
      go_column->set_long_text( value   = TEXT-010 ).
      go_column->set_medium_text( value = TEXT-010 ).
      go_column->set_short_text( value  = TEXT-010 ).

    CATCH cx_salv_not_found .

  ENDTRY.

ENDFORM.
