*&---------------------------------------------------------------------*
*& Report ZTEC_AJUSTES_INTEGRACAO
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ztec_ajustes_integracao.

TABLES: likp,
        vbak,
        dfkkbptaxnum,
        j_1bnflin,
        ztbc_repro_ov.

INCLUDE ztec_ajustes_integracao_top.

* Tela de seleção
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  PARAMETERS: p_rem   AS CHECKBOX,
              p_rem2  AS CHECKBOX,
              p_rem3  AS CHECKBOX,
              p_rem4  AS CHECKBOX,
              p_deliv TYPE flag.

  SELECT-OPTIONS: s_vbeln FOR likp-vbeln .

SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
  SELECT-OPTIONS: s_ov FOR vbak-vbeln .
  PARAMETERS : p_pgto AS CHECKBOX  .
  PARAMETERS : p_zd AS CHECKBOX  .
  PARAMETERS : p_sp AS CHECKBOX  .
  PARAMETERS : p_bpzd TYPE kunag.
*  PARAMETERS : p_bpSP TYPE kunag.
  PARAMETERS:  p_semrem AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK b2.


SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-003.
  SELECT-OPTIONS: s_bp FOR dfkkbptaxnum-partner .
  PARAMETERS : p_bp AS CHECKBOX.
  PARAMETERS : p_dom AS CHECKBOX.

SELECTION-SCREEN END OF BLOCK b3.



SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE TEXT-004.

  PARAMETERS: p_file TYPE rlgrap-filename.
  PARAMETERS : p_carga  TYPE flag  .
  PARAMETERS : p_peso  TYPE flag  .
  PARAMETERS : p_div TYPE flag.
  PARAMETERS : p_vbep TYPE flag.



SELECTION-SCREEN END OF BLOCK b4.



SELECTION-SCREEN BEGIN OF BLOCK b5 WITH FRAME TITLE TEXT-005.
  SELECT-OPTIONS: s_docnum FOR j_1bnflin-docnum.
  PARAMETERS: p_megapr TYPE c RADIOBUTTON GROUP b1,
              p_ecomm  TYPE c RADIOBUTTON GROUP b1.
SELECTION-SCREEN END OF BLOCK b5.

INCLUDE ztec_ajustes_integracao_scr.
INCLUDE ztec_ajustes_integracao_f01.

* Ajuda de pesquisa para seleção do arquivo
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  PERFORM f_file_dialog.

START-OF-SELECTION.
  DATA: lv_timestamp TYPE tzntstmps,
        lv_date      TYPE sy-datlo,
        lv_time      TYPE sy-timlo.


  TYPES: BEGIN OF ty_line,
           line(500) TYPE c,
         END OF ty_line.


  DATA: v_filename TYPE string.
  v_filename = p_file.
  DATA: it_file  TYPE TABLE OF ty_line.



  DATA: v_vbeln LIKE vbap-vbeln.

  DATA: orderheaderinx LIKE bapisdh1x.
  DATA: orderheaderin LIKE bapisdh1.
  DATA: partner LIKE bapiparnrc.
  DATA: partners TYPE TABLE OF bapiparnrc.
  DATA: bapisdpart TYPE TABLE OF bapisdpart.

  DATA: BEGIN OF it_extensionin OCCURS 0.
          INCLUDE STRUCTURE bapiparex.
  DATA: END OF it_extensionin.

  DATA: BEGIN OF return OCCURS 0.
          INCLUDE STRUCTURE bapiret2.
  DATA: END OF return.

  DATA: BEGIN OF bapireturn OCCURS 0.
          INCLUDE STRUCTURE bapiret2.
  DATA: END OF bapireturn.

  DATA: l_zavbak  TYPE bape_vbak,
        l_zavbakx TYPE bape_vbakx.

  IF p_cancel = 'X'.
    PERFORM f_processo_cancelamento.

  ELSE.

*  IF sy-uname(6) NE 'RAFAEL'.
*    MESSAGE 'Você não possui permissão para executar' TYPE 'S' DISPLAY LIKE 'E'.
*    EXIT.
*  ENDIF.

    IF p_rem2 IS NOT INITIAL.
      SELECT vbeln , faksk                         "#EC CI_NO_TRANSFORM
           FROM likp
           INTO TABLE @DATA(lt_likp)
           WHERE vbeln IN  @s_vbeln AND
                 faksk EQ 'TR'.

      LOOP AT  lt_likp ASSIGNING FIELD-SYMBOL(<fs_likp>).
        UPDATE likp SET faksk = '' WHERE vbeln = <fs_likp>-vbeln.
      ENDLOOP.
    ENDIF.

    IF s_docnum[] IS NOT INITIAL.

      IF p_megapr IS NOT INITIAL.
        PERFORM zf_nfe_rabbit.
      ENDIF.

      IF p_ecomm IS NOT INITIAL.
        PERFORM zf_nfe_oms_ecomm.
      ENDIF.

    ENDIF.

    IF p_rem IS NOT INITIAL AND s_vbeln[] IS NOT INITIAL.
      SELECT vbeln , faksk                         "#EC CI_NO_TRANSFORM
           FROM likp
           INTO TABLE @DATA(lt_likp_ax)
           WHERE vbeln IN  @s_vbeln.

      LOOP AT  lt_likp_ax ASSIGNING FIELD-SYMBOL(<fs_likp_ax>).

        CALL FUNCTION 'ABI_TIMESTAMP_CONVERT_INTO'
          EXPORTING
            iv_date      = sy-datum
            iv_time      = sy-uzeit
          IMPORTING
            ev_timestamp = lv_timestamp.


        UPDATE likp SET zz1_wmsreadtimestamp_dlh = lv_timestamp WHERE vbeln = <fs_likp_ax>-vbeln.
      ENDLOOP.
    ENDIF.


    IF p_rem3 IS NOT INITIAL AND s_vbeln[] IS NOT INITIAL.

      SELECT vbeln , faksk                         "#EC CI_NO_TRANSFORM
           FROM likp
           INTO TABLE @DATA(lt_likp_ax1)
           WHERE vbeln IN  @s_vbeln.

      LOOP AT lt_likp_ax1 ASSIGNING FIELD-SYMBOL(<fs_likp_ax1>).
        UPDATE likp SET zz1_wmsreadtimestamp_dlh = '' WHERE vbeln = <fs_likp_ax1>-vbeln.
      ENDLOOP.

    ENDIF.


    IF p_pgto IS NOT INITIAL AND s_ov[] IS NOT INITIAL.

      SELECT *                                     "#EC CI_NO_TRANSFORM
           FROM vbak
           INTO TABLE @DATA(lt_vbak)
           WHERE vbeln IN  @s_ov.

      IF sy-subrc EQ 0.
        LOOP AT lt_vbak ASSIGNING FIELD-SYMBOL(<fs_vbak>).


          v_vbeln = <fs_vbak>-vbeln.
          orderheaderinx-updateflag = 'U'.
          orderheaderinx-pmnttrms = 'X'.
          orderheaderin-pmnttrms = 'A045'.

          CALL FUNCTION 'BAPI_SALESORDER_CHANGE'
            EXPORTING
              salesdocument     = v_vbeln
              order_header_in   = orderheaderin
              order_header_inx  = orderheaderinx
              behave_when_error = 'P'
            TABLES
              return            = return
              extensionin       = it_extensionin.

          IF sy-subrc EQ 0.
            CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
          ELSE.
            CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
          ENDIF.

        ENDLOOP.
      ENDIF.


    ENDIF.

    IF p_zd IS NOT INITIAL AND s_ov[] IS NOT INITIAL .



      SELECT *                                     "#EC CI_NO_TRANSFORM
           FROM vbak
           INTO TABLE @DATA(lt_vbak_zd)
           WHERE vbeln IN  @s_ov.

      IF sy-subrc EQ 0.
        LOOP AT lt_vbak_zd ASSIGNING FIELD-SYMBOL(<fs_vbak_zd>).
          REFRESH: bapisdpart[],
                   partners[],
                   return[].

          CALL FUNCTION 'BAPI_SALESORDER_GETDETAILBOS'
            EXPORTING
              salesdocument = <fs_vbak_zd>-vbeln
            TABLES
              orderpartners = bapisdpart.

          READ TABLE bapisdpart ASSIGNING FIELD-SYMBOL(<fs_partner>) WITH  KEY partn_role = 'ZD'.

          orderheaderinx-updateflag = 'U'.
          partner-updateflag = 'U'.
          partner-partn_role = 'ZD'.
          partner-document = <fs_partner>-sd_doc.
          partner-p_numb_new = p_bpzd.
          APPEND partner TO partners.

          partner-updateflag = 'D'.
          partner-document = <fs_partner>-sd_doc.
          partner-partn_role = 'ZI'.
          partner-p_numb_new = '0000000000'.
          partner-p_numb_old = '0000000000'.
          partner-document = <fs_partner>-sd_doc.

          APPEND partner TO partners.


          CALL FUNCTION 'BAPI_SALESORDER_CHANGE'
            EXPORTING
              salesdocument     = <fs_vbak_zd>-vbeln
              order_header_in   = orderheaderin
              order_header_inx  = orderheaderinx
              behave_when_error = 'P'
            TABLES
              return            = return
              partnerchanges    = partners
              extensionin       = it_extensionin.

          IF sy-subrc EQ 0.
            CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
          ELSE.
            CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
          ENDIF.

        ENDLOOP.
      ENDIF.


    ENDIF.



    IF p_sp IS NOT INITIAL AND s_ov[] IS NOT INITIAL .



      SELECT *                                     "#EC CI_NO_TRANSFORM
           FROM vbak
           INTO TABLE @DATA(lt_vbak_sp)
           WHERE vbeln IN  @s_ov.

      IF sy-subrc EQ 0.
        LOOP AT lt_vbak_sp ASSIGNING FIELD-SYMBOL(<fs_vbak_sp>).
          REFRESH: bapisdpart[],
                   partners[],
                   return[].

          CALL FUNCTION 'BAPI_SALESORDER_GETDETAILBOS'
            EXPORTING
              salesdocument = <fs_vbak_sp>-vbeln
            TABLES
              orderpartners = bapisdpart.

          READ TABLE bapisdpart ASSIGNING FIELD-SYMBOL(<fs_partner_sp>) WITH  KEY partn_role = 'SP'.

          orderheaderinx-updateflag = 'U'.
          partner-updateflag = 'U'.
          partner-partn_role = 'SP'.
          partner-document = <fs_partner_sp>-sd_doc.
          partner-p_numb_new = p_bpzd.
          APPEND partner TO partners.

*          partner-updateflag = 'D'.
*          partner-document = <fs_partner>-sd_doc.
*          partner-partn_role = 'ZI'.
*          partner-p_numb_new = '0000000000'.
*          partner-P_NUMB_old = '0000000000'.
*          partner-document = <fs_partner>-sd_doc.

          APPEND partner TO partners.


          CALL FUNCTION 'BAPI_SALESORDER_CHANGE'
            EXPORTING
              salesdocument     = <fs_vbak_sp>-vbeln
              order_header_in   = orderheaderin
              order_header_inx  = orderheaderinx
              behave_when_error = 'P'
            TABLES
              return            = return
              partnerchanges    = partners
              extensionin       = it_extensionin.

          IF sy-subrc EQ 0.
            CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
          ELSE.
            CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
          ENDIF.

        ENDLOOP.
      ENDIF.


    ENDIF.


    IF p_bp IS NOT INITIAL AND s_bp IS NOT INITIAL.

      DATA: bp TYPE bapibus1006_head-bpartner.

      DATA : wa_central   TYPE  bapibus1006_central,
             wa_central_x TYPE   bapibus1006_central_x.

      DATA: git_return_bp TYPE  TABLE OF bapiret2.


      SELECT *                                     "#EC CI_NO_TRANSFORM
           FROM dfkkbptaxnum
           INTO TABLE @DATA(lt_dfkkbptaxnum)
           WHERE partner IN  @s_bp.

      LOOP AT lt_dfkkbptaxnum ASSIGNING FIELD-SYMBOL(<fs_dfkkbptaxnum>).
        bp = <fs_dfkkbptaxnum>-partner.
        CLEAR: wa_central, wa_central_x.

        wa_central-centralarchivingflag       = 'X'.
        wa_central_x-centralarchivingflag     = 'X'.


        CALL FUNCTION 'BAPI_BUPA_CENTRAL_CHANGE'
          EXPORTING
            businesspartner = bp
            centraldata     = wa_central
            centraldata_x   = wa_central_x
          TABLES
            return          = git_return_bp.

        IF sy-subrc EQ 0.
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = 'X'.
          UPDATE dfkkbptaxnum SET taxnum = '' WHERE partner = <fs_dfkkbptaxnum>-partner.
        ELSE.
          CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
        ENDIF.



      ENDLOOP.


    ENDIF.


    IF p_dom IS NOT INITIAL AND s_bp[] IS NOT INITIAL.


      DATA: endereco      TYPE bapibus1006_address,
            enderecodata  TYPE bapibus1006_address,
            enderecodatax TYPE bapibus1006_address_x.

      DATA: ls_location  TYPE com_jur,
            lt_locations TYPE TABLE OF com_jur.


      SELECT *                                     "#EC CI_NO_TRANSFORM
           FROM but000
           INTO TABLE @DATA(lt_but000)
           WHERE partner IN  @s_bp.

      LOOP AT lt_but000 ASSIGNING FIELD-SYMBOL(<fs_but000>).
        CLEAR: endereco, ls_location, lt_locations[] , enderecodata , enderecodatax.

        CALL FUNCTION 'BAPI_BUPA_ADDRESS_GETDETAIL'
          EXPORTING
            businesspartner = <fs_but000>-partner
          IMPORTING
            addressdata     = endereco.



*    READ TABLE endereco ASSIGNING FIELD-SYMBOL(<fs_endereco>) INDEX 1.
        IF sy-subrc EQ 0.
          ls_location-zipcode = endereco-postl_cod1.
          ls_location-city    = endereco-city.
          ls_location-country = endereco-country.
          ls_location-state   = endereco-region.


          CALL FUNCTION 'J_1BTAXJUR_DETERMINE_NEW'
            EXPORTING
              location_data    = ls_location
            TABLES
              location_results = lt_locations.

          READ TABLE lt_locations INTO ls_location INDEX 1.

          enderecodata-taxjurcode = ls_location-txjcd.
          enderecodatax-taxjurcode  = 'X'.



          CALL FUNCTION 'BAPI_BUPA_ADDRESS_CHANGE'
            EXPORTING
              businesspartner = <fs_but000>-partner
              addressdata     = enderecodata
              addressdata_x   = enderecodatax.


          IF sy-subrc EQ 0.
            CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
              EXPORTING
                wait = 'X'.
          ELSE.
            CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
          ENDIF.


        ENDIF.

      ENDLOOP.



    ENDIF.

    IF p_deliv IS  NOT INITIAL.

      DATA:
        lo_client        TYPE REF TO if_http_client,
        lv_res_data_bin  TYPE xstring,
        lv_res_data_str  TYPE string,
        lv_req_data_bin  TYPE xstring,
        lv_req_data_str1 TYPE string,
        lv_req_data_str2 TYPE string,
        lv_req_data_str3 TYPE string,
        lv_req_data_str4 TYPE string,
        lo_conv          TYPE REF TO cl_abap_conv_in_ce.
*        lv_company       TYPE butxt.

      DATA: lx_root                    TYPE REF TO cx_root.


      SELECT k~vbeln , k~faksk ,   k~vstel , s~vgbel , s~posnr "#EC CI_NO_TRANSFORM
           FROM likp AS k
           INNER JOIN lips AS s
           ON k~vbeln = s~vbeln
           INTO TABLE @DATA(lt_likp_x)
           WHERE k~vbeln IN  @s_vbeln .
      "AND s~posnr EQ '000010'.

      DELETE lt_likp_x WHERE faksk EQ 'TR'.
      DELETE ADJACENT DUPLICATES FROM lt_likp_x COMPARING vbeln.

      IF sy-subrc EQ 0.
        SELECT vbeln, bstnk FROM vbak
          INTO TABLE  @DATA(lt_vbak_a)
          FOR ALL ENTRIES IN @lt_likp_x
          WHERE vbeln EQ @lt_likp_x-vgbel.
      ENDIF.


*{
*"BSTNK":  "104312402", - ok
*"VBELN":  "0080590247", -ok
*"VGBEL":  "0000590409", - ok
*"VSTEL":  "L001" -ok
*}

      LOOP AT lt_likp_x ASSIGNING FIELD-SYMBOL(<fs_likp_x>).
        READ TABLE  lt_vbak_a ASSIGNING FIELD-SYMBOL(<fss>) WITH KEY vbeln = <fs_likp_x>-vgbel.

        CONCATENATE '{  "BSTNK": "' <fss>-bstnk '",  "VBELN":  "' <fs_likp_x>-vbeln '",  "VGBEL" : "' <fs_likp_x>-vgbel '", "VSTEL" : "'  <fs_likp_x>-vstel '"   '  '}' INTO   lv_req_data_str4 .

        cl_http_client=>create_by_destination(
            EXPORTING
              destination              = 'SAPINTSUITE_DELIVERY_MANUAL'
            IMPORTING
              client                   = lo_client
            EXCEPTIONS
              argument_not_found       = 1
              destination_not_found    = 2
              destination_no_authority = 3
              plugin_not_active        = 4
              internal_error           = 5
              OTHERS                   = 6
        ).

        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.
        lo_client->request->set_method( if_http_request=>co_request_method_post ).



        TRY.
            cl_bcs_convert=>string_to_xstring(
              EXPORTING
                iv_string     = lv_req_data_str4
              RECEIVING
                ev_xstring    = lv_req_data_bin
            ).
          CATCH cx_bcs cx_j_1bnfe_cf cx_j_1bnfe_exception cx_j1bnfe_exception INTO lx_root.
            DATA lo_message_texts TYPE REF TO if_t100_message.

        ENDTRY.


        lo_client->request->set_data(
          EXPORTING
            data               = lv_req_data_bin
        ).

        lo_client->send( ).
        lo_client->receive( ).

        lv_res_data_bin = lo_client->response->get_data( ).

        lo_conv = cl_abap_conv_in_ce=>create( input = lv_res_data_bin ).
        lo_conv->read( IMPORTING data = lv_req_data_str4 ).

        lo_client->close( ).


      ENDLOOP.


    ENDIF.

    IF p_semrem IS NOT INITIAL AND s_ov[] IS NOT INITIAL .

      DATA: wa_header  TYPE bapisdh1,
            wa_headerx TYPE bapisdh1x.


      SELECT *                                     "#EC CI_NO_TRANSFORM
           FROM vbak
           INTO TABLE @DATA(lt_vbak_we)
           WHERE vbeln IN  @s_ov.



      SELECT   zc~pedidosite,
               zc~ordemvenda,
               zi~condicaopagamento,
               zi~identificadorpagamento

          FROM zz1_11cdff4448f3 AS zc
         INNER JOIN zz1_cbc265388ba2 AS zi ON zc~sap_uuid = zi~sap_parent_uuid
        FOR ALL ENTRIES IN @lt_vbak_we
         WHERE ordemvenda EQ @lt_vbak_we-vbeln
         INTO TABLE @DATA(lt_bo_itens).


      IF sy-subrc EQ 0.
        LOOP AT lt_vbak_we ASSIGNING FIELD-SYMBOL(<fs_vbak_we>).

          REFRESH: bapisdpart[],
                   partners[],
                   return[].

          CLEAR: wa_header, wa_headerx.

          CALL FUNCTION 'BAPI_SALESORDER_GETDETAILBOS'
            EXPORTING
              salesdocument = <fs_vbak_we>-vbeln
            TABLES
              orderpartners = bapisdpart.

          READ TABLE bapisdpart ASSIGNING FIELD-SYMBOL(<fs_partner_we>) WITH  KEY partn_role = 'WE'.

          orderheaderinx-updateflag = 'U'.
          partner-updateflag = 'U'.
          partner-partn_role = 'RG'.
          partner-document = <fs_partner_we>-sd_doc.
          partner-p_numb_new = <fs_partner_we>-customer.
          APPEND partner TO partners.

          READ TABLE bapisdpart ASSIGNING FIELD-SYMBOL(<fs_partner_zi>) WITH  KEY partn_role = 'ZI'.
          partner-updateflag = 'D'.
          partner-document = <fs_partner_we>-sd_doc.
          partner-partn_role = 'ZI'.
          partner-p_numb_new = '0000000000'.
          partner-p_numb_old = <fs_partner_zi>-customer.
          partner-document = <fs_partner_we>-sd_doc.

          APPEND partner TO partners.

          READ TABLE lt_bo_itens ASSIGNING FIELD-SYMBOL(<fs_docz>) WITH KEY ordemvenda = <fs_vbak_we>-vbeln.
          IF sy-subrc EQ 0.
            wa_header-pmnttrms    = <fs_docz>-condicaopagamento..
            wa_headerx-pmnttrms   = 'X'.
            wa_headerx-updateflag = 'U'.
          ENDIF.



          CALL FUNCTION 'BAPI_SALESORDER_CHANGE'
            EXPORTING
              order_header_in   = wa_header
              order_header_inx  = wa_headerx
              salesdocument     = <fs_vbak_we>-vbeln
              behave_when_error = 'P'
            TABLES
              return            = return
              partnerchanges    = partners
              extensionin       = it_extensionin.

          IF sy-subrc EQ 0.
            CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
          ELSE.
            CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
          ENDIF.

        ENDLOOP.
      ENDIF.


    ENDIF.

    IF p_carga IS NOT INITIAL.

      CONSTANTS: con TYPE c VALUE ';'.

      TYPES: BEGIN OF ty_vbap_aux,
               vbeln TYPE vbap-vbeln,
               posnr TYPE vbap-posnr,
             END OF ty_vbap_aux.

      DATA:  wa_arq_aux      TYPE ztbc_repro_ov. " ty_vbap_aux.
      DATA: it_saida_aux  TYPE STANDARD TABLE OF  ty_vbap_aux.

      DELETE FROM ztbc_repro_ov.
      IF sy-subrc EQ 0  .
        COMMIT WORK AND WAIT.
      ENDIF.


      CALL FUNCTION 'GUI_UPLOAD'
        EXPORTING
          filename = v_filename
          filetype = 'ASC'
        TABLES
          data_tab = it_file.

      LOOP AT it_file ASSIGNING FIELD-SYMBOL(<fs_file_aux>).
        SPLIT <fs_file_aux> AT con INTO wa_arq_aux-vbeln
                                        wa_arq_aux-posnr.

        MODIFY ztbc_repro_ov FROM   wa_arq_aux .
        IF sy-subrc EQ 0.
          COMMIT WORK AND WAIT.
        ENDIF.

      ENDLOOP.


    ENDIF.

    IF p_vbep IS NOT INITIAL.

      SELECT *                                     "#EC CI_NO_TRANSFORM
        FROM ztbc_repro_ov
        INTO TABLE @DATA(it_repro_vbep).

      SELECT  vbeln,
                 posnr,
                 etenr,
                 wmeng                             "#EC CI_NO_TRANSFORM
           FROM vbep
           INTO TABLE @DATA(it_vbep_up)
           FOR ALL ENTRIES IN @it_repro_vbep
           WHERE vbeln EQ @it_repro_vbep-vbeln AND
                 posnr EQ @it_repro_vbep-posnr.


      LOOP AT it_repro_vbep ASSIGNING FIELD-SYMBOL(<fs_repro_vbep>).
        READ TABLE  it_vbep_up ASSIGNING FIELD-SYMBOL(<fs_up>) WITH KEY vbeln =  <fs_repro_vbep>-vbeln
                                                                        posnr =  <fs_repro_vbep>-posnr
                                                                        etenr  = '0001'.
        IF sy-subrc EQ 0.
          UPDATE vbep SET bmeng  = <fs_up>-wmeng
                          lmeng  = <fs_up>-wmeng
                       WHERE vbeln = <fs_repro_vbep>-vbeln  AND
                                           posnr =  <fs_repro_vbep>-posnr AND
                                           etenr  = '0001'.
          IF sy-subrc EQ 0.
            COMMIT WORK AND WAIT .
          ENDIF.
        ENDIF.


      ENDLOOP.




    ENDIF.

    IF p_div IS NOT INITIAL.

      DATA: lt_schdl          TYPE TABLE OF bapischdl WITH HEADER LINE,
            ls_ord_header_inx TYPE bapisdh1x,
            ls_return         LIKE bapiret2,
            lt_return         TYPE TABLE OF bapiret2 WITH HEADER LINE,
            lt_schdlx         TYPE TABLE OF bapischdlx WITH HEADER LINE.


      SELECT *                                     "#EC CI_NO_TRANSFORM
           FROM ztbc_repro_ov
           INTO TABLE @DATA(it_repro_div).


      SELECT  vbeln,
              posnr,
              etenr,
              wmeng                                "#EC CI_NO_TRANSFORM
        FROM vbep
        INTO TABLE @DATA(it_vbep)
        FOR ALL ENTRIES IN @it_repro_div
        WHERE vbeln EQ @it_repro_div-vbeln AND
              posnr EQ @it_repro_div-posnr.

      SORT: it_vbep[] BY vbeln  posnr,
            it_repro_div[] BY vbeln posnr.



      LOOP AT it_repro_div ASSIGNING FIELD-SYMBOL(<fs_repro_div>).
        REFRESH: lt_schdl[], lt_schdlx[].
        CLEAR:  lt_schdl, lt_schdlx.

        READ TABLE it_vbep ASSIGNING FIELD-SYMBOL(<fs_vbep>) WITH KEY vbeln = <fs_repro_div>-vbeln
                                                                      posnr = <fs_repro_div>-posnr BINARY SEARCH.

        IF sy-subrc EQ 0.

          lt_schdl-req_date   = sy-datum.
          lt_schdl-itm_number = <fs_vbep>-posnr.
          lt_schdl-sched_line = <fs_vbep>-etenr.
          lt_schdl-gi_date    = sy-datum.
          lt_schdl-req_qty    = <fs_vbep>-wmeng.
          lt_schdl-load_date  = sy-datum.
          lt_schdl-ms_date    = sy-datum.
          lt_schdl-tp_date    = sy-datum.
          APPEND lt_schdl.

          ls_ord_header_inx-updateflag = 'U'.

          lt_schdlx-updateflag = 'U'.
          lt_schdlx-itm_number = <fs_vbep>-posnr.. "vbep-posnr.
          lt_schdlx-sched_line = <fs_vbep>-etenr.
          lt_schdlx-req_date   = 'X'.
          lt_schdlx-gi_date    = 'X'.
          lt_schdlx-req_qty    = 'X'.
          lt_schdlx-load_date  = 'X'.
          lt_schdlx-ms_date    = 'X'.
          lt_schdlx-tp_date    = 'X'.
          APPEND lt_schdlx.


          CALL FUNCTION 'BAPI_SALESORDER_CHANGE'
            EXPORTING
              salesdocument    = <fs_vbep>-vbeln
              order_header_inx = ls_ord_header_inx
            TABLES
              return           = lt_return
              schedule_lines   = lt_schdl
              schedule_linesx  = lt_schdlx.

          IF sy-subrc EQ 0.
            COMMIT WORK AND WAIT.
          ELSE.
            ROLLBACK WORK.
          ENDIF.

        ENDIF.
      ENDLOOP.

    ENDIF.


    IF p_rem4 IS NOT INITIAL AND s_vbeln[] IS NOT INITIAL.

*    DATA: it_header_data TYPE TABLE OF bapiobdlvhdrchg,
*          wa_header_data TYPE bapiobdlvhdrchg.
*    DATA: it_header_control TYPE TABLE OF  bapiobdlvhdrctrlchg,
*          wa_header_control TYPE bapiobdlvhdrctrlchg.
*    DATA: it_return  TYPE TABLE OF  bapiret2.
*    DATA: it_header_deadlines TYPE TABLE OF  bapidlvdeadln,
*          wa_header_deadlines TYPE bapidlvdeadln.
*    DATA: l_delivery TYPE vbeln_vl.

      DATA: lv_number          TYPE tbtcjob-jobcount,
            lv_name            TYPE tbtcjob-jobname,
            print_parameters   TYPE pri_params,
            archive_parameters TYPE arc_params.

      DATA: BEGIN OF starttime.
              INCLUDE TYPE tbtcstrt.
      DATA: END OF starttime.
      DATA: starttimeimmediate TYPE  btch0000-char1.



      SELECT objky                                 "#EC CI_NO_TRANSFORM
           FROM nast
           INTO TABLE @DATA(lt_nast)
           WHERE objky IN  @s_vbeln AND
                 kschl EQ 'EXPD' AND
                 nacha EQ 'A'.

      LOOP AT lt_nast ASSIGNING FIELD-SYMBOL(<fs_nast>).
        UPDATE nast SET vsztp = 1 , vstat = 0 WHERE objky = @<fs_nast>-objky.
      ENDLOOP.



      LOOP AT lt_nast ASSIGNING FIELD-SYMBOL(<fs_nast_ax>)..

        CONCATENATE 'JOB_REMESSA_' <fs_nast_ax>-objky INTO lv_name.
        CALL FUNCTION 'JOB_OPEN'
          EXPORTING
            jobname          = lv_name
            sdlstrtdt        = sy-datum
            sdlstrttm        = sy-uzeit
          IMPORTING
            jobcount         = lv_number
          EXCEPTIONS
            cant_create_job  = 1
            invalid_job_data = 2
            jobname_missing  = 3
            OTHERS           = 4.

        IF sy-subrc EQ 0.
          SUBMIT rsnast00
          VIA JOB lv_name NUMBER lv_number
             WITH s_kappl EQ 'V2'
             WITH s_objky EQ <fs_nast_ax>-objky
             WITH s_kschl EQ 'EXPD'
             WITH s_nacha EQ 'A' AND RETURN.
          IF sy-subrc EQ 0.

            starttime-sdlstrtdt = sy-datum.
            starttime-sdlstrttm = sy-uzeit + 20.
            CALL FUNCTION 'JOB_CLOSE'
              EXPORTING
                jobcount             = lv_number
                jobname              = lv_name
                laststrtdt           = starttime-laststrtdt
                laststrttm           = starttime-laststrttm
                sdlstrtdt            = starttime-sdlstrtdt
                sdlstrttm            = starttime-sdlstrttm
              EXCEPTIONS
                cant_start_immediate = 1
                invalid_startdate    = 2
                jobname_missing      = 3
                job_close_failed     = 4
                job_nosteps          = 5
                job_notex            = 6
                lock_failed          = 7
                OTHERS               = 8.


          ENDIF.

        ENDIF.


      ENDLOOP.


*  WITH s_kappl EQ 'V2'
*          WITH s_objky EQ lv_deliv
*          WITH s_kschl EQ 'EXPD'
*          WITH s_nacha EQ 'A'

*    LOOP AT lt_likp_exp ASSIGNING FIELD-SYMBOL(<fs_exp>).
*
*
*
*
*      wa_header_data-deliv_numb = <fs_exp>-vbeln.
*      APPEND wa_header_data TO it_header_data.
*
*
*      wa_header_control-deliv_numb =  <fs_exp>-vbeln.
*      wa_header_control-gdsi_date_flg = 'X'.
*
*      wa_header_deadlines-deliv_numb = <fs_exp>-vbeln.
*      wa_header_deadlines-timetype = 'WSHDRWADAT'.
*      wa_header_deadlines-timestamp_utc = '20180308'.
*
*      APPEND wa_header_deadlines TO it_header_deadlines.
*      l_delivery = <fs_exp>-vbeln.
*
*      CALL FUNCTION 'BAPI_OUTB_DELIVERY_CHANGE'
*        EXPORTING
*          header_data      = wa_header_data
*          header_control   = wa_header_control
*          delivery         = l_delivery
*        TABLES
*          header_deadlines = it_header_deadlines
*          return           = it_return.
*
*      IF sy-subrc EQ 0.
*        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
*      ENDIF.
*
*    ENDLOOP.

    ENDIF.



    IF p_peso IS NOT INITIAL.
      TYPES: BEGIN OF ty_vbap,
               vbeln TYPE vbap-vbeln,
               posnr TYPE vbap-posnr,
             END OF ty_vbap.

      DATA:  wa_arq      TYPE ty_vbap.
      DATA: it_saida  TYPE STANDARD TABLE OF  ty_vbap.

      DATA: lwa_order_items     TYPE bapisditm,
            lwa_order_items_inx TYPE bapisditmx.


      DATA: li_order_items     TYPE TABLE OF bapisditm,
            li_order_items_inx TYPE TABLE OF bapisditmx.

      DATA: wa_saida LIKE LINE OF it_saida.

      CONSTANTS: con_tab TYPE c VALUE ';'.


      SELECT *                                     "#EC CI_NO_TRANSFORM
           FROM ztbc_repro_ov
           INTO TABLE @DATA(it_repro_peso).


**** começo a atualizar a OV.
      LOOP AT it_repro_peso ASSIGNING FIELD-SYMBOL(<it_saida_peso>).
        REFRESH: bapisdpart[],
                 partners[],
                 return[].

        REFRESH: li_order_items, li_order_items_inx.
        CLEAR: wa_header, wa_headerx.

        SELECT vbeln,
               posnr,
               matnr                               "#EC CI_NO_TRANSFORM
           FROM vbap
           INTO TABLE @DATA(lt_vbap_peso)
           WHERE vbeln EQ @<it_saida_peso>-vbeln AND
                 posnr EQ @<it_saida_peso>-posnr.

        IF sy-subrc EQ 0.
          SELECT
                 matnr,
                 brgew,
                 ntgew
            FROM mara
            INTO TABLE @DATA(lt_mara)
            FOR ALL ENTRIES IN @lt_vbap_peso
            WHERE matnr EQ @lt_vbap_peso-matnr.

        ENDIF.

        READ TABLE lt_mara ASSIGNING FIELD-SYMBOL(<fs_mara>) INDEX 1.
        lwa_order_items-itm_number = <it_saida_peso>-posnr.
        lwa_order_items-material   = <fs_mara>-matnr.
        lwa_order_items-net_weight = <fs_mara>-ntgew. " liquido
        lwa_order_items-gross_wght = <fs_mara>-brgew. " bruto
        APPEND lwa_order_items TO li_order_items.


        lwa_order_items_inx-updateflag  = 'U'.
        lwa_order_items_inx-material    = 'X'.
        lwa_order_items_inx-net_weight  = 'X'.
        lwa_order_items_inx-gross_wght  = 'X'.
        APPEND lwa_order_items_inx TO li_order_items_inx.

        CLEAR: lwa_order_items, lwa_order_items_inx.
        wa_headerx-updateflag = 'U'.

        CALL FUNCTION 'BAPI_SALESORDER_CHANGE'
          EXPORTING
            order_header_in   = wa_header
            order_header_inx  = wa_headerx
            salesdocument     = <it_saida_peso>-vbeln
            behave_when_error = 'P'
          TABLES
            return            = return
            order_item_in     = li_order_items
            order_item_inx    = li_order_items_inx
            partnerchanges    = partners
            extensionin       = it_extensionin.

        IF sy-subrc EQ 0.
*        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
          COMMIT WORK AND WAIT .
        ELSE.
          CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
        ENDIF.
      ENDLOOP.
    ENDIF.

  ENDIF.

*&---------------------------------------------------------------------*
*& Form f_file_dialog
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_file_dialog .
  DATA:
      lv_window_title TYPE string.

  lv_window_title = 'Arquivo para upload'(002).

  DATA:
    lt_file_table TYPE  filetable,
    lv_rc         TYPE i.
  cl_gui_frontend_services=>file_open_dialog(
    EXPORTING
      window_title = lv_window_title
      file_filter  = '*.csv'
    CHANGING
      file_table   = lt_file_table
      rc           = lv_rc
    EXCEPTIONS
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      not_supported_by_gui    = 4
      OTHERS                  = 5 ).
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
      DISPLAY LIKE 'E'.
  ENDIF.
  IF lv_rc >= 1.
    READ TABLE lt_file_table INDEX 1 INTO DATA(ls_file_table).
    IF  sy-subrc = 0.
      p_file = ls_file_table-filename.
    ENDIF.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form zf_nfe_oms_ecomm
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM zf_nfe_oms_ecomm .
*&---------------------------------------------------------------------*
*& Include          ZSD_SENDNFE_OMS_ECOMM_C01
*&---------------------------------------------------------------------*
  TYPES: BEGIN OF ty_mara_ecomm,
           matnr              TYPE matnr,
           zz1_externalb1_prd TYPE zz1_externalb1,
         END OF ty_mara_ecomm.
*
  TYPES: BEGIN OF ty_item_ecomm,
           itemnumber   TYPE j_1bnflin-matnr,
           materialcode TYPE j_1bnflin-matnr,
           cfop         TYPE j_1bnflin-cfop,
         END OF ty_item_ecomm.

  TYPES: BEGIN OF ty_vbrp,
           vbeln TYPE vbrp-vbeln,
           vgbel TYPE vbrp-vgbel,
           aubel TYPE vbrp-aubel,
         END OF ty_vbrp.

  TYPES: BEGIN OF ty_nfeout_ecomm,
           accesskey                   TYPE string,
           operationtype               TYPE string,
           invoicedata                 TYPE string,
           externalorderid             TYPE string,
           salesorderid                TYPE string,
           deliveryid                  TYPE string,
           basketcode                  TYPE string,
           nfenumber                   TYPE string,
           nfeserie                    TYPE string,
           authorizationtimestamp      TYPE string,
           authorizationprotocolnumber TYPE string,
           issuercnpj                  TYPE string,
           issuerstatetaxnumber        TYPE string,
           issuername                  TYPE string,
           internalnfeid               TYPE string,
           packages                    TYPE string,
           weight                      TYPE string,
           itemstotal                  TYPE string,
           nftotal                     TYPE string,
           items                       TYPE TABLE OF ty_item_ecomm WITH NON-UNIQUE DEFAULT KEY,
         END OF ty_nfeout_ecomm.
*
  TYPES: BEGIN OF ty_doclin_ecomm,
           docnum     TYPE j_1bnfdoc-docnum,
           cnpj_bupla TYPE j_1bnfdoc-cnpj_bupla,
           ie_bupla   TYPE j_1bnfdoc-ie_bupla,
           nftot      TYPE j_1bnfdoc-nftot,
           nfenum     TYPE j_1bnfdoc-nfenum,
           authtime   TYPE j_1bnfdoc-authtime,
           brgew      TYPE j_1bnfdoc-brgew,
           ntgew      TYPE j_1bnfdoc-ntgew,
           anzpk      TYPE j_1bnfdoc-anzpk,
           docdat     TYPE j_1bnfdoc-docdat,
           xped       TYPE j_1bnflin-xped,
           itmnum     TYPE j_1bnflin-itmnum,
           matnr      TYPE j_1bnflin-matnr,
           cfop       TYPE j_1bnflin-cfop,
           refkey     TYPE j_1bnflin-refkey,
           nfnet      TYPE j_1bnflin-nfnet,
         END OF ty_doclin_ecomm.

  DATA: lt_doclin_ecomm  TYPE TABLE OF ty_doclin_ecomm,
        ls_doclin_ecomm  LIKE LINE OF  lt_doclin_ecomm,
        lt_nfeout_ecomm  TYPE TABLE OF ty_nfeout_ecomm,
        ls_nfeout_ecomm  LIKE LINE OF  lt_nfeout_ecomm,
        lt_itens_ecomm   TYPE TABLE OF ty_item_ecomm,
        ls_itens_ecomm   LIKE LINE OF  lt_itens_ecomm,
        lt_mara_ecomm    TYPE TABLE OF ty_mara_ecomm,
        ls_mara_ecomm    LIKE LINE OF  lt_mara_ecomm,
        lo_client_ecomm  TYPE REF TO   if_http_client,
        lv_res_bin       TYPE xstring,
        lv_res_str       TYPE string,
        lv_req_bin       TYPE xstring,
        lv_req_str       TYPE string,
        gv_acc_ecomm     TYPE c LENGTH 44,
        lv_vbeln         TYPE vbrp-vbeln,
        lt_vbrp          TYPE TABLE OF ty_vbrp,
        ls_vbrp          LIKE LINE OF  lt_vbrp,
        lo_conv_ecomm    TYPE REF TO cl_abap_conv_in_ce,
        lv_company_ecomm TYPE butxt,
        lx_root_ecomm    TYPE REF TO cx_root,
        gv_accesskey     TYPE c LENGTH 44,
        lv_nfnet         TYPE j_1bnflin-nfnet,
        lv_nfnets        TYPE c LENGTH 30.

  cl_http_client=>create_by_destination(
    EXPORTING
      destination              = 'SAPINTSUITE_NFE_HTTP_ECOMM'
    IMPORTING
      client                   = lo_client_ecomm
    EXCEPTIONS
      argument_not_found       = 1
      destination_not_found    = 2
      destination_no_authority = 3
      plugin_not_active        = 4
      internal_error           = 5
      OTHERS                   = 6
  ).

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  lo_client_ecomm->request->set_method( if_http_request=>co_request_method_post ).

  SELECT
          doc~docnum,
          doc~cnpj_bupla,
          doc~ie_bupla,
          doc~nftot,
          doc~nfenum,
          doc~authtime,
          doc~brgew,
          doc~ntgew,
          doc~anzpk,
          doc~docdat,
          lin~xped,
          lin~itmnum,
          lin~matnr,
          lin~cfop,
          lin~refkey,
          lin~nfnet
    FROM j_1bnfdoc AS doc
    INNER JOIN j_1bnflin AS lin
    ON doc~docnum = lin~docnum
    INTO TABLE @lt_doclin_ecomm
    WHERE doc~docnum IN @s_docnum.

  IF sy-subrc = 0.
    SELECT docnum,
       bukrs,
       regio,
       nfyear,
       direct,
       authdate,
       authtime,
       authcod,
       nfmonth,
       stcd1,
       model,
       serie ,
       nfnum9,
       docnum9,
       cdv
      FROM j_1bnfe_active INTO TABLE @DATA(lt_active)
       FOR ALL ENTRIES IN @lt_doclin_ecomm
       WHERE docnum = @lt_doclin_ecomm-docnum
         AND docsta = '1'.

  ELSE.
    MESSAGE 'Não foram achados documentos para essa seleção' TYPE 'E'.
  ENDIF.

  SORT: lt_doclin_ecomm[] BY docnum,
        lt_active[] BY docnum.

  DATA(lt_nfeaux_ecomm) = lt_doclin_ecomm[].
  DELETE ADJACENT DUPLICATES FROM lt_doclin_ecomm COMPARING docnum.

  SORT lt_active BY docnum.

  LOOP AT lt_doclin_ecomm ASSIGNING FIELD-SYMBOL(<fs>).
    CLEAR: ls_nfeout_ecomm, lv_req_str, lt_itens, gv_accesskey.

    lo_client_ecomm->request->set_method( if_http_request=>co_request_method_post ).

    READ TABLE lt_active ASSIGNING FIELD-SYMBOL(<fs_acti>) WITH KEY docnum = <fs>-docnum BINARY SEARCH.

    IF sy-subrc <> 0.
      CONTINUE.
    ENDIF.



    gv_accesskey = |{ <fs_acti>-regio }{ <fs_acti>-nfyear }|   &&
                   |{ <fs_acti>-nfmonth }{ <fs_acti>-stcd1 }|  &&
                   |{ <fs_acti>-model }{ <fs_acti>-serie }|    &&
                   |{ <fs_acti>-nfnum9 }{ <fs_acti>-docnum9 }| &&
                   |{ <fs_acti>-cdv }|.

    CLEAR: lv_company.
    SELECT SINGLE butxt
      FROM t001 INTO ( lv_company )
      WHERE bukrs EQ <fs_acti>-bukrs.



    ls_nfeout_ecomm-accesskey                        = gv_accesskey.         "1
    ls_nfeout_ecomm-externalorderid                  = <fs>-xped. "2
    ls_nfeout_ecomm-nfenumber                        = <fs>-nfenum. "6
    ls_nfeout_ecomm-nfeserie                         = <fs_acti>-serie.        "7
    CONCATENATE <fs_acti>-authdate <fs_acti>-authtime INTO  ls_nfeout_ecomm-authorizationtimestamp. "8
*    "2022-11-10T15:19:01.000Z"
    CONCATENATE ls_nfeout_ecomm-authorizationtimestamp(4) '-' ls_nfeout_ecomm-authorizationtimestamp+4(2) '-'
                ls_nfeout_ecomm-authorizationtimestamp+6(2)'T' ls_nfeout_ecomm-authorizationtimestamp+8(2) ':'
                ls_nfeout_ecomm-authorizationtimestamp+10(2)':' ls_nfeout_ecomm-authorizationtimestamp+12(2)
                '.000Z' INTO ls_nfeout_ecomm-authorizationtimestamp.
    ls_nfeout_ecomm-authorizationprotocolnumber      = <fs_acti>-authcod. "9
    ls_nfeout_ecomm-issuercnpj                       = <fs>-cnpj_bupla. "10
    ls_nfeout_ecomm-issuerstatetaxnumber             = <fs>-ie_bupla. "11
    ls_nfeout_ecomm-issuername                       = lv_company. "12
    ls_nfeout_ecomm-internalnfeid                    = <fs_acti>-docnum. "13
    ls_nfeout_ecomm-packages                         = <fs>-anzpk. "14
    ls_nfeout_ecomm-weight                           = <fs>-brgew. "15
*    ls_nfeout_ecomm-itemstotal                       = <fs>-brgew. "16
    ls_nfeout_ecomm-nftotal                          = <fs>-nftot.  "17
    CONDENSE: ls_nfeout_ecomm-itemstotal  NO-GAPS,
              ls_nfeout_ecomm-nftotal     NO-GAPS.
    ls_nfeout_ecomm-operationtype =  <fs_acti>-direct.
    ls_nfeout_ecomm-invoicedata = <fs>-docdat.

    CLEAR: gt_mara.
    SELECT matnr zz1_externalb1_prd FROM mara INTO TABLE gt_mara FOR ALL ENTRIES IN lt_nfeaux_ecomm WHERE matnr = lt_nfeaux_ecomm-matnr.
    SORT gt_mara BY matnr.

    CLEAR: lv_nfnet.
    LOOP AT lt_nfeaux_ecomm INTO DATA(ls_nfeaux) WHERE docnum = <fs>-docnum.
      CLEAR: ls_itens, ls_mara.
      READ TABLE gt_mara INTO ls_mara WITH KEY matnr = ls_nfeaux-matnr BINARY SEARCH.
      IF sy-subrc = 0 .
        IF ls_mara-zz1_externalb1_prd IS NOT INITIAL.
          ls_itens_ecomm-materialcode    = ls_mara-zz1_externalb1_prd.
        ELSE.
          ls_itens_ecomm-materialcode    = ls_mara-matnr.
        ENDIF.
        ls_itens_ecomm-cfop       = ls_nfeaux-cfop(4).
        ls_itens_ecomm-itemnumber = ls_nfeaux-itmnum.
        APPEND ls_itens_ecomm TO lt_itens_ecomm.
      ENDIF.
      lv_nfnet = lv_nfnet + ls_nfeaux-nfnet."16
    ENDLOOP.


    CLEAR: lv_nfnets.
    lv_nfnets = lv_nfnet.
    CONDENSE lv_nfnets NO-GAPS.
    ls_nfeout_ecomm-itemstotal = lv_nfnets. "16

    CLEAR: lv_vbeln.
    lv_vbeln = <fs>-refkey.

    SELECT vbeln vgbel aubel FROM vbrp INTO TABLE lt_vbrp WHERE vbeln = lv_vbeln.
    SORT lt_vbrp BY vbeln.

    READ TABLE lt_vbrp INTO ls_vbrp INDEX 1.
    ls_nfeout_ecomm-salesorderid                  = ls_vbrp-aubel. "3
    ls_nfeout_ecomm-deliveryid                    = ls_vbrp-vgbel. "4


    SELECT SINGLE venum
      FROM vepo
      INTO @DATA(lv_venum)
      WHERE vbeln = @ls_vbrp-vgbel.
    IF sy-subrc = 0.

      SELECT SINGLE exidv
        FROM vekp
        INTO @DATA(lv_exidv)
        WHERE venum = @lv_venum.

      IF sy-subrc = 0 .
        ls_nfeout_ecomm-basketcode  = lv_exidv. "5
      ENDIF.

    ENDIF.







*ls_nfeout_ecomm-basketCode                    = novo. "5

    ls_nfeout_ecomm-items = lt_itens_ecomm.

    lv_req_str = /ui2/cl_json=>serialize( data = ls_nfeout_ecomm pretty_name = /ui2/cl_json=>pretty_mode-none ).



    REPLACE 'ACCESSKEY'                       WITH 'accessKey'                      INTO lv_req_str.
    REPLACE 'EXTERNALORDERID'                 WITH 'externalOrderId'                INTO lv_req_str.
    REPLACE 'SALESORDERID'                    WITH 'salesOrderId'                   INTO lv_req_str.
    REPLACE 'DELIVERYID'                      WITH 'deliveryId'                     INTO lv_req_str.
    REPLACE 'BASKETCODE'                      WITH 'basketCode'                     INTO lv_req_str.
    REPLACE 'NFENUMBER'                       WITH 'nfeNumber'                      INTO lv_req_str.
    REPLACE 'NFESERIE'                        WITH 'nfeSerie'                       INTO lv_req_str.
    REPLACE 'AUTHORIZATIONTIMESTAMP'          WITH 'authorizationTimestamp'         INTO lv_req_str.
    REPLACE 'AUTHORIZATIONPROTOCOLNUMBER'     WITH 'authorizationProtocolNumber'    INTO lv_req_str.
    REPLACE 'ISSUERCNPJ'                      WITH 'issuerCnpj'                     INTO lv_req_str.
    REPLACE 'ISSUERSTATETAXNUMBER'            WITH 'issuerStateTaxNumber'           INTO lv_req_str.
    REPLACE 'ISSUERNAME'                      WITH 'issuerName'                     INTO lv_req_str.
    REPLACE 'INTERNALNFEID'                   WITH 'internalNfeId'                  INTO lv_req_str.
    REPLACE 'PACKAGES'                        WITH 'packages'                       INTO lv_req_str.
    REPLACE 'WEIGHT'                          WITH 'weight'                         INTO lv_req_str.
    REPLACE 'ITEMSTOTAL'                      WITH 'itemsTotal'                     INTO lv_req_str.
    REPLACE 'NFTOTAL'                         WITH 'nfTotal'                        INTO lv_req_str.
    REPLACE 'ITEMS'                           WITH 'items'                          INTO lv_req_str.
    REPLACE 'OPERATIONTYPE'                   WITH 'operationType'                  INTO lv_req_str.
    REPLACE 'INVOICEDATA'                     WITH 'invoiceData'                    INTO lv_req_str.

    REPLACE ALL OCCURRENCES OF 'ITEMNUMBER'   IN lv_req_str                     WITH 'itemNumber'.
    REPLACE ALL OCCURRENCES OF 'MATERIALCODE' IN lv_req_str                     WITH 'materialCode'.
    REPLACE ALL OCCURRENCES OF 'CFOP'         IN lv_req_str                     WITH 'cfop'.


    TRY.
        cl_bcs_convert=>string_to_xstring(
          EXPORTING
            iv_string     = lv_req_str
          RECEIVING
            ev_xstring    = lv_req_bin
        ).
      CATCH cx_bcs cx_j_1bnfe_cf cx_j_1bnfe_exception cx_j1bnfe_exception INTO lx_root_ecomm.
    ENDTRY.

    lo_client_ecomm->request->set_data(
      EXPORTING
        data               = lv_req_bin
    ).

    lo_client_ecomm->send( ).
    lo_client_ecomm->receive( ).
    lv_res_bin = lo_client_ecomm->response->get_data( ).
    lo_conv_ecomm = cl_abap_conv_in_ce=>create( input = lv_res_bin ).
    lo_conv_ecomm->read( IMPORTING data = lv_res_str ).
    lo_client_ecomm->close( ).

  ENDLOOP.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form zf_nfe_rabbit
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM zf_nfe_rabbit .


  DATA:
    lo_clients        TYPE REF TO if_http_client,
    lv_res_data_bins  TYPE xstring,
    lv_res_data_strs  TYPE string,
    lv_req_data_bins  TYPE xstring,
    lv_req_data_str1s TYPE string,
    lv_req_data_str2s TYPE string,
    lv_req_data_str3s TYPE string,
    lv_req_data_str4s TYPE string,
    lo_convs          TYPE REF TO cl_abap_conv_in_ce,
    lv_companys       TYPE butxt VALUE 'Beleza.com Com Prod Blz'.

  DATA: lx_roots                    TYPE REF TO cx_root.




  DATA: gv_accesskey  TYPE c LENGTH 44,
        lv_cnpj_bupla TYPE j_1bnfdoc-cnpj_bupla,
        lv_ie_bupla   TYPE j_1bnfdoc-ie_bupla,
        lv_nftot      TYPE c LENGTH 10,
        lv_docdat     TYPE j_1bnfdoc-docdat,
        lv_docstat    TYPE j_1bnfdoc-docstat,
        lv_xped       TYPE j_1bnflin-xped,
        lv_nfenum     TYPE j_1bnfdoc-nfenum,
        lv_authtime   TYPE j_1bnfdoc-authtime.
  DATA: wa TYPE j_1bnfdoc.

  SELECT  doc~cnpj_bupla,
          doc~ie_bupla,
          doc~docnum,
          doc~nftot,
          doc~docdat,
*          doc~cretim,
          doc~docstat,
          doc~nfenum,
          doc~authtime,
          doc~brgew,
          doc~ntgew,
          lin~xped,
          lin~matnr,
          lin~cfop
    FROM j_1bnfdoc AS doc
    INNER JOIN j_1bnflin AS lin
    ON doc~docnum = lin~docnum
    INTO TABLE @lt_nfe
    WHERE doc~docnum IN @s_docnum.


  IF lt_nfe[] IS NOT INITIAL.
    SELECT docnum,
           bukrs,
           regio,
           nfyear,
           direct,
           authdate,
           authtime,
           authcod,
           nfmonth,
           stcd1,
           model,
           serie ,
           nfnum9,
           docnum9,
           cdv
    FROM j_1bnfe_active INTO TABLE @DATA(lt_active)
      FOR ALL ENTRIES IN @lt_nfe
      WHERE docnum EQ @lt_nfe-docnum.
  ENDIF.

  SORT: lt_nfe[] BY docnum,
        lt_active[] BY docnum.

  lt_nfeaux[] = lt_nfe[].
  DELETE ADJACENT DUPLICATES FROM lt_nfe COMPARING docnum.
*
  LOOP AT lt_nfe ASSIGNING FIELD-SYMBOL(<fs>).
    CLEAR: ls_nfeout, lv_req_data_str4s, lt_itens.

    cl_http_client=>create_by_destination(
      EXPORTING
        destination              = 'SAPINTSUITE_NFE_HTTP'
      IMPORTING
        client                   = lo_clients
      EXCEPTIONS
        argument_not_found       = 1
        destination_not_found    = 2
        destination_no_authority = 3
        plugin_not_active        = 4
        internal_error           = 5
        OTHERS                   = 6
    ).


    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    lo_clients->request->set_method( if_http_request=>co_request_method_post ).

    lv_nftot = <fs>-nftot.
    CONDENSE lv_nftot NO-GAPS.

    READ TABLE lt_active ASSIGNING FIELD-SYMBOL(<fs_acti>) WITH KEY docnum = <fs>-docnum BINARY SEARCH.

    gv_accesskey = |{ <fs_acti>-regio }{ <fs_acti>-nfyear }|   &&
                   |{ <fs_acti>-nfmonth }{ <fs_acti>-stcd1 }|  &&
                   |{ <fs_acti>-model }{ <fs_acti>-serie }|    &&
                   |{ <fs_acti>-nfnum9 }{ <fs_acti>-docnum9 }| &&
                   |{ <fs_acti>-cdv }|.

    CLEAR: lv_company.
    SELECT SINGLE butxt
      FROM t001 INTO ( lv_company )
      WHERE bukrs EQ <fs_acti>-bukrs.


    ls_nfeout-br_notafiscal                 = <fs_acti>-docnum.
    ls_nfeout-bstnk                         = <fs>-xped.
    ls_nfeout-br_nfissuercnpjorcpf          = <fs>-cnpj_bupla.
    ls_nfeout-br_nfissuerstatetaxnumber     = <fs>-ie_bupla.
    ls_nfeout-br_nftotalamount              = <fs>-nftot.
    ls_nfeout-br_nfeaccesskey               = gv_accesskey.
    ls_nfeout-br_nfdirection                = <fs_acti>-direct.
    CONCATENATE <fs_acti>-authdate <fs_acti>-authtime INTO  ls_nfeout-br_nfauthenticationdate.
    ls_nfeout-br_nfissuedate                = <fs>-docdat.
*    ls_nfeout-br_nfissuetime                = <fs>-cretim.
    ls_nfeout-br_nfedocumentstatus          = <fs>-docstat.
    ls_nfeout-br_nfeseries                  = <fs_acti>-serie.
    ls_nfeout-br_nfenumber                  = <fs>-nfenum.
    ls_nfeout-br_nfauthznprotocolnumber     = <fs_acti>-authcod.
    ls_nfeout-br_nfauthenticationtime       = <fs>-authtime.
    ls_nfeout-companycodename               = lv_company.
    ls_nfeout-brgew                         = <fs>-brgew.
    ls_nfeout-ntgew                         = <fs>-ntgew.

    CONDENSE: ls_nfeout-brgew  NO-GAPS,
              ls_nfeout-ntgew NO-GAPS,
              ls_nfeout-br_nftotalamount NO-GAPS.

    CLEAR: gt_mara.
    SELECT matnr zz1_externalb1_prd FROM mara INTO TABLE gt_mara FOR ALL ENTRIES IN lt_nfeaux WHERE matnr = lt_nfeaux-matnr.
    SORT gt_mara BY matnr.



    LOOP AT lt_nfeaux INTO DATA(ls_nfeaux) WHERE docnum = <fs>-docnum.
      CLEAR: ls_itens, ls_mara.
      READ TABLE gt_mara INTO ls_mara WITH KEY matnr = ls_nfeaux-matnr BINARY SEARCH.
      IF sy-subrc = 0 .
        IF ls_mara-zz1_externalb1_prd IS NOT INITIAL.
          ls_itens-sku    = ls_mara-zz1_externalb1_prd.
        ELSE.
          ls_itens-sku    = ls_mara-matnr.
        ENDIF.
        ls_itens-cfop   = ls_nfeaux-cfop(4).
        APPEND ls_itens TO lt_itens.
      ENDIF.
    ENDLOOP.

    ls_nfeout-itens = lt_itens.

    lv_req_data_str4s = /ui2/cl_json=>serialize( data = ls_nfeout pretty_name = /ui2/cl_json=>pretty_mode-none ).

    REPLACE 'BR_NOTAFISCAL'             WITH 'BR_NotaFiscal'             INTO lv_req_data_str4s.
    REPLACE 'BR_NFISSUERCNPJORCPF'      WITH 'BR_NFIssuerCNPJOrCPF'      INTO lv_req_data_str4s.
    REPLACE 'BR_NFISSUERSTATETAXNUMBER' WITH 'BR_NFIssuerStateTaxNumber' INTO lv_req_data_str4s.
    REPLACE 'BR_NFTOTALAMOUNT'          WITH 'BR_NFTotalAmount'          INTO lv_req_data_str4s.
    REPLACE 'BR_NFEACCESSKEY'           WITH 'BR_NFeAccessKey'           INTO lv_req_data_str4s.
    REPLACE 'BR_NFDIRECTION'            WITH 'BR_NFDirection'            INTO lv_req_data_str4s.
    REPLACE 'BR_NFAUTHENTICATIONDATE'   WITH 'BR_NFAuthenticationDate'   INTO lv_req_data_str4s.
    REPLACE 'BR_NFISSUEDATE'            WITH 'BR_NFIssueDate'            INTO lv_req_data_str4s.
*    REPLACE 'BR_NFISSUETIME'            WITH 'BR_NFIssueTime'            INTO lv_req_data_str4s.
    REPLACE 'BR_NFEDOCUMENTSTATUS'      WITH 'BR_NFeDocumentStatus'      INTO lv_req_data_str4s.
    REPLACE 'BR_NFESERIES'              WITH 'BR_NFeSeries'              INTO lv_req_data_str4s.
    REPLACE 'BR_NFENUMBER'              WITH 'BR_NFeNumber'              INTO lv_req_data_str4s.
    REPLACE 'BR_NFAUTHZNPROTOCOLNUMBER' WITH 'BR_NFAuthznProtocolNumber' INTO lv_req_data_str4s.
    REPLACE 'BR_NFAUTHENTICATIONTIME'   WITH 'BR_NFAuthenticationTime'   INTO lv_req_data_str4s.
    REPLACE 'COMPANYCODENAME'           WITH 'CompanyCodeName'           INTO lv_req_data_str4s.
    REPLACE 'ITENS'                     WITH 'Itens'                     INTO lv_req_data_str4s.


*        CONCATENATE '{ "BR_NotaFiscal": "' <fs_acti>-docnum '", "BSTNK":  "' <fs>-xped '",    "BR_NFIssuerCNPJOrCPF" : "' <fs>-cnpj_bupla '", "BR_NFIssuerStateTaxNumber" : "' <fs>-ie_bupla '","BR_NFTotalAmount" : "' lv_nftot '",  "BR_NFeAccessKey" : "'
*        gv_accesskey '",' INTO  lv_req_data_str1s .
*        CONCATENATE   ' "BR_NFDirection" : "' <fs_acti>-direct '", "BR_NFAuthenticationDate":   "' <fs_acti>-authdate <fs_acti>-authtime  '",  "BR_NFIssueDate" : "' <fs>-docdat '","BR_NFeDocumentStatus" : "' <fs>-docstat '", "BR_NFeSeries" : "'
*    <fs_acti>-serie '",' INTO lv_req_data_str2s.
*        CONCATENATE   ' "BR_NFeNumber" : "' <fs>-nfenum '",   "BR_NFAuthznProtocolNumber" : "' <fs_acti>-authcod '","BR_NFAuthenticationTime" : "' wa-authtime '", "CompanyCodeName" : "' lv_companys '"}' INTO lv_req_data_str3s.
*        CONCATENATE lv_req_data_str1s lv_req_data_str2s lv_req_data_str3s INTO lv_req_data_str4s.

*
***
***      CONCATENATE '{ "BR_NotaFiscal": "' <fs_active>-docnum '", "BSTNK":  "' <fs_nfe>-xped '", "BR_NFIssuerCNPJOrCPF": "' <fs_nfe>-cnpj_bupla '", "BR_NFIssuerStateTaxNumber": "' <fs_nfe>-ie_bupla '","BR_NFTotalAmount": "' lv_nftot ",
***      gv_accesskey '",' INTO  lv_req_data_str1s .
***
***      CONCATENATE   ' "BR_NFDirection" : "'  <fs_active>-direct '", "BR_NFAuthenticationDate":   "' <fs_active>-authdate <fs_active>-authtime  '",  "BR_NFIssueDate" : "' <fs_nfe>-docdat '", "BR_NFeDocumentStatus" : "' <fs_nfe>-docstat  ",
***      ' "BR_NFeSeries:": "' <fs_active>-serie '",' INTO lv_req_data_str2s.
***
***      CONCATENATE   ' "BR_NFeNumber":  "' <fs_nfe>-nfenum '",   "BR_NFAuthznProtocolNumber":  "' <fs_active>-authcod '","BR_NFAuthenticationTime" : "' <fs_nfe>-authtime '", "CompanyCodeName" : "' lv_companys '"}' INTO lv_req_data_str3s.
***
***      CONCATENATE lv_req_data_str1s lv_req_data_str2s lv_req_data_str3s INTO lv_req_data_str4s.

    TRY.
        cl_bcs_convert=>string_to_xstring(
          EXPORTING
            iv_string     = lv_req_data_str4s
          RECEIVING
            ev_xstring    = lv_req_data_bins
        ).
      CATCH cx_bcs cx_j_1bnfe_cf cx_j_1bnfe_exception cx_j1bnfe_exception INTO lx_roots.
        DATA lo_message_text TYPE REF TO if_t100_message.

    ENDTRY.

    lo_clients->request->set_data(
      EXPORTING
        data               = lv_req_data_bins
    ).

    lo_clients->send( ).
    lo_clients->receive( ).

    lv_res_data_bins = lo_clients->response->get_data( ).

    lo_convs = cl_abap_conv_in_ce=>create( input = lv_res_data_bins ).
    lo_convs->read( IMPORTING data = lv_res_data_strs ).

    lo_clients->close( ).

  ENDLOOP.


ENDFORM.
