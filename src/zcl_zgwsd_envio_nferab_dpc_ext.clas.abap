class ZCL_ZGWSD_ENVIO_NFERAB_DPC_EXT definition
  public
  inheriting from ZCL_ZGWSD_ENVIO_NFERAB_DPC
  create public .

public section.

  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~CHANGESET_BEGIN
    redefinition .
  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~CHANGESET_PROCESS
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZCL_ZGWSD_ENVIO_NFERAB_DPC_EXT IMPLEMENTATION.


  method /IWBEP/IF_MGW_APPL_SRV_RUNTIME~CHANGESET_BEGIN.
**TRY.
*CALL METHOD SUPER->/IWBEP/IF_MGW_APPL_SRV_RUNTIME~CHANGESET_BEGIN
*  EXPORTING
*    IT_OPERATION_INFO =
**  CHANGING
**    cv_defer_mode     =
*    .
**  CATCH /iwbep/cx_mgw_busi_exception.
**  CATCH /iwbep/cx_mgw_tech_exception.
**ENDTRY.

    cv_defer_mode = abap_true .
  endmethod.


  METHOD /iwbep/if_mgw_appl_srv_runtime~changeset_process.
**TRY.
*CALL METHOD SUPER->/IWBEP/IF_MGW_APPL_SRV_RUNTIME~CHANGESET_PROCESS
*  EXPORTING
*    IT_CHANGESET_REQUEST  =
*  CHANGING
*    CT_CHANGESET_RESPONSE =
*    .
**  CATCH /iwbep/cx_mgw_busi_exception.
**  CATCH /iwbep/cx_mgw_tech_exception.
**ENDTRY.

    DATA : lo_func_import_context TYPE REF TO /iwbep/if_mgw_req_func_import,
           lt_parameters          TYPE /iwbep/t_mgw_name_value_pair,
           ls_zinferabbitmq       TYPE zi_nferabbitmq,
           ls_result              TYPE zcl_zgwsd_envio_nferab_mpc_ext=>ts_zi_nferabbitmqtype,
           ls_changeset_response  TYPE /iwbep/if_mgw_appl_types=>ty_s_changeset_response.

    DATA: lo_client TYPE REF TO if_http_client,
          lo_conv   TYPE REF TO cl_abap_conv_in_ce.

    DATA: lv_accesskey    TYPE c LENGTH 44,
          lv_valortotal   TYPE c LENGTH 10,
          lv_company      TYPE butxt VALUE 'Beleza.com Com Prod Blz',
          lv_res_data_str TYPE string.

    "read requests where operation is execute action (EA)
    LOOP AT it_changeset_request ASSIGNING FIELD-SYMBOL(<lfs_changeset_request>)
            WHERE operation_type = /iwbep/if_mgw_appl_types=>gcs_operation_type-execute_action.

      "find function name
      lo_func_import_context ?= <lfs_changeset_request>-request_context .
      DATA(lv_function_import_name) = lo_func_import_context->get_function_import_name( ) .

      IF lv_function_import_name = 'Enviar'.
        "read parameters
        lt_parameters = lo_func_import_context->get_parameters( ).
        ls_zinferabbitmq-documento = lt_parameters[ name = 'DOCUMENTO' ]-value .

        SELECT SINGLE *
          FROM zi_nferabbitmq
          INTO @ls_result
          WHERE documento = @ls_zinferabbitmq-documento.

        IF sy-subrc = 0.
          cl_http_client=>create_by_destination(
          EXPORTING
            destination              = 'SAPINTSUITE_NFE_HTTP'
          IMPORTING
            client                   = lo_client
          EXCEPTIONS
            argument_not_found       = 1
            destination_not_found    = 2
            destination_no_authority = 3
            plugin_not_active        = 4
            internal_error           = 5
            OTHERS                   = 6     ).
          IF sy-subrc <> 0.
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                      INTO DATA(lv_message)
                      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

            ls_result-mensagem = lv_message.
          ELSE.

            lo_client->request->set_method( if_http_request=>co_request_method_post ).

            lv_valortotal = ls_result-valortotal.
            CONDENSE lv_valortotal NO-GAPS.

            lv_accesskey = |{ ls_result-regiaoemissor     }{ ls_result-anodoc }|      &&
                           |{ ls_result-mesdoc            }{ ls_result-cnpjemissor }| &&
                           |{ ls_result-modelonf          }{ ls_result-serie }|       &&
                           |{ ls_result-nfe               }{ ls_result-chaveacesso }| &&
                           |{ ls_result-chaveacessodigito }|.

            CONCATENATE '{ "BR_NotaFiscal": "'                 ls_result-documento
                        '", "BSTNK":  "'                       ls_result-pedido
                        '",    "BR_NFIssuerCNPJOrCPF" : "'     ls_result-cnpjemitente
                        '", "BR_NFIssuerStateTaxNumber" : "'   ls_result-codimpostoestadual
                        '","BR_NFTotalAmount" : "'             lv_valortotal
                        '",  "BR_NFeAccessKey" : "'            lv_accesskey '",'    INTO DATA(lv_str1).

            CONCATENATE ' "BR_NFDirection" : "'                ls_result-direcaomov
                        '", "BR_NFAuthenticationDate":   "'    ls_result-dataproc ls_result-horaproc
                        '",  "BR_NFIssueDate" : "'             ls_result-datadoc
                        '","BR_NFeDocumentStatus" : "'         ls_result-statusdoc
                        '", "BR_NFeSeries" : "'                ls_result-serie '",' INTO DATA(lv_str2).

            CONCATENATE ' "BR_NFeNumber" : "'                  ls_result-nfenum
                        '",   "BR_NFAuthznProtocolNumber" : "' ls_result-numlog
                        '","BR_NFAuthenticationTime" : "'      ls_result-horaproc
                        '", "CompanyCodeName" : "'             lv_company '"}'      INTO DATA(lv_str3).

            CONCATENATE lv_str1 lv_str2 lv_str3 INTO DATA(lv_req_data_str).

            TRY.
                cl_bcs_convert=>string_to_xstring(
                  EXPORTING
                    iv_string     = lv_req_data_str
                  RECEIVING
                    ev_xstring    = DATA(lv_req_data_bin)
                ).
              CATCH cx_bcs cx_j_1bnfe_cf cx_j_1bnfe_exception cx_j1bnfe_exception INTO DATA(lx_root).
                DATA lo_message_text TYPE REF TO if_t100_message.
            ENDTRY.

            lo_client->request->set_data(
              EXPORTING
                data = lv_req_data_bin ).

            lo_client->send(
              EXCEPTIONS
                http_communication_failure = 1        " Communication Error
                http_invalid_state         = 2        " Invalid state
                http_processing_failed     = 3        " Error when processing method
                http_invalid_timeout       = 4        " Invalid Time Entry
                OTHERS                     = 5   ).
            IF sy-subrc <> 0.
              DATA(lv_erro) = abap_true.
            ENDIF.

            lo_client->receive(
              EXCEPTIONS
                http_communication_failure = 1        " Communication Error
                http_invalid_state         = 2        " Invalid state
                http_processing_failed     = 3        " Error when processing method
                OTHERS                     = 4  ).
            IF sy-subrc <> 0.
              lv_erro = abap_true.
            ENDIF.

            IF lv_erro IS NOT INITIAL.
              ls_result-mensagem = 'Erro de comunicação'.
            ELSE.

              DATA(lv_res_data_bin) = lo_client->response->get_data( ).

              lo_conv = cl_abap_conv_in_ce=>create( input = lv_res_data_bin ).
              lo_conv->read( IMPORTING data = lv_res_data_str ).

              DATA(lv_sub) = substring_after( val =  substring_before( val = lv_res_data_str sub = '</title>' )
                                              sub = '<title>' ). "HTTP ERROR...

              IF lv_sub IS INITIAL.
                ls_result-mensagem = 'Enviado com sucesso'.
              ELSE.
                ls_result-mensagem = lv_sub.
              ENDIF.

              lo_client->close( ).
            ENDIF. "lv_erro IS NOT INITIAL.
          ENDIF. "create_by_destination
        ENDIF.

        "prepare response with operation number and respective data,
        "insert in CT_CHANGESET_RESPONSE
        ls_changeset_response-operation_no = <lfs_changeset_request>-operation_no .
        copy_data_to_ref(
           EXPORTING
             is_data = ls_result
           CHANGING
             cr_data = ls_changeset_response-entity_data ).

        INSERT ls_changeset_response INTO TABLE ct_changeset_response.
      ENDIF .
    ENDLOOP .
  ENDMETHOD.
ENDCLASS.
