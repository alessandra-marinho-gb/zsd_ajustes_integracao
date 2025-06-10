class ZCL_ZGWSD_ENVIO_REMESS_MPC_EXT definition
  public
  inheriting from ZCL_ZGWSD_ENVIO_REMESS_MPC
  create public .

public section.

  methods DEFINE
    redefinition .
protected section.
private section.

  methods ADD_ACTION
    importing
      !IV_ACTION_NAME type /IWBEP/MED_EXTERNAL_NAME .
ENDCLASS.



CLASS ZCL_ZGWSD_ENVIO_REMESS_MPC_EXT IMPLEMENTATION.


  METHOD add_action.
    DATA: lv_fc_fieldvalue TYPE /iwbep/med_annotation_value,
          lo_complex_type  TYPE REF TO /iwbep/if_mgw_odata_cmplx_type,
          lo_prop          TYPE REF TO /iwbep/if_mgw_odata_property.

    TRY .
        DATA(lo_action) = model->create_action( iv_action_name ).

        "set return parameter
        lo_action->set_return_entity_type( 'ZI_RemessaRabbitMQType' ) .
        lo_action->set_return_entity_set( 'ZI_RemessaRabbitMQ' ).

        lo_action->set_http_method( 'POST' ).
        lo_action->set_return_multiplicity( /iwbep/if_mgw_med_odata_types=>gcs_cardinality-cardinality_1_1 ).

      CATCH /iwbep/cx_mgw_med_exception.
    ENDTRY.

    TRY .
        "specify input parameters
        DATA(lo_parameter) = lo_action->create_input_parameter(
                                      iv_parameter_name = 'Remessa'
                                      iv_abap_fieldname = 'REMESSA' ).
        lo_parameter->/iwbep/if_mgw_odata_property~set_type_edm_string( ).
        lo_parameter->set_maxlength( iv_max_length = 10 ).

      CATCH /iwbep/cx_mgw_med_exception.
    ENDTRY.

    "Is Action Active?
*    CONCATENATE 'IsActive' iv_action_name INTO DATA(lv_action_ac).

    TRY .
        DATA(lo_annotation) = lo_action->/iwbep/if_mgw_odata_annotatabl~create_annotation( 'sap' ).
        lo_annotation->add( iv_key = 'action-for' iv_value = 'ZI_RemessaRabbitMQType' ).
*        lo_annotation = lo_action->/iwbep/if_mgw_odata_annotatabl~create_annotation( 'sap' ).
*        lo_annotation->add( iv_key = 'applicable-path' iv_value = lv_action_ac ).

      CATCH /iwbep/cx_mgw_med_exception.
    ENDTRY.
  ENDMETHOD.


  METHOD define.
    super->define( ) .
    add_action( iv_action_name = 'Enviar' ) .
  ENDMETHOD.
ENDCLASS.
