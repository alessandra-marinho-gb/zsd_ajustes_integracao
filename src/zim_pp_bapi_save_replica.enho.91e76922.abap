"Name: \FU:BAPI_BATCH_SAVE_REPLICA\SE:BEGIN\EI
ENHANCEMENT 0 ZIM_PP_BAPI_SAVE_REPLICA.

CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
  EXPORTING
    input        = material
  IMPORTING
    output       = material
  EXCEPTIONS
    length_error = 1
    OTHERS       = 2.
IF sy-subrc <> 0.
* Implement suitable error handling here
ENDIF.

ENDENHANCEMENT.
