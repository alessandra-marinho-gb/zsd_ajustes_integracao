*&---------------------------------------------------------------------*
*& Include          ZTEC_AJUSTES_INTEGRACAO_SCR
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b6 WITH FRAME TITLE TEXT-006.
  PARAMETERS :    p_cancel TYPE flag.
  SELECT-OPTIONS: s_can_re FOR likp-vbeln .
  SELECT-OPTIONS: s_can_ov FOR vbak-vbeln .
SELECTION-SCREEN END OF BLOCK b6.
