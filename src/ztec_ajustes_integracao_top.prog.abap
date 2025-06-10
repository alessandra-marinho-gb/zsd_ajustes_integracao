*&---------------------------------------------------------------------*
*& Include          ZTEC_AJUSTES_INTEGRACAO_TOP
*&---------------------------------------------------------------------*
TYPES: BEGIN OF ty_vbfa,
         vbelv TYPE vbfa-vbeln,
         posnv TYPE vbfa-posnv,
       END OF ty_vbfa,
       ty_t_vbfa TYPE STANDARD TABLE OF ty_vbfa,

       BEGIN OF ty_vbuk,
         vbeln TYPE vbuk-vbeln,
         wbstk TYPE vbuk-wbstk,
       END OF ty_vbuk,
       ty_t_vbuk TYPE STANDARD TABLE OF ty_vbuk,

       BEGIN OF ty_docven,
         vbeln TYPE vbap-vbeln,
         posnr TYPE vbap-posnr,
       END OF ty_docven,
       ty_t_docven TYPE STANDARD TABLE OF ty_docven,

       BEGIN OF ty_report,
         vbeln TYPE vbuk-vbeln,
         pdtxt TYPE string,
       END OF ty_report,
       ty_t_report         TYPE STANDARD TABLE OF ty_report,

       ty_t_ordem_item_in  TYPE STANDARD TABLE OF bapisditm,
       ty_t_ordem_item_inx TYPE STANDARD TABLE OF bapisditmx.

***Grupo Boticário - Gustavo Luquez - Ajuste estrutura de NF-e RabbitMQ - Inicio do Desenvolvimento

TYPES: BEGIN OF ty_itens,
         sku  TYPE j_1bnflin-matnr,
         cfop TYPE j_1bnflin-cfop,
       END OF ty_itens.

DATA:
  lt_itens TYPE TABLE OF ty_itens,
  ls_itens LIKE LINE OF lt_itens.

TYPES: BEGIN OF ty_nfeout,
         br_notafiscal             TYPE string,
         bstnk                     TYPE string,
         br_nfissuercnpjorcpf      TYPE string,
         br_nfissuerstatetaxnumber TYPE string,
         br_nftotalamount          TYPE string,
         br_nfeaccesskey           TYPE string,
         br_nfdirection            TYPE string,
         br_nfauthenticationdate   TYPE string,
         br_nfissuedate            TYPE string,
*         br_nfissuetime            TYPE string,
         br_nfedocumentstatus      TYPE string,
         br_nfeseries              TYPE string,
         br_nfenumber              TYPE string,
         br_nfauthznprotocolnumber TYPE string,
         br_nfauthenticationtime   TYPE string,
         companycodename           TYPE string,
         brgew                     TYPE string,
         ntgew                     TYPE string,
         itens                     TYPE TABLE OF ty_itens WITH NON-UNIQUE DEFAULT KEY,
       END OF ty_nfeout.

TYPES: BEGIN OF ty_doclin,
         cnpj_bupla TYPE j_1bnfdoc-cnpj_bupla,
         ie_bupla   TYPE j_1bnfdoc-ie_bupla,
         docnum     TYPE j_1bnfdoc-docnum,
         nftot      TYPE j_1bnfdoc-nftot,
         docdat     TYPE j_1bnfdoc-docdat,
*         cretim     TYPE j_1bnfdoc-cretim,
         docstat    TYPE j_1bnfdoc-docstat,
         nfenum     TYPE j_1bnfdoc-nfenum,
         authtime   TYPE j_1bnfdoc-authtime,
         brgew      TYPE j_1bnfdoc-brgew,
         ntgew      TYPE j_1bnfdoc-ntgew,
         xped       TYPE j_1bnflin-xped,
         matnr      TYPE j_1bnflin-matnr,
         cfop       TYPE j_1bnflin-cfop,
       END OF ty_doclin.

TYPES: BEGIN OF ty_mara,
         matnr              TYPE matnr,
         zz1_externalb1_prd TYPE zz1_externalb1,
       END OF ty_mara.


DATA:
  lt_nfe    TYPE TABLE OF ty_doclin,
  lt_nfeaux TYPE TABLE OF ty_doclin,
  lt_nfeout TYPE TABLE OF ty_nfeout,
  ls_nfeout LIKE LINE OF lt_nfeout,
  gt_mara   TYPE TABLE OF ty_mara,
  ls_mara   LIKE LINE OF gt_mara.

DATA:
        lv_company      TYPE butxt.

***Grupo Boticário - Gustavo Luquez - Ajuste estrutura de NF-e RabbitMQ - Inicio do Desenvolvimento

DATA: gt_ov      TYPE ty_t_vbfa,
      gt_remessa TYPE ty_t_vbuk,
      gt_docven  TYPE ty_t_docven,
      gt_report  TYPE ty_t_report.


DATA: go_salv             TYPE REF TO cl_salv_table,
      go_columns          TYPE REF TO cl_salv_columns_table,
      go_column           TYPE REF TO cl_salv_column,
      go_layout_settings  TYPE REF TO cl_salv_layout,
      gs_layout_key       TYPE        salv_s_layout_key,
      go_display_settings TYPE REF TO cl_salv_display_settings,
      gx_message          TYPE REF TO cx_salv_msg.
