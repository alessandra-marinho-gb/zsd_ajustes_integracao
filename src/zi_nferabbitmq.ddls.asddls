@AbapCatalog.sqlViewName: 'ZINFERABBITMQ'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'NFe RabbitMQ'
@Metadata.allowExtensions: true
@ObjectModel.semanticKey: ['Documento']
define view ZI_NFeRabbitMQ
  as select distinct from j_1bnfdoc      as doc
    inner join            j_1bnflin      as lin on doc.docnum = lin.docnum
    inner join            j_1bnfe_active as act on doc.docnum = act.docnum
{
  key doc.docnum             as Documento,          //Nº documento
      doc.docdat             as DataDoc,            //Data do documento
      doc.cnpj_bupla         as CNPJEmitente,       //CNPJ do emitente
      doc.ie_bupla           as CodImpostoEstadual, //Código do imposto estadual
      doc.nftot              as ValorTotal,         //Valor total incluindo todos os impostos
      doc.docstat            as StatusDoc,          //Status do documento
      doc.nfenum             as NFeNum,             //Número de nota fiscal eletrônica
      lin.xped               as Pedido,             //Nº do pedido
      act.regio              as RegiaoEmissor,      //Região do emissor do documento
      act.nfyear             as AnoDoc,             //Ano da data do documento
      act.nfmonth            as MesDoc,             //Mês da data do documento
      act.direct             as DirecaoMov,         //Direção do movimento
      act.authdate           as DataProc,           //Registro hora processamento - data
      act.authtime           as HoraProc,           //Registro hora processamento - hora
      act.authcod            as NumLog,             //Nº do log
      act.stcd1              as CNPJEmissor,        //Nº CNPJ/CPF do emissor do documento
      act.model              as ModeloNF,           //Modelo da nota fiscal
      act.serie              as Serie,              //Séries
      act.nfnum9             as NFe,                //Número de nota fiscal eletrônica
      act.docnum9            as ChaveAcesso,        //Nº aleatório na chave de acesso
      act.cdv                as ChaveAcessoDigito,  //Dígito verificador para a chave de acesso
      
      cast( '' as abap.char( 200 ) )  as Mensagem
}
