@AbapCatalog.sqlViewName: 'ZIREMESSARABBIT'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Remessa RabbitMQ'
@Metadata.allowExtensions: true
@ObjectModel.semanticKey: ['Remessa']
define view ZI_RemessaRabbitMQ
  as select distinct from likp
    inner join            lips on likp.vbeln = lips.vbeln
    inner join            vbak on vbak.vbeln = lips.vgbel
{
  key likp.vbeln as Remessa,         //Remessa
      likp.faksk as BloqDocFatmto,   //Bloqueio de documento de faturamento em documento SD
      likp.vstel as LocalExpedReceb, //Local de expedição/ponto de recebimento
      lips.vgbel as DocReferencia,   //Nº documento do documento de referência
      vbak.vbeln as DocVendas,       //Documento de vendas
      vbak.bstnk as RefCliente,      //Referência de cliente
      
      cast( '' as abap.char( 200 ) )  as Mensagem
}
where
  likp.faksk <> 'TR'
