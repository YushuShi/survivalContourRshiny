output$pEffectsize<- renderTable({
  input$getPlot
  tableOut<-NULL
  if(!identical(input$noCompSelect,"rf")){
  dataTable<-filedata()
  predictPlot<-calculatePredict()
  tableOut<-data.frame(covNames=predictPlot$covNames,pvalue=predictPlot$pvalue,effectsize=predictPlot$effectsize)
  colnames(tableOut)<-c("Covariate","p-value","Effect size")
  }
  tableOut
})

