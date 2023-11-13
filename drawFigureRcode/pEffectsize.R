output$pEffectsize<- renderTable({
  input$getPlot
  tableOut<-NULL
  if(!identical(input$noCompSelect,"rf")){
  dataTable<-filedata()
  predictPlot<-calculatePredict()
  covNames<-predictPlot$covNames
  #print(covNames)
  covNames[covNames==""]<-input$covariate
  tableOut<-data.frame(covNames=covNames,pvalue=predictPlot$pvalue,effectsize=predictPlot$effectsize)
  colnames(tableOut)<-c("Covariate","p-value","Effect size")
  }
  tableOut
})

