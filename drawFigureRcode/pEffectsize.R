output$pEffectsize<- renderTable({
  input$getPlot
  tableOut<-NULL

  dataTable<-filedata()
  predictPlot<-calculatePredict()
  covNames<-predictPlot$covNames
  #print(covNames)
  covNames[covNames==""]<-input$covariate
  if(!identical(input$noCompSelect,"rf")){
  tableOut<-data.frame(covNames=covNames,pvalue=predictPlot$pvalue,effectsize=predictPlot$effectsize)
  }else{
    tableOut<-data.frame(covNames=covNames,pvalue=NA,effectsize=NA)
  }
  colnames(tableOut)<-c("Covariate","p-value","Effect size")

  tableOut
})

