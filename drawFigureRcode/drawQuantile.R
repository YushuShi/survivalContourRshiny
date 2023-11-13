output$drawQuantile<- renderImage({
  input$getPlot
  heading<-ifelse(identical(input$CoxorFG,"noComp"),"Predicted survival at the","Predicted CIF at the")
  predictPlot<-calculatePredict()
  dataTable<-filedata()
  contcov<-seq(input$covSlider[1],input$covSlider[2],length.out = 21)

  outFile <- tempfile(fileext = ".png")
  if(!identical(input$strata,"yes")){
    heightScale<-1
  }else{
    temp<-unique(dataTable[,input$strataCov])
    temp<-temp[!is.na(temp)]
    heightScale<-length(temp)
  }

  png(outFile,width = 800,height = 800*heightScale,pointsize = 20)
  par(mfrow=c(3*heightScale,2),
      mar=c(3.1, 3.1, 4.1, 1.1),
      mgp=c(1.5, 0.5, 0),
      oma=c(0, 0, 0, 0))
  if(!identical(input$strata,"yes")){
    quantilePlot(predictPlot, dataTable, contcov, paste(heading,"min of the \n continuous covariate"),1)
    quantilePlot(predictPlot, dataTable, contcov, paste(heading,"first quantile of the \n continuous covariate"),6)
    quantilePlot(predictPlot, dataTable, contcov, paste(heading,"median of the \n continuous covariate"),11)
    quantilePlot(predictPlot, dataTable, contcov, paste(heading,"third quantile of the \n continuous covariate"),16)
    quantilePlot(predictPlot, dataTable, contcov, paste(heading,"max of the \n continuous covariate"),21)
    plot(1, type="n", axes=F, xlab="", ylab="")  
  }else{
    strataList<-unique(dataTable[,input$strataCov])
    strataList<-strataList[!is.na(strataList)]
    strataName<-ifelse(identical(input$strataName,""),input$strataCov,input$strataName)
    for(i in 1:length(strataList)){
      quantilePlot(predictPlot, dataTable, contcov, paste(heading,"min of the \n continuous covariate"),1,i, strataName,strataList[i])
      quantilePlot(predictPlot, dataTable, contcov, paste(heading,"first quantile of the \n continuous covariate"),6,i,strataName,strataList[i])
      quantilePlot(predictPlot, dataTable, contcov, paste(heading,"median of the \n continuous covariate"),11,i, strataName,strataList[i])
      quantilePlot(predictPlot, dataTable, contcov, paste(heading,"third quantile of the \n continuous covariate"),16,i, strataName,strataList[i])
      quantilePlot(predictPlot, dataTable, contcov, paste(heading,"max of the \n continuous covariate"),21,i, strataName,strataList[i])
      plot(1, type="n", axes=F, xlab="", ylab="")        
    }
  }
  dev.off()

  list(src = outFile,
    width = 800,
    height = 800*heightScale,
    contentType = "image/png",
    alt = "This is alternate text"
  )
}, deleteFile = TRUE)