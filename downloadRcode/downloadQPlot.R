output$downloadQPlot <- downloadHandler(
  filename = "quantile.zip",
  content = function(file) {
    predictPlot<-calculatePredict()
    dataTable<-filedata()
    contcov<-seq(input$covSlider[1],input$covSlider[2],length.out = 21)
    heading<-ifelse(identical(input$CoxorFG,"Cox"),"Predicted survival at the","Predicted CIF at the")
    quantile0 <- tempfile("Min_",fileext = ".png")
    if(!identical(input$strata,"yes")){
      png(quantile0,width = 800,height = 400,pointsize = 20)
      par(mar=c(3.1, 3.1, 2.1, 1.1), mgp=c(1.5, 0.5, 0), oma=c(0, 0, 0, 0))
      quantilePlot(predictPlot, dataTable, contcov, paste(heading,"min of the continuous \n covariate"),1)
      dev.off()  
    }else{
      strataList<-unique(dataTable[,input$strataCov])
      strataList<-strataList[!is.na(strataList)]
      png(quantile0,width = 800,height = 400*length(strataList),pointsize = 20)
      par(mar=c(3.1, 3.1, 2.1, 1.1), mgp=c(1.5, 0.5, 0), oma=c(0, 0, 0, 0),mfrow=c(length(strataList),1))
      for(i in 1:length(strataList)){
        quantilePlot(predictPlot, dataTable, contcov, paste(heading,"min of the continuous \n covariate"),1,i,input$strataName,strataList[i])
      }
      dev.off()  
    }
    
    quantile25 <- tempfile("FirstQuantile_",fileext = ".png")
    if(!identical(input$strata,"yes")){
      png(quantile25,width = 800,height = 400,pointsize = 20)
      par(mar=c(3.1, 3.1, 2.1, 1.1), mgp=c(1.5, 0.5, 0), oma=c(0, 0, 0, 0))
      quantilePlot(predictPlot, dataTable, contcov, paste(heading,"first quantile of the continuous \n covariate"),6)
      dev.off()  
    }else{
      strataList<-unique(dataTable[,input$strataCov])
      strataList<-strataList[!is.na(strataList)]
      png(quantile25,width = 800,height = 400*length(strataList),pointsize = 20)
      par(mar=c(3.1, 3.1, 2.1, 1.1), mgp=c(1.5, 0.5, 0), oma=c(0, 0, 0, 0),mfrow=c(length(strataList),1))
      for(i in 1:length(strataList)){
        quantilePlot(predictPlot, dataTable, contcov, paste(heading,"first quantile of the continuous \n covariate"),6,i,input$strataName,strataList[i])
      }
      dev.off()  
    }
    
    quantile50 <- tempfile("Median_",fileext = ".png")
    if(!identical(input$strata,"yes")){
      png(quantile50,width = 800,height = 400,pointsize = 20)
      par(mar=c(3.1, 3.1, 2.1, 1.1), mgp=c(1.5, 0.5, 0), oma=c(0, 0, 0, 0))
      quantilePlot(predictPlot, dataTable, contcov, paste(heading,"median of the continuous \n covariate"),11)
      dev.off()  
    }else{
      strataList<-unique(dataTable[,input$strataCov])
      strataList<-strataList[!is.na(strataList)]
      png(quantile50,width = 800,height = 400*length(strataList),pointsize = 20)
      par(mar=c(3.1, 3.1, 2.1, 1.1), mgp=c(1.5, 0.5, 0), oma=c(0, 0, 0, 0),mfrow=c(length(strataList),1))
      for(i in 1:length(strataList)){
        quantilePlot(predictPlot, dataTable, contcov, paste(heading,"median of the continuous \n covariate"),11,i,input$strataName,strataList[i])
      }
      dev.off()  
    }
    
    quantile75 <- tempfile("ThirdQuantile_",fileext = ".png")
    if(!identical(input$strata,"yes")){
      png(quantile75,width = 800,height = 400,pointsize = 20)
      par(mar=c(3.1, 3.1, 2.1, 1.1), mgp=c(1.5, 0.5, 0), oma=c(0, 0, 0, 0))
      quantilePlot(predictPlot, dataTable, contcov, paste(heading,"third quantile of the continuous \n covariate"),16)
      dev.off()  
    }else{
      strataList<-unique(dataTable[,input$strataCov])
      strataList<-strataList[!is.na(strataList)]
      png(quantile75,width = 800,height = 400*length(strataList),pointsize = 20)
      par(mar=c(3.1, 3.1, 2.1, 1.1), mgp=c(1.5, 0.5, 0), oma=c(0, 0, 0, 0),mfrow=c(length(strataList),1))
      for(i in 1:length(strataList)){
        quantilePlot(predictPlot, dataTable, contcov, paste(heading,"third quantile of the continuous \n covariate"),16,i,input$strataName,strataList[i])
      }
      dev.off()  
    }
    
    quantile100 <- tempfile("Max_",fileext = ".png")
    if(!identical(input$strata,"yes")){
      png(quantile100,width = 800,height = 400,pointsize = 20)
      par(mar=c(3.1, 3.1, 2.1, 1.1), mgp=c(1.5, 0.5, 0), oma=c(0, 0, 0, 0))
      quantilePlot(predictPlot, dataTable, contcov, paste(heading,"max of the continuous \n covariate"),21)
      dev.off()  
    }else{
      strataList<-unique(dataTable[,input$strataCov])
      strataList<-strataList[!is.na(strataList)]
      png(quantile100,width = 800,height = 400*length(strataList),pointsize = 20)
      par(mar=c(3.1, 3.1, 2.1, 1.1), mgp=c(1.5, 0.5, 0), oma=c(0, 0, 0, 0),mfrow=c(length(strataList),1))
      for(i in 1:length(strataList)){
        quantilePlot(predictPlot, dataTable, contcov, paste(heading,"max of the continuous \n covariate"),21,i,input$strataName,strataList[i])
      }
      dev.off()  
    }
    
    zipr(file,c(quantile0,quantile25,quantile50,quantile75,quantile100))
  }
)