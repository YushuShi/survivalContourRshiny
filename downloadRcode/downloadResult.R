output$downloadResult <- downloadHandler(
  filename = function() {
    ifelse(!identical(input$strata,"yes"),paste("SurvContour Plot-", Sys.Date(), ".csv", sep=""),"plotinfo.zip")
  },
  content = function(file) {
    predictResult<- calculatePredict()
    if(!identical(input$strata,"yes")){
      dataExport<-cbind(predictResult$time,predictResult$surv)
      colnames(dataExport)<-c("time",paste(input$covName,"=",
                                           seq(input$covSlider[1],input$covSlider[2],length.out = 21)))
      write.csv(dataExport, file, row.names = FALSE)
    }else{
      dataTable<-filedata()
      strataList<-unique(dataTable[,input$strataCov])
      dataExport<-NULL
      cumTable<-NULL
      for(i in 1:length(strataList)){
        tempTable <- tempfile(paste("Strata_",strataList[i],"_",sep=""),fileext = ".csv")
        dataExport<-cbind(predictResult$time,predictResult$surv[[i]])
        colnames(dataExport)<-c(paste(input$strataName,"=",strataList[i]),
                                paste(input$covName,"=",seq(input$covSlider[1],input$covSlider[2],length.out = 21)))
        write.csv(dataExport, tempTable, row.names = FALSE)
        cumTable<-c(cumTable,tempTable)
      }
      zipr(file,cumTable)
    }
  }
)