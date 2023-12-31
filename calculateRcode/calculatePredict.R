putPara<-function(x){
  paste0("factor(",x,")")
}

calculatePredict<-function(){
  dataTable<-filedata()
  exportPlot<-NULL
  stacov<-input$status
  contcov<-input$covariate
  if(identical(input$CoxorFG,"noComp")){
    if(identical(input$intCen,"noint")){
      timecov<-input$time
      if(identical(input$noCompSelect,"Cox")){
        if(!identical(input$strata,"yes")){
          source("calculateRcode/CoxNoIntNoStrata.R",local=TRUE)
        }else{
          strataCov<-input$strataCov
          if(!identical(is.factor(dataTable[,strataCov]),TRUE)){
            dataTable[,strataCov]<-as.factor(dataTable[,strataCov])
          }
          source("calculateRcode/CoxNoIntStrata.R",local=TRUE)
        }
        exportPlot$time<-time
        exportPlot$pvalue<-round(summary(cox)$coefficients[,5],3)
        exportPlot$effectsize<-round(summary(cox)$coef[,1],2)
      }else{
        if(identical(input$noCompSelect,"rf")){
          if(!identical(input$numTree>0,TRUE)){
            warning(safeError("The number of trees must be an integer greater than 0."))
          }
          source("calculateRcode/rf.R",local=TRUE)
        }else{
          source("calculateRcode/paraNoIntNoStrata.R",local=TRUE)
        }
      }
    }else{
      timecov1<-input$time1
      timecov2<-input$time2
      dataTable[,timecov2]<-ifelse(dataTable[,timecov2]==dataTable[,timecov1],
                                   dataTable[,timecov1]*1.001,
                                   dataTable[,timecov2])
      if(sum(is.na(dataTable[,timecov2]))>0){
        dataTable[,timecov2]<-ifelse(is.na(dataTable[,timecov2]),
                                     rep(max(dataTable[,timecov2],na.rm=TRUE)*1000,nrow(dataTable)),
                                     dataTable[,timecov2])
      }
      if(sum(!is.infinite(dataTable[,timecov2]))>0){
        dataTable[,timecov2]<-ifelse(is.finite(dataTable[,timecov2]),dataTable[,timecov2],
                                     rep(max(dataTable[,timecov2],na.rm=TRUE)*1000,nrow(dataTable)))
      }
      if(identical(input$noCompSelect,"Cox")){
        if(!identical(input$strata,"yes")){
          source("calculateRcode/CoxIntNoStrata.R",local=TRUE)
        }else{
          strataCov<-input$strataCov
          if(!identical(is.factor(dataTable[,strataCov]),TRUE)){
            dataTable[,strataCov]<-as.factor(dataTable[,strataCov])
          }
          source("calculateRcode/CoxIntStrata.R",local=TRUE)
        }
        exportPlot$pvalue<-round(summary(cox)$coef[,4],3)
        exportPlot$effectsize<-round(summary(cox)$coef[,1],2)
        exportPlot$time<-ctime
      }else{
        source("calculateRcode/paraIntNoStrata.R",local=TRUE)
      }
    }

  }else{
    # Fine and gray model
    if(identical(input$intCen,"noint")){
      source("calculateRcode/FGNoInt.R",local=TRUE)
    }else{
      source("calculateRcode/FGInt.R",local=TRUE)
    }
  }
  return(exportPlot)
}