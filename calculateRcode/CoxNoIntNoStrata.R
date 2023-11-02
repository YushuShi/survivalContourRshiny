if(identical(input$otherCov,"Yes")){
  rtable=hot_to_r(input$table)
  validateData(rtable,dataTable)
  cox <-eval(parse(text=paste("coxph(Surv(",timecov,",",stacov,") ~ ",contcov,"+",
                              paste(ifelse(rtable$CateorNot,sapply(rtable$otherCovName,putPara),rtable$otherCovName),collapse="+"),
                              ",dataTable)")))
  newdata<-data.frame(cbind(seq(input$covSlider[1],input$covSlider[2],length.out = 21),
                            matrix(rep(rtable$value4Plot,21),byrow = TRUE,nrow=21)))
  colnames(newdata)<-c(contcov,rtable$otherCovName)
  newdata[,contcov]<-as.numeric(newdata[,contcov])
  for(i in 1:length(rtable$CateorNot)){
    if(!rtable$CateorNot[i]){
      newdata[,rtable$otherCovName[i]]<-as.numeric(newdata[,rtable$otherCovName[i]])
    }
  }
  otherCovList<-NULL
  for(i in 1:length(rtable$otherCovName)){
    if(rtable$CateorNot[i]){
      covLevel<-levels(factor(dataTable[,rtable$otherCovName[i]]))
      otherCovList<-c(otherCovList,paste0(rtable$otherCovName[i],": ",
                                         covLevel[2:length(covLevel)]," vs ",covLevel[1]))
    }else{
      otherCovList<-c(otherCovList,rtable$otherCovName[i])
    }
  }
  exportPlot$covNames<-c(input$covName,otherCovList)
}else{
  cox <-eval( parse(text=paste("coxph(Surv(",timecov,",",stacov,") ~ ",contcov,",dataTable)")))
  newdata<-data.frame(tempcov=seq(input$covSlider[1],input$covSlider[2],length.out = 21))
  colnames(newdata)<-contcov
  exportPlot$covNames<-input$covName
}
predictPlot<-survfit(cox,newdata = newdata)

time<-cox$y[,1]
time<-c(time[time<input$timeSlider],input$timeSlider)
time<-c(0,unique(time))
time<-time[order(time)]
summaryPred<-summary(predictPlot,times=time,extend=TRUE)
exportPlot$surv<-summaryPred$surv
exportPlot$upper<-summaryPred$upper
exportPlot$lower<-summaryPred$lower    
