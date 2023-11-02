strataCov<-input$strataCov
if(identical(input$otherCov,"Yes")){
  rtable=hot_to_r(input$table)
  validateData(rtable,dataTable)
  cox <-eval(parse(text=paste("coxph(Surv(",timecov,",",stacov,") ~ strata(",strataCov,")+",contcov,"+",
                              paste(ifelse(rtable$CateorNot,sapply(rtable$otherCovName,putPara),rtable$otherCovName),
                                    collapse="+"),
                              ",dataTable)")))
  newdata<-data.frame(cbind(rep(seq(input$covSlider[1],input$covSlider[2],length.out = 21),
                                length(unique(dataTable[,strataCov]))),
                            rep(unique(dataTable[,strataCov]),each=21),
                            matrix(rep(rtable$value4Plot,21*length(unique(dataTable[,strataCov])))
                                   ,byrow = TRUE,ncol=length(rtable$otherCovName))))
  colnames(newdata)<-c(contcov,strataCov,rtable$otherCovName)
  newdata[,contcov]<-as.numeric(newdata[,contcov])
  newdata[,strataCov]<-eval(parse(text=paste0("as.",typeof(dataTable[,strataCov]),"(newdata[,strataCov])")))
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
  cox <-eval(parse(text=paste("coxph(Surv(",timecov,",",stacov,") ~ strata(",strataCov,")+",contcov,",dataTable)")))
  newdata<-data.frame(cbind(rep(seq(input$covSlider[1],input$covSlider[2],length.out = 21),
                                length(unique(dataTable[,strataCov]))),
                            rep(unique(dataTable[,strataCov]),each=21)))
  colnames(newdata)<-c(contcov,strataCov)
  newdata[,contcov]<-as.numeric(newdata[,contcov])
  newdata[,strataCov]<-eval(parse(text=paste0("as.",typeof(dataTable[,strataCov]),"(newdata[,strataCov])")))
  exportPlot$covNames<-input$covName
}
time<-cox$y[,1]
time<-c(time[time<input$timeSlider],input$timeSlider)
time<-c(0,unique(time))
time<-time[order(time)]
predictPlot<-survfit(cox,newdata = newdata)
summaryPred<-summary(predictPlot,times=time,extend=TRUE)
for(i in 1:length(unique(dataTable[,strataCov]))){
  rangelimit<-(length(time)*21*(i-1)+1):(length(time)*21*i)
  exportPlot$surv[[i]]<-matrix(summaryPred$surv[rangelimit],ncol=21)
  exportPlot$upper[[i]]<-matrix(summaryPred$upper[rangelimit],ncol=21)
  exportPlot$lower[[i]]<-matrix(summaryPred$lower[rangelimit],ncol=21)   
}