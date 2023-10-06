if(input$otherCov=="Yes"){
  rtable=hot_to_r(input$table)
  validateData(rtable,dataTable)
  cox <-eval(parse(text=paste('phreg(Surv(',timecov1,',',timecov2,',type="interval2") ~ ',contcov,"+",
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
  cox <-eval(parse(text=paste('phreg(Surv(',timecov1,',',timecov2,',type="interval2") ~ ',contcov,',dataTable)')))
  newdata<-data.frame(tempcov=seq(input$covSlider[1],input$covSlider[2],length.out = 21))
  colnames(newdata)<-contcov
  exportPlot$covNames<-input$covName
}

ctime<-c(dataTable[,timecov1],dataTable[,timecov2])
ctime<-ctime[!is.na(ctime)]
ctime<-ctime[is.finite(ctime)]
ctime<-c(ctime[ctime<input$timeSlider],input$timeSlider)
ctime<-unique(ctime)
ctime<-ctime[order(ctime)]
if(ctime[1]>0){
  ctime<-c(0,ctime)
}
source("calculateRcode/predictPhreg.R",local=TRUE)
predictPlot<-predictPhreg(cox,newdata = newdata,times=ctime)
exportPlot$surv<-t(predictPlot$surv)
exportPlot$upper<-t(predictPlot$surv.upper)
exportPlot$lower<-t(predictPlot$surv.lower)

#print(predictPlot$surv)

#print(predictPlot$surv.upper)