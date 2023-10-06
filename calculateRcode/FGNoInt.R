timecov<-input$time
ctime<-dataTable[,timecov]
ctime<-c(ctime[ctime<input$timeSlider],input$timeSlider)
ctime<-unique(ctime)
ctime<-ctime[order(ctime)]
if(input$otherCov=="Yes"){
  rtable=hot_to_r(input$table)
  validateData(rtable,dataTable)
  fgrModel<-eval(parse(text=paste("model.frame(~",contcov,"+",
                       paste(ifelse(rtable$CateorNot,sapply(rtable$otherCovName,putPara),rtable$otherCovName),
                             collapse="+"),
                       ",data=dataTable)")))
  fgrMatrix<-eval(parse(text=paste("model.matrix(Hist(",timecov,",",stacov,")~",contcov,"+",
                                  paste(ifelse(rtable$CateorNot,sapply(rtable$otherCovName,putPara),
                                               rtable$otherCovName),collapse="+"),
                                  ",data=dataTable)")))
  fgrMatrix<-data.frame(fgrMatrix[,-1])
  fg2cov<-colnames(fgrMatrix)
  fgrMatrix$time<-dataTable[,timecov]
  fgrMatrix$event<-dataTable[,stacov]
  ff <- as.formula(paste0("Hist(time, event) ~ ",
                          paste(fg2cov, collapse = "+")))
  fgr<-FGR(ff,data=fgrMatrix, cause=1)
  newdata<-data.frame(cbind(seq(input$covSlider[1],input$covSlider[2],length.out = 21),
                            matrix(rep(rtable$value4Plot,21),byrow = TRUE,nrow=21)))
  colnames(newdata)<-c(contcov,rtable$otherCovName)
  newdata[,contcov]<-as.numeric(newdata[,contcov])
  for(i in 1:length(rtable$CateorNot)){
    if(!rtable$CateorNot[i]){
      newdata[,rtable$otherCovName[i]]<-as.numeric(newdata[,rtable$otherCovName[i]])
    }
  }
  xlevels <- .getXlevels(attr(fgrModel,"terms"),fgrModel)
  newdata2<-eval(parse(text=paste("data.frame(model.matrix(~",contcov,"+",
                                  paste(ifelse(rtable$CateorNot,sapply(rtable$otherCovName,putPara),
                                               rtable$otherCovName),collapse="+"),
                                  ",newdata,xlev = xlevels))")))
  newdata2<-newdata2[-1]
  predictFG<-predictRisk(fgr,times=c(0,ctime),newdata=newdata2)
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
  fgr<-eval(parse(text=paste("FGR(Hist(",timecov,",",stacov,")~",contcov,", data=dataTable, cause=1)")))
  newdata<-data.frame(tempcov=seq(input$covSlider[1],input$covSlider[2],length.out = 21))
  colnames(newdata)<-contcov
  predictFG<-predictRisk(fgr,times=c(0,ctime),newdata=newdata)
  exportPlot$covNames<-input$covName
}

exportPlot$surv<-t(predictFG)
exportPlot$time<-c(0,ctime)
exportPlot$pvalue<-round(summary(fgr)$coef[,5],3)
exportPlot$effectsize<-round(summary(fgr)$coef[,1],2)