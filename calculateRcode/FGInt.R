timecov1<-input$time1
timecov2<-input$time2
dataTable[,timecov2]<-ifelse(dataTable[,timecov1]==dataTable[,timecov2],
                             dataTable[,timecov2]+sd(dataTable[,timecov1],na.rm=TRUE)*10^(-3),
                             dataTable[,timecov2])
ctime<-c(dataTable[,timecov1],dataTable[,timecov2])
ctime<-ctime[!is.na(ctime)]
ctime<-ctime[is.finite(ctime)]
ctime<-c(ctime[ctime<input$timeSlider],input$timeSlider)
ctime<-unique(ctime)
ctime<-ctime[order(ctime)]
if(ctime[1]>0){
  ctime<-c(0,ctime)
}
predMat<-NULL
exportPlot<-NULL
if(identical(input$otherCov,"Yes")){
  rtable=hot_to_r(input$table)
  validateData(rtable,dataTable)
  fgrModel<-eval(parse(text=paste("model.frame(~",contcov,"+",
                                  paste(ifelse(rtable$CateorNot,sapply(rtable$otherCovName,putPara),rtable$otherCovName),
                                        collapse="+"),
                                  ",data=dataTable)")))
  fgrMatrix<-eval(parse(text=paste("model.matrix(intccr::Surv2(v =",timecov1,",u =",timecov2,",event=",stacov,")~",contcov,"+",
                                   paste(ifelse(rtable$CateorNot,sapply(rtable$otherCovName,putPara),
                                                rtable$otherCovName),collapse="+"),
                                   ",data=dataTable)")))
  fgrMatrix<-data.frame(fgrMatrix[,-1])
  fg2cov<-colnames(fgrMatrix)
  fgrMatrix$time1<-dataTable[,timecov1]
  fgrMatrix$time2<-dataTable[,timecov2]
  fgrMatrix$event<-dataTable[,stacov]
  ff <- as.formula(paste0("intccr::Surv2(v=time1,u=time2, event=event) ~ ",
                          paste(fg2cov, collapse = "+")))
  fit<-ciregic(ff,data=fgrMatrix, alpha = c(0, 1), nboot = 0, do.par = FALSE)
  newdata0<-data.frame(cbind(seq(input$covSlider[1],input$covSlider[2],length.out = 21),
                            matrix(rep(rtable$value4Plot,21),byrow = TRUE,nrow=21)))
  colnames(newdata0)<-c(contcov,rtable$otherCovName)
  newdata0[,contcov]<-as.numeric(newdata0[,contcov])
  for(i in 1:length(rtable$CateorNot)){
    if(!rtable$CateorNot[i]){
      newdata0[,rtable$otherCovName[i]]<-as.numeric(newdata0[,rtable$otherCovName[i]])
    }
  }
  xlevels <- .getXlevels(attr(fgrModel,"terms"),fgrModel)
  newdata<-eval(parse(text=paste("data.frame(model.matrix(~",contcov,"+",
                                  paste(ifelse(rtable$CateorNot,sapply(rtable$otherCovName,putPara),
                                               rtable$otherCovName),collapse="+"),
                                  ",newdata0,xlev = xlevels))")))
  newdata<-newdata[-1]
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
  fit <-eval(parse(text=paste('ciregic(formula = intccr::Surv2(v =',timecov1,',u =',timecov2,',event=',stacov,') ~ ',
                              contcov,',data=dataTable, alpha = c(0, 1), nboot = 0, do.par = FALSE)')))
  newdata<- matrix(seq(input$covSlider[1],input$covSlider[2],length.out = 21),ncol=1)
  exportPlot$covNames<-input$covName
} 

for(i in 1:21){
 pred <- predict(object = fit, covp = as.numeric(newdata[i,]), times = ctime)
 predMat<-cbind(predMat,pred$cif1)
}

exportPlot$surv<-predMat
exportPlot$time<-ctime
exportPlot$pvalue<-summary(fit)$p[1:length(exportPlot$covNames)]
exportPlot$effectsize<-summary(fit)$coefficients[1:length(exportPlot$covNames)]