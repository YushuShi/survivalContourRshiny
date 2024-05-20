if(identical(input$otherCov,"Yes")){
  rtable=hot_to_r(input$table)
  validateData(rtable,dataTable)
  if(!identical(input$noCompSelect,"Spline")){
    parModel<-eval(parse(text=paste0('flexsurvreg(Surv(',timecov1,',',timecov2,',type="interval2") ~ ',contcov,'+',
                                      paste(ifelse(rtable$CateorNot,sapply(rtable$otherCovName,putPara),rtable$otherCovName),collapse="+"),',data=dataTable,dist="',input$noCompSelect,
                                      '")')))
  }else{

    parModel<-eval(parse(text=paste0('flexsurvspline(Surv(',timecov1,',',timecov2,',type="interval2") ~ ',contcov,'+',
                                      paste(ifelse(rtable$CateorNot,sapply(rtable$otherCovName,putPara),rtable$otherCovName),collapse="+"),',data=dataTable,k=4)')))
  }
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
  if(!identical(input$noCompSelect,"Spline")){
    parModel <-eval(parse(text=paste0('flexsurvreg(Surv(',timecov1,',',timecov2,',type="interval2") ~ ',contcov,
                                   ',data=dataTable,dist="',input$noCompSelect,
                                   '")')))
  }else{
    parModel <-eval(parse(text=paste0('flexsurvspline(Surv(',timecov1,',',timecov2,',type="interval2") ~ ',contcov,
                                      ',data=dataTable,k=4)')))
  }
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
if(ctime[1]==0){
  ctime<-ctime[-1]
}

predictPlot0<-predict(parModel,newdata = newdata,type="survival",conf.int=TRUE,times=ctime)

predictPlot<-list.stack(predictPlot0$.pred)

#print(str(tidy(parModel)$pvalue))
#print(str(summary(parModel)))
exportPlot$surv<-matrix(predictPlot$.pred_survival,nrow=length(ctime))
exportPlot$surv<-rbind(rep(1,ncol(exportPlot$surv)),exportPlot$surv)
exportPlot$upper<-matrix(predictPlot$.pred_upper,nrow=length(ctime))
exportPlot$upper<-rbind(rep(1,ncol(exportPlot$upper)),exportPlot$upper)
exportPlot$lower<-matrix(predictPlot$.pred_lower,nrow=length(ctime))
exportPlot$lower<-rbind(rep(1,ncol(exportPlot$lower)),exportPlot$lower)
exportPlot$pvalue<-round(tidy(parModel)$p.value[names(parModel$datameans)],2)
exportPlot$effectsize<-round(tidy(parModel)$estimate[names(parModel$datameans)],2)
exportPlot$time<-c(0,ctime)