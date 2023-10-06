if(input$otherCov=="Yes"){
  rtable=hot_to_r(input$table)
  validateData(rtable,dataTable)
  if(!identical(input$noCompSelect,"Spline")){
  parModel <-eval(parse(text=paste0('flexsurvreg(Surv(',timecov,',',stacov,') ~ ',contcov,'+',
                                    paste(ifelse(rtable$CateorNot,sapply(rtable$otherCovName,putPara),rtable$otherCovName),collapse="+"),',data=dataTable, dist="',input$noCompSelect,'")')))
  }else{
  parModel <-eval(parse(text=paste0('flexsurvspline(Surv(',timecov,',',stacov,') ~ ',contcov,'+',
                                      paste(ifelse(rtable$CateorNot,sapply(rtable$otherCovName,putPara),rtable$otherCovName),collapse="+"),',data=dataTable, k=4)')))
  }

#  parModel <-flexsurvreg(Surv(time,status) ~ karno,dist="gengamma.orig",dataTable)
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
  parModel <-eval( parse(text=paste0('flexsurvreg(Surv(',timecov,',',stacov,') ~ ',contcov,',data=dataTable,dist="',input$noCompSelect,'")')))
  }else{
    parModel <-eval( parse(text=paste0('flexsurvspline(Surv(',timecov,',',stacov,') ~ ',contcov,',data=dataTable,k=4)')))
  }
  newdata<-data.frame(tempcov=seq(input$covSlider[1],input$covSlider[2],length.out = 21))
  colnames(newdata)<-contcov
  exportPlot$covNames<-input$covName
}


time<-parModel$data$Y[,"time"]
time<-c(time[time<input$timeSlider],input$timeSlider)
time<-unique(time)
time<-time[order(time)]
predictPlot0<-predict(parModel,newdata = newdata,type="survival",conf.int=TRUE,times=time)
print(str(predictPlot0))
predictPlot<-list.stack(predictPlot0$.pred)
str(predictPlot)
exportPlot$surv<-matrix(predictPlot$.pred_survival,nrow=length(time))
exportPlot$surv<-rbind(rep(1,ncol(exportPlot$surv)),exportPlot$surv)
exportPlot$upper<-matrix(predictPlot$.pred_upper,nrow=length(time))
exportPlot$upper<-rbind(rep(1,ncol(exportPlot$upper)),exportPlot$upper)
exportPlot$lower<-matrix(predictPlot$.pred_lower,nrow=length(time))
exportPlot$lower<-rbind(rep(1,ncol(exportPlot$lower)),exportPlot$lower)
exportPlot$pvalue<-round(tidy(parModel)$p.value[names(parModel$datameans)],2)
exportPlot$effectsize<-round(tidy(parModel)$estimate[names(parModel$datameans)],2)
exportPlot$time<-c(0,time)