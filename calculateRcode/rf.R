if(identical(input$otherCov,"Yes")){
  rtable=hot_to_r(input$table)
  validateData(rtable,dataTable)
  modelTrained<-eval(parse(text=paste("rfsrc(Surv(",timecov,",",stacov,") ~ ",contcov,"+",
                              paste(ifelse(rtable$CateorNot,sapply(rtable$otherCovName,putPara),rtable$otherCovName),collapse="+")
                              ,", ntree=",input$numTree
                              ,",dataTable)")))
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
  modelTrained<-eval( parse(text=paste("rfsrc(Surv(",timecov,",",stacov,") ~ ",contcov
                                       ,", ntree=",input$numTree,
                                       ",dataTable)")))
  newdata<-data.frame(tempcov=seq(input$covSlider[1],input$covSlider[2],length.out = 21))
  colnames(newdata)<-contcov
  exportPlot$covNames<-input$covName
}
newdata<- newdata %>%
  mutate(across(all_of(colnames(newdata)[!colnames(newdata)%in% c(timecov,stacov)]), as.integer))
predictOutcome<-predict(modelTrained,newdata)

time<-predictOutcome$time.interest
contourZ<-predictOutcome$survival#x axis is time
if(min(predictOutcome$time.interest)>0){
  time<-c(0,time)
  contourZ<-cbind(rep(1,nrow(contourZ)),contourZ)
}
exportPlot$time<-time
exportPlot$surv<-t(contourZ)

