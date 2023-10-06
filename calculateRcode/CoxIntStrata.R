strataCov<-input$strataCov

if(input$otherCov=="Yes"){
  rtable=hot_to_r(input$table)
  validateData(rtable,dataTable)
  cox <-eval(parse(text=paste('phreg(Surv(',timecov1,',',timecov2,',type="interval2") ~ strata(',strataCov,')+',contcov,"+",
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
  cox <-eval(parse(text=paste('phreg(Surv(',timecov1,',',timecov2,',type="interval2") ~ strata(',strataCov,')+',contcov,',dataTable)')))
  newdata<-data.frame(tempcov=rep(seq(input$covSlider[1],input$covSlider[2],length.out = 21),length(unique(dataTable[,strataCov]))),
                      stratacov=rep(unique(dataTable[,strataCov]),each=21))
  colnames(newdata)<-c(contcov,strataCov)
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
for(i in 1:length(unique(dataTable[,strataCov]))){
  rangelimit<-seq((i-1)*21+1,i*21)
  exportPlot$surv[[i]]<-t(predictPlot$surv[rangelimit,])
  exportPlot$upper[[i]]<-t(predictPlot$surv.upper[rangelimit,])
  exportPlot$lower[[i]]<-t(predictPlot$surv.lower[rangelimit,])
}