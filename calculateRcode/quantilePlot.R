quantilePlot<-function(predictPlot, dataTable, contcov, mainTitle,covNum, indicator=NULL, strataName=NULL,strataLevel=NULL){
  if(identical(input$CoxorFG,"noComp")){
    if(!identical(input$strata,"yes")){
      p <- plot(predictPlot$time,predictPlot$surv[,covNum],type="l",
                main =paste0(mainTitle,"(",input$covName,"=",round(contcov[covNum],3), ")"),
                ylab = "",
                xlab = "Time",pch=19,cex=0.5,lwd=3,ylim=c(0,1))
      if(!identical(input$noCompSelect,"rf")){
      p<-p+lines(predictPlot$time,predictPlot$upper[,covNum],type="l",lty=3,pch=19,cex=0.5)
      p<-p+lines(predictPlot$time,predictPlot$lower[,covNum],type="l",lty=3,pch=19,cex=0.5)
      }
    }else{
      xvalue<- predictPlot$time
      p <- plot(xvalue,predictPlot$surv[[indicator]][,covNum],type="l",
                main = paste0(mainTitle,"(",input$covName,"=",round(contcov[covNum],3), ") when ",
                              strataName,"=",strataLevel),
                ylab = "",
                xlab = "Time",pch=19,cex=0.5,lwd=3,ylim=c(0,1))
      p<-p+lines(xvalue,predictPlot$upper[[indicator]][,covNum],type="l",lty=3,pch=19,cex=0.5)
      p<-p+lines(xvalue,predictPlot$lower[[indicator]][,covNum],type="l",lty=3,pch=19,cex=0.5)
    }
  }else{
    p <- plot(predictPlot$time,predictPlot$surv[,covNum],type="l",
              main =paste0(mainTitle,"(",input$covName,"=",round(contcov[covNum],3), ")"),
              ylab = "",
              xlab = "Time",pch=19,cex=0.5,lwd=3,
              ylim=c(0,max(predictPlot$surv)))
  }
  p
}