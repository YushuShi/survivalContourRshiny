contourPlot<-function(predictPlot, dataTable, contcov, indicator=NULL, strataName=NULL,strataLevel=NULL){
  colSche<-input$colSche
  if(!identical(input$strata,"yes")){
    p <- filled.contour(
      x = predictPlot$time,
      y = contcov,
      z = predictPlot$surv,
      nlevels=11,
      main = ifelse(input$CoxorFG=="Cox","Contour Plot of the Predicted Survival Probability",
                    "Contour Plot of the Predicted Cumulative Incidence Function"),
      cex.main=ifelse(input$CoxorFG=="Cox",1,0.8),
      #ylab = input$covName,
      xlab = "Time",
      color.palette = ifelse(colSche=="default",function(n) hcl.colors(n, "YlOrRd", rev = TRUE),get(colSche)),
      key.title = title(main = ifelse(input$CoxorFG=="Cox","Surival\n Probability",
                                      "Probability \n of Having \n Primary Event"),cex.main=0.5))
    mar.orig <- par("mar")
    w <- (3 + mar.orig[2]) * par("csi") * 2.54
    layout(matrix(c(2, 1), nc = 2), widths = c(1, lcm(w)))
    p<-p+contour(x = predictPlot$time,
                 y = contcov,
                 z = predictPlot$surv,
                 nlevels=11,
                 drawlabels =TRUE, 
                 axes = FALSE, frame.plot = FALSE, add = TRUE)
  }else{
    xvalue<- predictPlot$time
    p <- filled.contour(
      x = xvalue,
      y = contcov,
      z = predictPlot$surv[[indicator]],
      nlevels=11,
      main = paste("Contour Plot of the Predicted Survival Probability \n when",strataName,"=",strataLevel),
      cex.main=0.8,
      ylab = input$covName,
      xlab = "Time",
      color.palette = ifelse(colSche=="default",function(n) hcl.colors(n, "YlOrRd", rev = TRUE),get(colSche)),
      key.title = title(main = "Surival\n Probability",cex.main=0.5))
    mar.orig <- par("mar")
    w <- (3 + mar.orig[2]) * par("csi") * 2.54
    layout(matrix(c(2, 1), nc = 2), widths = c(1, lcm(w)))
    p<-p+contour(x = xvalue,
                 y = contcov,
                 z = predictPlot$surv[[indicator]],
                 nlevels=11,
                 drawlabels =TRUE, 
                 axes = FALSE, frame.plot = FALSE, add = TRUE)
  }
  p
}