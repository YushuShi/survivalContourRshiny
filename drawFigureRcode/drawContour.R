output$drawContour<- renderPlotly({
  input$getPlot
  dataTable<-filedata()
  predictPlot<-calculatePredict()
  contcov<-seq(input$covSlider[1],input$covSlider[2],length.out = 21)
  if(!identical(input$strata,"yes")){
    histData<-dataTable[,input$covariate]
    fig <- plot_ly(x = predictPlot$time, y = contcov, z= t(predictPlot$surv),
                   colorbar = list(title =                                    ifelse(identical(input$CoxorFG,"noComp"),                                                                              "Predicted survival probability","Predicted CIF"),
                                   titleside='right'),
                   colorscale=input$colSche,type = "contour",
                   hovertemplate = paste('At time %{x:.2f} <br>with',input$covName,
                                         'being %{y:.2f},<br>the predicted ',
                                         ifelse(identical(input$CoxorFG,"noComp"),"survival","CIF"),
                                         ' is %{z:.2f}<extra></extra>'))
    fig <- fig %>% layout(title=list(text=ifelse(identical(input$CoxorFG,"noComp"),
                                         "Contour Plot of the Predicted Survival Probability",
                         "Contour Plot of the Predicted Cumulative Incidence Function"),
                         x=0.15),
                  xaxis=list(title="Time",range=range(predictPlot$time)),
                  yaxis=list(title=input$covName,range=range(contcov)))
    
    s <- subplot(fig,
      plot_ly(y = histData, type = "histogram",hoverinfo='none') %>% layout(xaxis=list(title="Count")),
      nrows = 1, widths = c(0.8, 0.2), margin = 0.01,
      shareY = TRUE,
      titleX=TRUE,
      titleY=TRUE
    )
  }else{
    # for now we only have stratified Cox model, no stratified FG model.
    strataList<-unique(dataTable[,input$strataCov])
    strataList<-strataList[!is.na(strataList)]
    plotList<-NULL
    for(i in 1:length(strataList)){
      histData<-dataTable[dataTable[,input$strataCov]==strataList[i],input$covariate]
      histData<-histData[!is.na(histData)]
      temp <- plot_ly(x = predictPlot$time, y = contcov, z= t(predictPlot$surv[[i]]),
                      colorbar = list(title =                                    ifelse(identical(input$CoxorFG,"noComp"),                                                                              "Predicted survival probability","Predicted CIF"),
                                      titleside='right'),
                      colorscale=input$colSche,type = "contour",
                     hovertemplate = paste('At time %{x:.2f} <br>with',input$covName,
                                           'being %{y:.2f},<br>the predicted survival is %{z:.2f}<extra></extra>'))
      temp <- temp %>% layout(title=list(text="Contour Plot of the Predicted Survival Probability",x=0.15),
        xaxis=list(title="Time",range=range(predictPlot$time)),
                            yaxis=list(title=input$covName,range=range(contcov)))
      if(i>1){
        temp<-temp %>% hide_colorbar()
      }
      plotList[[2*i-1]]<-temp
      temp2 <- plot_ly(y = histData, type = "histogram",hoverinfo='none',showlegend=FALSE)%>% layout(xaxis=list(title="Count"))
      temp2 <- temp2 %>% add_annotations(x=0.5,y=1.05,
                                         yref = "paper",
                                         xref = "paper",
                                         text=paste(input$strataName,strataList[i]),
                                         xanchor = "middle",
                                         yanchor = "top",
                                         showarrow=FALSE)
      plotList[[2*i]]<-temp2
    }
    s <- subplot(plotList,
                 nrows = length(strataList), 
                 #ncol=2,
                 widths = c(0.8, 0.2),
                 margin=c(0,0.01,0.02,0),
                 shareX=TRUE,
                 shareY=TRUE,titleX = TRUE,titleY = TRUE)
    s<-s%>%layout(height=450*length(strataList))
  }
  s
})
 