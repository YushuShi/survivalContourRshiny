output$draw3DContour<- renderPlotly({
  input$getPlot
  dataTable<-filedata()
  predictPlot<-calculatePredict()
  contcov<-seq(input$covSlider[1],input$covSlider[2],length.out = 21)
  yaxisValue<-predictPlot$time
  xaxisValue<-rev(contcov)
  showCont<-ifelse(identical(input$covName,""),input$covariate,input$covName)
  if(!identical(input$strata,"yes")){
    zaxisValue<-predictPlot$surv
    zaxisValue<-zaxisValue[,rev(1:ncol(zaxisValue))]
    # fig <- plot_ly(x = ~xaxisValue,
    #                y = ~yaxisValue,
    #                z = zaxisValue)
    # fig <- plot_ly(x = xaxisValue,
    #                y = yaxisValue,
    #                z = zaxisValue)
    #fig<-fig%>% add_surface()
    fig <- plot_ly(z = ~volcano)
    fig <- fig %>% add_surface()
    #,
    # fig <- plot_ly(type = 'surface',
    #                x = ~xaxisValue,
    #                y = ~yaxisValue,
    #                z = zaxisValue)#,
                   # colorbar = list(title = ifelse(identical(input$CoxorFG,"noComp"),                                                                              "Predicted survival probability","Predicted CIF"),
                   #                 titleside='right'),
                   # colorscale=input$colSche,
                   # hovertemplate = paste('At time %{y:.2f} <br>with',showCont,
                   #                       'being %{x:.2f},<br>the predicted', 
                   #                       ifelse(identical(input$CoxorFG,"noComp"),"survival","CIF") ,'is %{z:.2f}<extra></extra>'))
    if(identical(input$CI3D,"Yes")&(!is.null(predictPlot))){
      upperZ<-predictPlot$upper
      upperZ<-upperZ[,rev(1:ncol(upperZ))]
      fig<-fig %>% add_surface(z = upperZ,opacity = 0.75,showscale=FALSE)
      lowerZ<-predictPlot$lower
      lowerZ<-lowerZ[,rev(1:ncol(lowerZ))]
      fig<-fig %>% add_surface(z = lowerZ,opacity = 0.75,showscale=FALSE)
    }
    
    fig <- fig %>% layout(scene = list(xaxis = list(nticks = 8,
                                                    title=showCont, range=rev(range(xaxisValue))),
                                       yaxis = list(nticks = 5,title="Time"),
                                       zaxis=list(title=ifelse(identical(input$CoxorFG,"noComp"),"Predicted Survival","Predicted CIF"))
    ))
  }else{
    strataList<-unique(dataTable[,input$strataCov])
    strataList<-strataList[!is.na(strataList)]
    plotList<-NULL
    for(i in 1:length(strataList)){
      zaxisValue<-predictPlot$surv[[i]]
      zaxisValue<-zaxisValue[,rev(1:ncol(zaxisValue))]
      plotList[[i]]<- plot_ly(type = 'surface',
                              x = ~xaxisValue,
                              y = ~yaxisValue,
                              z = zaxisValue)
      # ,
      #                         colorbar = list(title =                                    ifelse(identical(input$CoxorFG,"noComp"),                                                                              "Predicted survival probability","Predicted CIF"),
      #                                         titleside='right'),
      #                         colorscale=input$colSche,
      #                         scene=paste0('scene',i),
      #                         hovertemplate = paste('At time %{y:.2f} <br>with',showCont,
      #                                               'being %{x:.2f},<br>the predicted survival is %{z:.2f}<extra></extra>'))
      
      eval(parse(text=paste0("plotList[[",i,
                             "]]<-plotList[[",i,"]] %>% layout(scene",ifelse(i>1,i,""),
                             " = list(xaxis = list(nticks = 8,title=showCont, range=rev(range(xaxisValue))),",
                             "yaxis = list(nticks = 5,title='Time'),zaxis=list(title='Predicted Survival')))"
      )))
      
      plotList[[i]]<-plotList[[i]] %>% layout(annotations = list(
        #x = (i-0.5)/length(strataList), 
        x=0,
        y = 1,
        text = paste(input$strataName, strataList[i]), showarrow = F, xref='paper', yref='paper')
      )
      
      if(i>1){
        plotList[[i]]<-plotList[[i]] %>% hide_colorbar()
        
      }
    }
    fig <- subplot(plotList,nrows = 1)
    #fig <- subplot(plotList,nrows = length(strataList))
  }
  fig
})