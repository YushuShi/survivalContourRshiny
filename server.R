library(shiny)
library(riskRegression)
library(prodlim)
library(survival)
library(intccr)
library(viridis)
library(zip)
library(mets)
library(magick)
library(fields)
library(spatstat.utils)
library(rhandsontable)
library(plotly)
library(jsonlite)
library(flexsurv)
library(rlist)
library(dplyr)
library(randomForestSRC)
shinyServer(function(input, output,session) {
  output$noCompSelect <- renderUI({
    if(input$CoxorFG=="noComp"){
    selectInput("noCompSelect", 
                "Choose which model to use",
              c("Cox" = "Cox",
                "Random Survival Forest"="rf",
                "Spline"="Spline",
                "parametric generalized Gamma AFT" = "gengamma.orig",
                "parametric stable generalized Gamma AFT" = "gengamma",
                "parametric Weibull AFT" = "weibull",
                "parametric Log-logistic AFT" = "llogis",
                "parametric Log-Normal AFT" = "lnorm"), selected="Cox")}
  })
  
  output$numTree <- renderUI({
    if(input$noCompSelect=="rf"){
      numericInput("numTree", "Number of trees:", value = 500)
    }
  })
  output$intCen <- renderUI({
    if((input$CoxorFG!="noComp")||(input$noCompSelect=="Cox")){
      radioButtons("intCen", "Do your data have interval censored observation(s)?",
                   list("No, my data don't have interval censored observation(s)." = "noint",
                        "Yes, my data do have interval censored observation(s)." = "int"),
                   selected="noint")}
  })
  
  output$strata <- renderUI({
    if((input$CoxorFG=="noComp")&(identical(input$noCompSelect,"Cox"))){
      radioButtons("strata", "Do your need a stratified Cox model?",
                   list("No, I do not need to stratify my analysis."="no",
                        "Yes, I do want to stratify my analysis."="yes"),
                   selected="no")
    }
  })
  
  output$downloadData <- downloadHandler(
    filename = "example.csv",
    content=function(file){
      if(input$CoxorFG=="noComp"){
        if(input$intCen=="noint"){
          if(input$strata=="yes"){
            load("veteran2.RData")
            write.csv(veteran, file,row.names = FALSE)
          }else{
            load("veteran.RData")
            write.csv(veteran, file,row.names = FALSE) 
          }
        }else{
          if(input$strata=="yes"){
            load("bcdeter2.RData")
            write.csv(bcdeter, file,row.names = FALSE)
          }else{
            load("bcdeter.RData")
            write.csv(bcdeter, file,row.names = FALSE) 
          }
        }
      }else{
        if(input$intCen=="noint"){
          load("Paquid.RData")
          write.csv(Paquid, file,row.names = FALSE)
        }else{
          load("sim1234.RData")
          write.csv(newdata, file,row.names = FALSE) 
        }
      }
    }
  )
  
  source("readInRcode/filedata.R",local = TRUE)
  output$time <- renderUI({
    df <-filedata()      
    #if (is.null(df)) return(NULL)
    if(input$intCen=="noint"){
      items=names(df)
      names(items)=items
      selectInput("time", "time",items, selected = "time")
    }else{
      items=names(df)
      names(items)=items
      selectInput("time1", "The censoring or event time for right censored data or the last observation time prior to the failure for interval censored data;",items, selected = "lower")
    }
  })
  
  output$time2 <- renderUI({
    df <-filedata()
    #if (is.null(df)) return(NULL)
    if(input$intCen=="int"){
      items=names(df)
      names(items)=items
      selectInput("time2", "The first observation time after the failure for interval censored data. (Inf for right censored data )",items, selected = "upper")
    }
  })
  
  output$status <- renderUI({
    df <-filedata()
    #if (is.null(df)) return(NULL)
    items=names(df)
    names(items)=items
    if(!((input$intCen=="int")&(input$CoxorFG=="noComp"))){
      selectInput("status", "event", items, selected = ifelse(input$intCen!="int","status","event"))
    }
  })
  
  output$covariate <- renderUI({
    df <-filedata()
    #if (is.null(df)) return(NULL)
    items=names(df)
    names(items)=items
    selectInput("covariate", "The continuous covariate used for plotting", items,
                selected = ifelse(input$CoxorFG=="noComp",
                                  ifelse(input$intCen=="noint","karno","cont"),
                                  ifelse(input$intCen=="noint","DSST","continuousCov"))
    )
  })
  
  output$strataCov <- renderUI({
    df <-filedata()
    #if (is.null(df)) return(NULL)
    if(input$strata=="yes"){
      items=names(df)
      names(items)=items
      selectInput("strataCov", "The categorical covariate used to stratify analysis",
                  items, selected = ifelse(input$intCen=="int","treat","trt"))
    }
  })
  
  output$otherCovSelect <- renderUI({
    df <-filedata()
    #if (is.null(df)) return(NULL)
    if(input$otherCov=="Yes"){
      dfcolnames=colnames(df)
      names(dfcolnames)=dfcolnames
      selectInput("otherCovSelect", "Other coveraites in the model:",
                  dfcolnames, multiple=TRUE)
    }
  })
  
  observe({
    if(!is.null(input$otherCovSelect))
      output$table <- renderRHandsontable({rhandsontable(data.frame(otherCovName=input$otherCovSelect,
                                                                    CateorNot=rep(FALSE,length(input$otherCovSelect)),
                                                                    value4Plot=rep("0",length(input$otherCovSelect))),
                                                         rowHeaders = NULL,
                                                         colHeaders = c("Covariate Name",
                                                                        "If Categorical click","Value for \n predicting the outcome"))})
  })
  
  output$strataName <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    if(input$strata=="yes"){
      textInput("strataName", "Name of the strata covariate that will appear on the plot", "")
    }
  })
  
  output$timeSlider <- renderUI({
    dataTable<-filedata()
    if(input$intCen=="int"){
      if(!identical(input$strata,"yes")){
        timecov<-c(dataTable[,input$time1],dataTable[,input$time2])
        timecov<-timecov[!is.na(timecov)]
        timecov<-timecov[is.finite(timecov)]   
        maxtime<-max(timecov)
      }else{
        strataCov<-unique(dataTable[,input$strataCov])
        strataCov<-strataCov[!is.na(strataCov)]
        maxtime<-min(sapply(strataCov,function(x){
          subData<-dataTable[dataTable[,input$strataCov]==x,]
          timecov<-c(subData[,input$time1],subData[,input$time2])
          timecov<-timecov[!is.na(timecov)]
          timecov<-timecov[is.finite(timecov)]
          max(timecov)
        }))
      }
    }else{
      if(!identical(input$strata,"yes")){
        timecov<-dataTable[,input$time]
        maxtime<-max(timecov,na.rm = TRUE)
      }else{
        strataCov<-unique(dataTable[,input$strataCov])
        strataCov<-strataCov[!is.na(strataCov)]
        maxtime<-min(sapply(strataCov,function(x){
          subData<-dataTable[dataTable[,input$strataCov]==x,]
          timecov<-subData[,input$time]
          max(timecov,na.rm=TRUE)
        }))
      }
    }
    if (is.null(dataTable)) return(NULL)
    
    sliderInput("timeSlider", "The time range for the plot",
                min=0,
                max=isolate(maxtime),
                value = isolate(9/10*maxtime),
                round=3)
    
  })
  
  output$covSlider <- renderUI({
    dataTable<-filedata()
    contcov<-input$covariate
    if (is.null(dataTable)) return(NULL)
    sliderInput("covSlider", "The covariate range for the plot",
                min=isolate(min(dataTable[,contcov],na.rm=TRUE)),
                max=isolate(max(dataTable[,contcov],na.rm=TRUE)),
                value = c(isolate(9/10*min(dataTable[,contcov],na.rm=TRUE)+1/10*max(dataTable[,contcov],na.rm=TRUE)),
                          isolate(1/10*min(dataTable[,contcov],na.rm=TRUE)+9/10*max(dataTable[,contcov],na.rm=TRUE))),
                round=3)
    
  })
  
  output$CI3D <- renderUI({
    if((input$strata=="no")&(input$CoxorFG=="noComp")){
      radioButtons("CI3D", "Do you want to show 95% CI for predicted survival in 3D contour plot?",
                   list("No, I don't." = "No",
                        "Yes, I do." = "Yes"),
                   selected="No")
    }
  })
  source("calculateRcode/validateData.R",local=TRUE)  
  source("calculateRcode/calculatePredict.R",local=TRUE)  
  source("calculateRcode/contourPlot.R",local=TRUE)  
  source("calculateRcode/quantilePlot.R",local=TRUE)  
  #if(!identical(input$noCompSelect,"rf")){
    source("drawFigureRcode/pEffectsize.R",local=TRUE)
  #}
  source("drawFigureRcode/drawContour.R",local=TRUE)
  source("drawFigureRcode/draw3DContour.R",local=TRUE) 
  source("drawFigureRcode/drawQuantile.R",local=TRUE)   
#  source("downloadRcode/downloadPlot.R",local=TRUE) 
  source("downloadRcode/downloadQPlot.R",local=TRUE)   
  source("downloadRcode/downloadResult.R",local=TRUE)  

  observeEvent(input$getPlot, {
    updateTabsetPanel(session, "outputtab",selected = "Result")
  })
})