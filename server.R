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
    if(identical(input$CoxorFG,"noComp")){
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
    if(identical(input$noCompSelect,"rf")){
      numericInput("numTree", "Number of trees:", value = 500)

    }
  })
  output$intCen <- renderUI({
    if(!identical(input$CoxorFG,"noComp")||identical(input$noCompSelect,"Cox")){
      radioButtons("intCen", "Do your data have interval censored observation(s)?",
                   list("No, my data don't have interval censored observation(s)." = "noint",
                        "Yes, my data do have interval censored observation(s)." = "int"),
                   selected="noint")}
  })
  
  output$strata <- renderUI({
    if(identical(input$CoxorFG,"noComp")&(identical(input$noCompSelect,"Cox"))){
      radioButtons("strata", "Do your need a stratified Cox model?",
                   list("No, I do not need to stratify my analysis."="no",
                        "Yes, I do want to stratify my analysis."="yes"),
                   selected="no")
    }
  })
  
  output$downloadData <- downloadHandler(
    filename = "example.csv",
    content=function(file){
      if(identical(input$CoxorFG,"noComp")){
        if(identical(input$intCen,"noint")){
          if(identical(input$strata,"yes")){
            load("veteran2.RData")
            write.csv(veteran, file,row.names = FALSE)
          }else{
            load("veteran.RData")
            write.csv(veteran, file,row.names = FALSE) 
          }
        }else{
          if(identical(input$strata,"yes")){
            load("bcdeter2.RData")
            write.csv(bcdeter, file,row.names = FALSE)
          }else{
            load("bcdeter.RData")
            write.csv(bcdeter, file,row.names = FALSE) 
          }
        }
      }else{
        if(identical(input$intCen,"noint")){
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
    if (is.null(df)) return(NULL)
    if(identical(input$intCen,"noint")){
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
    if (is.null(df)) return(NULL)
    if(identical(input$intCen,"int")){
      items=names(df)
      names(items)=items
      selectInput("time2", "The first observation time after the failure for interval censored data. (Inf for right censored data )",items, selected = "upper")
    }
  })
  
  output$status <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    items=names(df)
    names(items)=items
    if(!(identical(input$intCen,"int")&identical(input$CoxorFG,"noComp"))){
      selectInput("status", "event", items, selected = ifelse(identical(input$intCen,"int"),"event","status"))
    }
  })
  
  output$covariate <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    items=names(df)
    names(items)=items
    selectInput("covariate", "The continuous covariate used for plotting", items,
                selected = ifelse(identical(input$CoxorFG,"noComp"),
                                  ifelse(identical(input$intCen,"noint"),"karno","cont"),
                                  ifelse(identical(input$intCen,"noint"),"DSST","continuousCov"))
    )
  })
  
  output$strataCov <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    if(identical(input$strata,"yes")){
      items=names(df)
      names(items)=items
      selectInput("strataCov", "The categorical covariate used to stratify analysis",
                  items, selected = ifelse(identical(input$intCen,"int"),"treat","trt"))
    }
  })
  
  output$otherCovSelect <- renderUI({
    df <-filedata()
    #if (is.null(df)) return(NULL)
    if(identical(input$otherCov,"Yes")){
      dfcolnames=colnames(df)
      names(dfcolnames)=dfcolnames
      selectInput("otherCovSelect", "Other covariates in the model \u2193",
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
    if(identical(input$strata,"yes")){
      textInput("strataName", "Name of the strata covariate that will appear on the plot", "")
    }
  })
  
  output$timeSlider <- renderUI({
    dataTable<-filedata()
    if(identical(input$intCen,"int")){
      if(!identical(input$strata,"yes")){
        timecov<-unlist(c(dataTable[,input$time1],dataTable[,input$time2]))
        timecov<-timecov[!is.na(timecov)]
        timecov<-timecov[is.finite(unlist(timecov))]   
        maxtime<-max(timecov)
      }else{
        strataCov<-unique(dataTable[,input$strataCov])
        strataCov<-strataCov[!is.na(strataCov)]
        if(!is.null(input$strataCov)){
          if(!is.null(input$time1)){
            if(!is.null(input$time2)){        
              maxtime<-min(unlist(sapply(strataCov,function(x){
                subData<-dataTable[dataTable[,input$strataCov]==x,]
                timecov<-c(subData[,input$time1],subData[,input$time2])
                timecov<-timecov[!is.na(timecov)]
                timecov<-timecov[is.finite(timecov)]
                max(timecov)
              })))
            }
          }
        }
      }
    }else{
      if(!identical(input$strata,"yes")){
        timecov<-dataTable[,input$time]
        if(!is.null(input$time)){
          maxtime<-max(timecov,na.rm = TRUE)
        }
      }else{
        strataCov<-unique(dataTable[,input$strataCov])
        strataCov<-strataCov[!is.na(strataCov)]
        if(!is.null(input$strataCov)){
          if(!is.null(input$time)){
            maxtime<-min(unlist(sapply(strataCov,function(x){
              subData<-dataTable[dataTable[,input$strataCov]==x,]
              timecov<-subData[,input$time]
              max(timecov,na.rm=TRUE)
            })))
          }
        }
      }
    }
    if (is.null(dataTable)) return(NULL)
    if(exists("maxtime")){
      sliderInput("timeSlider", "The time range for the plot",
                  min=0,
                  #max=ifelse(!is.null(isolate(maxtime)),isolate(maxtime),1),
                  #value = ifelse(!is.null(isolate(maxtime)),isolate(9/10*maxtime),1),
                  max=ifelse(is.finite(isolate(maxtime)),isolate(maxtime),1),
                  value = ifelse(is.finite(isolate(maxtime)),isolate(9/10*maxtime),1),
                  round=3)
    }
  })
  
  output$covSlider <- renderUI({
    dataTable<-filedata()
    if (is.null(dataTable)) return(NULL)
    contcov<-input$covariate
    if(!is.null(contcov)){
      if(contcov %in% colnames(dataTable)){
      sliderInput("covSlider", "The covariate range for the plot",
                       min=min(isolate(dataTable[,contcov]),na.rm=TRUE),
                       max=max(isolate(dataTable[,contcov]),na.rm=TRUE),
                       value = c(9/10*min(isolate(dataTable[,contcov]),na.rm=TRUE)+
                                1/10*max(isolate(dataTable[,contcov]),na.rm=TRUE),
                                1/10*min(isolate(dataTable[,contcov]),na.rm=TRUE)+
                                9/10*max(isolate(dataTable[,contcov]),na.rm=TRUE)),
                       round=3)
    }
    }

    # sliderInput("covSlider", "The covariate range for the plot",
    #             #min=min(isolate(dataTable[,contcov]),na.rm=TRUE),         
    #             #max=max(isolate(dataTable[,contcov]),na.rm=TRUE),  
    #             #value = c(min(isolate(dataTable[,contcov]),na.rm=TRUE)+1/10*max(isolate(dataTable[,contcov]),na.rm=TRUE),
    #             #          min(isolate(dataTable[,contcov]),na.rm=TRUE)+9/10*max(isolate(dataTable[,contcov]),na.rm=TRUE)),
    #             min=ifelse(!is.null(isolate(dataTable[,contcov])),
    #                        min(isolate(dataTable[,contcov]),na.rm=TRUE),0),
    #             max=ifelse(!is.null(isolate(dataTable[,contcov])),
    #                        max(isolate(dataTable[,contcov]),na.rm=TRUE),1),
    #             value = c(ifelse(!is.null(isolate(dataTable[,contcov])),
    #                              9/10*min(isolate(dataTable[,contcov]),na.rm=TRUE)+
    #                                1/10*max(isolate(dataTable[,contcov]),na.rm=TRUE),0),
    #                       ifelse(!is.null(isolate(dataTable[,contcov])),
    #                              1/10*min(isolate(dataTable[,contcov]),na.rm=TRUE)+
    #                                9/10*max(isolate(dataTable[,contcov]),na.rm=TRUE),1)
    #                       ),
    #             round=3)
    
  })
  
  output$CI3D <- renderUI({
    if(identical(input$strata,"no")&identical(input$CoxorFG,"noComp")){
      if(!identical(input$noCompSelect,"rf")){
        radioButtons("CI3D", "Do you want to show 95% CI for predicted survival in 3D contour plot?",
                     list("No, I don't." = "No",
                          "Yes, I do." = "Yes"),
                     selected="No")
      }
    }
  })

  source("calculateRcode/validateData.R",local=TRUE)  

  source("calculateRcode/calculatePredict.R",local=TRUE)  

  source("calculateRcode/contourPlot.R",local=TRUE)  

  source("calculateRcode/quantilePlot.R",local=TRUE)  

    source("drawFigureRcode/pEffectsize.R",local=TRUE)

  source("drawFigureRcode/drawContour.R",local=TRUE)

  source("drawFigureRcode/draw3DContour.R",local=TRUE) 

  source("drawFigureRcode/drawQuantile.R",local=TRUE)   

  source("downloadRcode/downloadQPlot.R",local=TRUE) 

  source("downloadRcode/downloadResult.R",local=TRUE)  

  observeEvent(input$getPlot, {
    updateTabsetPanel(session, "outputtab",selected = "Result")
  })
})