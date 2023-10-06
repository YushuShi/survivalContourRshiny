library(shiny)
library(rhandsontable)
library(plotly)
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
library(jsonlite)
library(flexsurv)
library(rlist)
library(dplyr)
library(randomForestSRC)
#setwd("C:/Users/ys8wp/OneDrive - University of Missouri/contourPlot/survivalContour/")
#setwd("/Users/yushushi/OneDrive - University of Missouri/contourPlot/survivalContour/")
source("ui.R")
source("server.R")

PID=NA
version="1.0"
shinyAppName="survivalContour"
author="Yushu Shi"
authorEmail="shiyushu2006@gmail.com"
Function="Plot predicted survival and cumulative incidence function for continuous covariate"

shinyApp(ui = ui, server = server)

