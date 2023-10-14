#library(rhandsontable)
shinyUI(
  fluidPage(
    headerPanel("SurvivalContour: Show Survival Prediction in Contour Plot"),
    HTML(paste(HTML('&emsp;'),"PID:xxx",HTML('&nbsp;'),"Version:1.0",HTML('&nbsp;'), 
               "Last updated:", Sys.Date()),'<br>',
         paste(HTML('&emsp;'),HTML('Yushu Shi<sup>1</sup></strong>,'), HTML('Liangliang Zhang<sup>2</sup>,'),
               HTML('Kim-Anh Do<sup>3</sup>,'), HTML('Robert Jenq<sup>4</sup>,'), "and", HTML('Christine Peterson<sup>3</sup>')),'<br>',
         paste(HTML('&emsp;'),"Contact email:",HTML('&nbsp;'),HTML('<a>shiyushu2006@gmail.com</a>'))),
    
    sidebarLayout( 
      sidebarPanel(
        
        radioButtons("CoxorFG", "Do your data have competing risks?",
                     list("No, my data don't have competing risks." = "noComp",
                          "Yes, my data do have competing risks." = "FG"),
                     selected="noComp"),
        uiOutput("noCompSelect"),
        uiOutput("numTree"),
        uiOutput("intCen"),
        
        # radioButtons("intCen", "Do your data have interval censored observation(s)?",
        #              list("No, my data don't have interval censored observation(s)." = "noint",
        #                   "Yes, my data do have interval censored observation(s)." = "int"),
        #              selected="noint"),
        

        uiOutput("strata"),
        tags$div(tags$h4("Step 1: Provide data")),
        tags$br(),
        
        downloadButton("downloadData", "Example dataset"),
        
        tags$br(),
        tags$div(tags$h4("")),
        # fileInput('datafile', 'Choose CSV or txt file',
        #           accept=c('text/csv', 'text/comma-separated-values,text/plain')),
        fileInput('datafile', 'Choose CSV or txt file',
                  accept=c('text', 'csv')),
        tags$div(tags$h4("Step 2: Run a survival model or a competing risks model")),
        tags$br(),
        tags$div("Select time, event and relevant covariate"),
        tags$br(),
        
        uiOutput("time"),
        uiOutput("time2"),
        uiOutput("status"),
        uiOutput("covariate"),
        uiOutput("strataCov"),
        uiOutput("strataName"),
        
        tags$div("Select range of time and covariate for the plot"),
        
        uiOutput("timeSlider"),
        uiOutput("covSlider"),
        conditionalPanel
        (
          condition = '0==1',
          sliderInput("dummyslider", "", min=0, max=1, value=0)
        ),
        
        tags$br(),
        
        selectInput("colSche", "Choose color scheme for the plot",
                    c("Viridis" = "Viridis",
                      "Thermal" = "Thermal",
                      "Jet"="Jet",
                      "Hot"="Hot",
                      "Electric"="Electric"), selected="Viridis"),
        
        textInput("covName", "Name of the covariate that will appear on the plot", ""),
        
        
        radioButtons("otherCov", "Do you have other covariates in your model?",
                     list("No, I don't." = "No",
                          "Yes, I do." = "Yes"),
                     selected="No"),
        
        uiOutput("CI3D"),
        
        # radioButtons("CI3D", "Do you want to show 95% CI for predicted survival in 3D contour plot?(only for Cox model)",
        #              list("No, I don't." = "No",
        #                   "Yes, I do." = "Yes"),
        #              selected="No"),
        uiOutput("otherCovSelect"),
        rhandsontable::rHandsontableOutput("table"),
        tags$br(),
        actionButton("getPlot", "Create Plots"),
      ),
      mainPanel(
        tabsetPanel(id="outputtab",
                    #includeHTML()
                    tabPanel("Information",includeHTML('SurvContour.html')),
                    tabPanel("Survival Data",includeHTML('cox.html')),
                    tabPanel("Stratified Cox Model",includeHTML('strata.html'),value="strata"),
                    tabPanel("Competing Risks Data",includeHTML('fg2.html'),value="fg"),
                    tabPanel("Random Survival Forest",includeHTML('rf.html'),value="rf"),
                    tabPanel("Result",conditionalPanel(condition="input.getPlot!=0",
                                                 #tableOutput("pEffectsize"),
                                                 conditionalPanel(condition="!identical(input.noCompSelect,'rf')",tableOutput("pEffectsize")),
                                                 hr(),
                                                 plotly::plotlyOutput("drawContour",height="auto"),
                                                 hr(),
                                                 plotly::plotlyOutput("draw3DContour",height="auto"),
                                                 hr(),
                                                 plotOutput("drawQuantile",inline=TRUE),
                                                 hr(),
                                                 downloadButton('downloadQPlot', 'Download survival(CIF) plots at 5 quantiles of the covariate'),
                                                 hr(),
                                                 downloadButton('downloadResult', 'Download plot information')),
                             value="Result")
        )
      )
    ),

    hr(),
    HTML(paste(HTML('&emsp;'),"1:",HTML('&nbsp;'),"Department of Population Health Sciences, Weill Cornell Medicine"),'<br>',
         paste(HTML('&emsp;'),"2:",HTML('&nbsp;'),"Department of Population and Quantitative Health Sciences, Case Western Reserve University"),'<br>',
         paste(HTML('&emsp;'),"3:",HTML('&nbsp;'),"Department of Biostatistics, the University of Texas, MD Anderson Cancer Center"),'<br>',
         paste(HTML('&emsp;'),"4:",HTML('&nbsp;'),"Department of Genomic Medicine,the University of Texas, MD Anderson Cancer Center"))
  ))
