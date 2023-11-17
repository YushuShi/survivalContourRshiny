filedata <- reactive({
  infile <- input$datafile
  if (is.null(infile)) {
    if(identical(input$CoxorFG,"noComp")){
      if(identical(input$intCen,"noint")){
        read.csv(ifelse(identical(input$strata,"yes"),"veteran2.csv","veteran.csv"), stringsAsFactors = TRUE,row.names = 1)
      }else{
        read.csv(ifelse(identical(input$strata,"yes"),"bcdeter2.csv","bcdeter2.csv"), stringsAsFactors = TRUE,row.names = 1)   

      }
    }else{
      if(identical(input$intCen,"noint")){
        read.csv("Paquid.csv", stringsAsFactors = TRUE,row.names = 1)
      }else{
        read.csv("sim1234.csv", stringsAsFactors = TRUE,row.names = 1)  
      }
    }
  }else{
    read.csv(infile$datapath,stringsAsFactors = TRUE)}
})