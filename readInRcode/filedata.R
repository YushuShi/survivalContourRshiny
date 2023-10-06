filedata <- reactive({
  infile <- input$datafile
  if (is.null(infile)) {
    if(input$CoxorFG=="noComp"){
      if(input$intCen=="noint"){
        read.csv(ifelse(input$strata=="yes","veteran2.csv","veteran.csv"), stringsAsFactors = FALSE,row.names = 1)
      }else{
        read.csv(ifelse(input$strata=="yes","bcdeter2.csv","bcdeter2.csv"), stringsAsFactors = FALSE,row.names = 1)   
        # read.csv(ifelse(input$strata=="yes","bcdeter2.csv","bcdeter.csv"), stringsAsFactors = FALSE,row.names = 1) 
      }
    }else{
      if(input$intCen=="noint"){
        read.csv("Paquid.csv", stringsAsFactors = FALSE,row.names = 1)
      }else{
        read.csv("sim1234.csv", stringsAsFactors = FALSE,row.names = 1)  
      }
    }
  }else{
    read.csv(infile$datapath,stringsAsFactors = FALSE)}
})