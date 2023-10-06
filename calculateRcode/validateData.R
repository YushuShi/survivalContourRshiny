validateData<-function(rtable,dataTable){
  for(i in 1:length(rtable$CateorNot)){
    if(!rtable$CateorNot[i]){
      
      if((rtable$value4Plot[i]>max(range(dataTable[,rtable$otherCovName[i]])))|(rtable$value4Plot[i]<min(range(dataTable[,rtable$otherCovName[i]])))){
        warning(safeError(paste0("covariate ",rtable$otherCovName[i],"'s value for prediction is outside original data range")))}
    }else{
      if(!rtable$value4Plot[i] %in% levels(factor(dataTable[,rtable$otherCovName[i]]))){
        stop(safeError(paste0("covariate ",rtable$otherCovName[i],"'s value for prediction does not appear in the original dataset")))
      }
    }
  } 
}
