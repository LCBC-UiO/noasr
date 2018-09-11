list2df = function(DATA, COLUMN){
  
  # In case varying number of columns
  idx = sapply(DATA[,COLUMN], length)
  
  tmp =  as.data.frame(do.call(rbind,lapply(DATA[,COLUMN], `length<-`, max(idx))))
  
  for(i in grep("Date", names(tmp))){
    tmp[,i] = tmp[,i] %>% as.Date(origin="1970-01-01")
  }
  
  DATA = DATA %>% select(-one_of(COLUMN)) %>% bind_cols(tmp)
  
return(DATA)
}