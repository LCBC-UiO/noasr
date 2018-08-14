Clean_DF = function(DATA){
  DATA = apply(DATA,2,function(x) as.character(x)) %>% as.data.frame(stringsAsFactors=F)
  
  #Grab any column with an alhpabetic character or what is date
  COLS = apply(DATA,2, function(x) grepl("[a-zA-Z]|\\:", x) %>% any()) %>% 
    unlist %>% unname() + grepl("date|dato",names(DATA), ignore.case = T) 
  COLS = ifelse(COLS==1,TRUE,FALSE)
  
  OUT = DATA %>% select(one_of(names(DATA)[COLS]))
  
  #Remove data classes and labels to enable easier handling.
  DATA = DATA %>% 
    apply(2,function(x) type.convert(as.character(x))) %>% 
    as.data.frame() %>% 
    select(-one_of(names(OUT))) %>% cbind.data.frame(OUT)
  
  return(DATA)
}
