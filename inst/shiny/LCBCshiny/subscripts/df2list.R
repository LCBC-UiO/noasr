df2list = function(DATA, TXT){
  
  LIST = apply(DATA, 1, 
               function(x){ 
                 x[grep(paste(TXT, collapse="|"), names(x))] %>% 
                   as.data.frame()
                 }
               )
  
  return(LIST)
}