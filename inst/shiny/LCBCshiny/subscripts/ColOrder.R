ColOrder = function(DATA, ConversionTab){
  
  #Get column-order
  idx = ConversionTab %in% names(DATA) %>% na.omit() %>% which()
  
  df = DATA %>% select(one_of(ConversionTab[idx])) 
  
  idx = names(df) %in% ConversionTab[1:10]
  df = df[!duplicated(df[,idx]),]
  
  return(df)
}