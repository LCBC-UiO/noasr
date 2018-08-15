na.col.rm = function(DATA){
  
  NaNidx = apply(DATA,2,function(x) all(is.na(x)))
  DATA = DATA[,(1:ncol(DATA))[!NaNidx]]
  
  return(DATA)
}