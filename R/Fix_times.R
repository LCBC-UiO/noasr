Fix_times = function(DATA){
  
  tmp = strsplit(as.character(DATA), ":")
  
  DATA2 = data.frame(matrix(nrow=length(tmp),ncol=3))
  
  for(i in 1:length(tmp)){
    DATA2[i,]  = tmp[[i]]
  }
  DATA2 = as.data.frame(apply(DATA2,2,type.convert))
  DATA2 = DATA2[,1:2]; names(DATA2) = c("H","M"); 
  
  DATA2$Time = DATA2$H+(DATA2$M/60)
  
  return(DATA2$Time)
}