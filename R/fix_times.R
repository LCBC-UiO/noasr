#' Calculates time in decimal hours from HH:MM format.
#'
#' \code{fix_times} calculates time in decimal hours from HH:MM format.
#'
#' @param data String or time data in 'HH:MM' format
#'
#' @return a numeric vector of decimal hours

#' @examples
#'  fix_times(data)
#'
#' @export

fix_times = function(data){

  tmp = strsplit(as.character(data), ":")

  DATA2 = data.frame(matrix(nrow=length(tmp),ncol=3))

  for(i in 1:length(tmp)){
    DATA2[i,]  = tmp[[i]]
  }
  DATA2 = as.data.frame(apply(DATA2,2,type.convert))
  DATA2 = DATA2[,1:2]; names(DATA2) = c("H","M");

  DATA2$Time = DATA2$H+(DATA2$M/60)

  return(DATA2$Time)
}
