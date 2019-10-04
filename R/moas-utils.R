get_moas <- function(MOAS){
  if(is.character(MOAS)){
    nn <- load(MOAS)
    get(nn)
  }else{
    MOAS
  }
}

#' Utility function to remove columns with no data
#'
#' \code{na.col.rm} Removes columns from the
#' data frame that does not contain any data.
#'
#' @param data a data.frame
#'
#' @return a data frame with no empty columns

#' @examples
#' \dontrun{
#' na.col.rm(data)
#' }
#' @export
na_col_rm = function(data) {
  NaNidx = apply(data, 2, function(x) all(is.na(x)))
  data[, !NaNidx]
}


