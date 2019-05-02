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
na.col.rm = function(data) {
  NaNidx = apply(data, 2, function(x) all(is.na(x)))
  data[, !NaNidx]
}
