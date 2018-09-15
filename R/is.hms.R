#' Utility function to locate hms columns
#'
#' \code{is.hms} locates columns that are time (hms) classes
#'
#' @param data a data.frame
#'
#' @return logical vector of length==ncol(data)

#' @examples
#' is.hms(data)
#'
#' @export
is.hms = function(data){
  any(class(data) %in% "hms")
}
