
#' Coerce NOAS date into R date
#'
#' Dates from the NOAS always come
#' in a specific format (yyyy-mm-dd).
#' This function is for convenience in
#' turning NOAS dates into R-dates easily.
#'
#' @param dates character of dates (yyyy-mm-dd)
#'
#' @return vector of R dates
#' @export
#'
#' @examples
#' dates <- c("2016-03-01", "2002-12-31")
#' dates <- as.Date(dates, format="%Y-%m-%d")
#' dates
#' as.numeric(dates)
as_date <- function(dates){
  stopifnot(class(dates) == "character")
  as.Date(dates, format="%Y-%m-%d")
}

#' Check if data contains important cols
#'
#' @template data
#' @importFrom magrittr %>%
#' @return
#' @noRd
check_data <- function(data){
  if(!is.data.frame(data))
      stop("data is not a data.frame", call. = FALSE)

  cols <- !c("subject_id","project_id","wave_code") %in% names(data)
  if(any(cols))
    stop("Data is missing necessary columns:\n  ",
         paste(c("subject_id","project_id","wave_code")[cols], collapse = ", "),
         call. = FALSE)
}
