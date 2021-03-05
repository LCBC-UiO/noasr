
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



#' Calculate the mean between two dates
#'
#' @param date1 first date
#' @param date2 second date
#'
#' @return mean difference between two dates
#' @export
#'
#' @examples
#' dates <- as.Date(c("2020-01-20", "1998-01-20"))
#' mean_date(dates[1], dates[2])
mean_date <- function(date1, date2){

  dts <- cbind(date1,date2)
  diff <- rowMeans(dts, na.rm=TRUE)

  as.Date(diff, origin = "1970-01-01")
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
