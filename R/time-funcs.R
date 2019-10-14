#' Utility function to locate hms columns
#'
#' \code{is_hms} locates columns that are time (hms) classes
#'
#' @param data a data.frame
#'
#' @return logical vector of length==ncol(data)

#' @examples
#' \dontrun{
#' is_hms(data)
#' }
#' @export
is_hms = function(data){
  any(class(data) %in% "hms")
}



#' Factor time of day
#'
#' Takes a vector of HH:MM (HH:MM:SS) information and
#' categorises these by a 4 level factor of time of day.
#'
#' @param x character or hms vector
#'
#' @return factor vector
#' @export
#'
#' @examples
#' factor_times(c("12:23", "15:59", "22:10", "8:13"))
factor_times = function(x) {

  DATA2 = suppressWarnings(dplyr::tibble(Time = lubridate::hms(x))) %>%
    mutate(TimeOfDay = dplyr::case_when(
      Time@hour >= 5 & Time@hour < 12 ~ "Morning",
      Time@hour >= 12 & Time@hour < 17 ~ "Afternoon",
      Time@hour >= 17 & Time@hour < 21 ~ "Evening",
      TRUE ~ "Night"
    ),
    TimeOfDay = factor(TimeOfDay,
                       levels = c("Morning", "Afternoon", "Evening", "Night"),
                       ordered = TRUE)
    )

  return(DATA2$TimeOfDay)
}

## quiets concerns of R CMD check
if(getRversion() >= "2.15.1"){
  utils::globalVariables(c("TimeOfDay"))
}

