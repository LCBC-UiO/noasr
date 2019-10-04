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


#' Factor time of day into categories of 4.
#'
#' \code{factor_times} will categorise a time vector into one of
#' four times of day: Morning, Afternoon, Evening, or Night.
#'
#' @param TimeVector String or time data in 'HH:MM' format
#'
#' @return A string vector with up to 4 levels.

#' @examples
#' factor_times(c("12:34","23:35","10:23","15:15"))
#'
#' @importFrom lubridate hm hms
#' @importFrom dplyr as_tibble mutate case_when
#' @importFrom magrittr '%>%'
#' @export

factor_times = function(TimeVector) {

  DATA2 = dplyr::tibble(Time = lubridate::hms(TimeVector)) %>%
    dplyr::mutate(
      TimeOfDay = dplyr::case_when(
                            Time@hour >= 5 & Time@hour < 12 ~ "Morning",
                            Time@hour >= 12 & Time@hour < 17 ~ "Afternoon",
                            Time@hour >= 17 & Time@hour < 21 ~ "Evening",
                            TRUE  ~  "Night")
    )

  return(DATA2$TimeOfDay)
}

