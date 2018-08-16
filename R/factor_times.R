#' Factor time of day into categories of 4.
#'
#' \code{factor_times} will categorise a time vector into one of
#' four times of day: Morning, Afternoon, Evening, or Night.
#'
#' @param TimeVector String or time data in 'HH:MM' format
#'
#' @return A string vector with up to 4 levels.

#' @examples
#'  factor_times(data)
#'
#' @importFrom lubridate hm hms
#' @export

factor_times = function(TimeVector) {

    DATA2 = as.data.frame(lubridate::hms(TimeVector))
    names(DATA2) = "Time"

    DATA2$TimeOfDay = ifelse(DATA2$Time@hour >= 5 & DATA2$Time@hour < 12, "Morning",
                             ifelse(DATA2$Time@hour >= 12 & DATA2$Time@hour < 17, "Afternoon",
                                    ifelse(DATA2$Time@hour >= 17 & DATA2$Time@hour < 21, "Evening",
                                           "Night")))

    return(DATA2$TimeOfDay)
}
