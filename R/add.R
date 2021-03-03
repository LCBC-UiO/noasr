

#' Add timepoint to data
#'
#' Calculated sequential timepoint
#' for participants along the 'age'
#' column.
#'
#' @template data
#' @template var_name
#'
#' @return data frame with extra column with timepoint
#' @export
add_timepoint <- function(data, var_name = timepoint){
  check_data(data)

  if(!"age" %in% names(data))
    stop("This function needs 'age' to calculate timepoint.",
         call. = FALSE)

  group_by(data, subject_id, age) %>%
    mutate(
      .n = dplyr::row_number(),
      .tp = ifelse(.n == 1, 1, 0)
    ) %>%
    group_by(subject_id) %>%
    mutate(
      {{var_name}} := cumsum(.tp)
    ) %>%
    select(-.n, -.tp)
}


#' Add intervals
#'
#' Since we have longitudinal data,
#' intervals for subjects is a good  way
#' to get a better idea of the time scale
#' of the data. These functions add extra
#' columns to your data.
#'
#' \itemize{
#' \item{add_interval}{ - add interval since last visit - default col: interval}
#' \item{add_interval_baaseline}{ - add interval since first visit- default col: baseline}
#' }
#'
#' @template data
#' @template var_name
#'
#' @return data frame with one extra column
#'
#' @name add_interval
NULL


#' @export
#' @rdname add_interval
#' @importFrom dplyr group_by mutate ungroup lag lead case_when
add_interval <- function(data, var_name = interval){
  data %>%
    add_interval_baseline(.baseline) %>%
    group_by(subject_id) %>%
    mutate(
      .lag = lag(age),
      .dup = lag(ifelse(.baseline == lead(.baseline), TRUE, FALSE)),

      {{var_name}} := ifelse(is.na(.lag), 0, age-.lag),
      {{var_name}} := case_when(
        is.na(.lag) ~ 0,
        .dup == TRUE ~ lag({{var_name}}),
        TRUE ~ {{var_name}}
      )
    ) %>%
    ungroup() %>%
    select(-.baseline, -.lag, -.dup)
}

#' @export
#' @rdname add_interval
#' @importFrom dplyr arrange group_by mutate ungroup
add_interval_baseline <- function(data, var_name = interval_baseline){
  check_data(data)

  if(!"age" %in% names(data))
    stop("This function needs 'age' to calculate timepoint.",
         call. = FALSE)

  data %>%
    arrange(age) %>%
    group_by(subject_id) %>%
    mutate(
      {{var_name}} := age-min(age)
    ) %>%
    ungroup()
}


mean_date <- function(date1, date2){

  dts <- cbind(date1,date2)
  diff <- rowMeans(dts, na.rm=TRUE)

  as.Date(diff, origin = "1970-01-01")
}

## quiets concerns of R CMD check
if(getRversion() >= "2.15.1"){
  utils::globalVariables(c("timepoint", "subject_id", "age", ".n",
                           ":=", ".tp", "age", ".lag", ".dup",
                           "interval", ".baseline", "interval_baseline"))
}

