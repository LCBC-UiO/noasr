

#' Add timepoint to data
#'
#' Calculated sequential timepoint
#' for participants along the 'age'
#' column.
#'
#' @template data
#' @template name
#'
#' @return data frame with extra column with timepoint
#' @export
#' @examples
#' # attach built-in noas example data to test
#' dt <- noas_example
#'
#' add_timepoint(dt)
#'
#' library(dplyr)
#' dt %>%
#'  add_timepoint()
#'
#' # Change the name of the variable
#' dt %>%
#'  add_timepoint(name = tp)
#'
add_timepoint <- function(data, name = timepoint){
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
      {{name}} := cumsum(.tp)
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
#' @template name
#'
#' @return data frame with one extra column
#'
#' @name add_interval
#' @examples
#' # attach built-in noas example data to test
#' dt <- noas_example
#'
#' add_interval(dt)
#' add_interval_baseline(dt)
#'
#' library(dplyr)
#' dt %>%
#'   add_interval() %>%
#'   add_interval_baseline()
#'
#' # Change the default column names
#' dt %>%
#'   add_interval(name = intv) %>%
#'   add_interval_baseline(name = bsl_intv)
NULL


#' @export
#' @rdname add_interval
#' @importFrom dplyr group_by mutate ungroup lag lead case_when
add_interval <- function(data, name = interval){
  data %>%
    add_interval_baseline(.baseline) %>%
    group_by(subject_id) %>%
    mutate(
      .lag = lag(age),
      .dup = lag(ifelse(.baseline == lead(.baseline), TRUE, FALSE)),

      {{name}} := ifelse(is.na(.lag), 0, age-.lag),
      {{name}} := case_when(
        is.na(.lag) ~ 0,
        .dup == TRUE ~ lag({{name}}),
        TRUE ~ {{name}}
      )
    ) %>%
    ungroup() %>%
    select(-.baseline, -.lag, -.dup)
}

#' @export
#' @rdname add_interval
#' @importFrom dplyr arrange group_by mutate ungroup
add_interval_baseline <- function(data, name = interval_baseline){
  check_data(data)

  if(!"age" %in% names(data))
    stop("This function needs 'age' to calculate timepoint.",
         call. = FALSE)

  data %>%
    arrange(age) %>%
    group_by(subject_id) %>%
    mutate(
      {{name}} := age-min(age)
    ) %>%
    ungroup()
}

## quiets concerns of R CMD check
if(getRversion() >= "2.15.1"){
  utils::globalVariables(c("timepoint", "subject_id", "age", ".n",
                           ":=", ".tp", "age", ".lag", ".dup",
                           "interval", ".baseline", "interval_baseline"))
}

