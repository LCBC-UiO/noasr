#' Alter sensitive columns to make them less sensitive
#'
#' The function will round Age to nearest integer, and
#' scramble participants ids, to make data less sensitive.
#'
#' Scrambling IDs is default behaviour, set this to FALSE
#' if you do not wish to do so. Note that the data is then
#' still considered sensitive to some degree, be careful with it.
#' If you want your ID scramble to be reproducible, set a seed
#' before running the function.
#'
#' @inheritParams filter_site
#' @param scramble_ids make IDs anonymous
#'
#' @return tibble
#' @export
#' @importFrom dplyr mutate_at vars ends_with select distinct
#' @importFrom dplyr mutate rename everything
#' @importFrom magrittr '%>%'
mutate_sensitive <- function(data, scramble_ids = TRUE){

  ret_dt <- data %>%
    dplyr::mutate_at(dplyr::vars(dplyr::ends_with("Age")),
                     function(x) round(x, 0))

  if(scramble_ids){
    ret_dt <- ret_dt %>%
      mutate(
        CrossProject_ID = factor(CrossProject_ID,
                                 labels = sample(1:length(unique(.$CrossProject_ID)),
                                                 replace = FALSE)))
  }

  ret_dt
}



#' Deselect sensitive columns
#'
#' This function will remove all the columns from
#' a MOAS-like data.frame that are known to include
#' possible sensitive information.
#'
#' @inheritParams filter_site
#'
#' @return tibble
#' @export
#' @importFrom dplyr select contains ends_with
#' @examples
#' dt <- data.frame(ID = 1:3,
#'     Birth_Date = c("1997-01-12", "1984-01-01", "1953-09-16"),
#'    Comment = c("", "Has leukemia", "is on diazepam"),
#'    CVLT_A = c(19, 25, 29),
#'    stringsAsFactors = FALSE)
#' deselect_sensitive(dt)
deselect_sensitive <- function(data){
  dplyr::select(data,
                # All freetext columns, may contain medical information
                -dplyr::contains("Comment"),
                -dplyr::contains("Note"),
                -dplyr::ends_with("_Desc"),

                # All dates, may be able to trace participant
                -dplyr::ends_with("_Date"),

                # Anything with "national, can contain national ID
                -dplyr::contains("National"),

                # Anything relating to medical information
                -dplyr::contains("Medical")
  )
}

#' Select sensitive columns
#'
#' This function will select all the columns from
#' a MOAS-like data.frame that are known to include
#' possible sensitive information.
#'
#' @inheritParams filter_site
#'
#' @return tibble
#' @export
#' @importFrom dplyr select contains ends_with
#' @examples
#' dt <- data.frame(ID = 1:3,
#'     Birth_Date = c("1997-01-12", "1984-01-01", "1953-09-16"),
#'    Comment = c("", "Has leukemia", "is on diazepam"),
#'    CVLT_A = c(19, 25, 29),
#'    stringsAsFactors = FALSE)
#' select_sensitive(dt)
select_sensitive <- function(data){
  dplyr::select(data,
                suppressWarnings(
                  one_of(c("CrossProject_ID",
                           "Site_BIDS",
                           "Folder",
                           "Project_Wave_ID"))
                ),

                # All freetext columns, may contain medical information
                dplyr::contains("Comment"),
                dplyr::contains("Note"),
                dplyr::ends_with("_Desc"),

                # All dates, may be able to trace participant
                dplyr::ends_with("_Date"),

                # Anything with "national, can contain national ID
                dplyr::contains("National"),

                # Anything relating to medical information
                dplyr::contains("Medical")
  )
}

#' Anonymise the MOAS
#'
#' The function calls both [deselect_sensitive()]
#' and [mutate_sensitive()]
#' to remove sensitive columns, and alter data in the
#' MOAS to have a less
#' sensitive nature.
#'
#' @inheritParams filter_site
#' @inheritParams mutate_sensitive
#'
#' @return tibble
#' @importFrom magrittr '%>%'
#' @export
anonymize_moas <- function(data, scramble_ids = TRUE){
  deselect_sensitive(data) %>%
    mutate_sensitive(scramble_ids = scramble_ids)
}


## quiets concerns of R CMD check
if(getRversion() >= "2.15.1"){
  utils::globalVariables(c("CrossProject_ID2",
                           "scramble_ids"))
}
