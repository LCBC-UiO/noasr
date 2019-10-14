#' Alter sensitive columns to make them less sensitive
#'
#' THe function will round Age to nearest integer, and
#' scramble participants ids, to make data less sensitive
#'
#' @inheritParams filter_site
#' @param scramble_ids make IDs anonymous
#'
#' @return tibble
#' @export
#' @importFrom dplyr mutate_at vars ends_with select distinct mutate rename everything
#' @importFrom magrittr '%>%'
mutate_sensitive <- function(data, scramble_ids = TRUE){

  ret_dt <- data %>%
    dplyr::mutate_at(dplyr::vars(dplyr::ends_with("Age")),
                     function(x) round(x, 0))

  if(scramble_ids){
    ids <- dplyr::select(ret_dt, CrossProject_ID) %>%
      dplyr::distinct() %>%
      dplyr::mutate(CrossProject_ID2 = sample(1:nrow(.), replace = FALSE))

    ret_dt <- left_join(ret_dt, ids, by="CrossProject_ID") %>%
      dplyr::select(-CrossProject_ID) %>%
      dplyr::rename(CrossProject_ID = CrossProject_ID2) %>%
      dplyr::select(CrossProject_ID, dplyr::everything())
  }
}



#' Select away sensitive columns
#'
#' This function will remove all the columns from
#' a MOAS-like data.frame that is known to include
#' possible sensitive information.
#'
#' @inheritParams filter_site
#' @param remove  logical, to remove (default) sensitive columns,
#' or select them
#'
#' @return tibble
#' @export
#' @importFrom dplyr select contains ends_with
select_sensitive <- function(data, remove = TRUE){

  if(remove){
    dplyr::select(data,
                  -one_of(c("Site_BIDS", "Folder","Project_Wave_ID")),
                  -dplyr::contains("Comment"),
                  -dplyr::contains("Note"),
                  -dplyr::ends_with("_Desc"),    # All freetext columns, may contain medical information
                  -dplyr::ends_with("_Date"),     # All dates, may be able to trace participant
                  -dplyr::contains("National"), # Anything with "national, can contain national ID
                  -dplyr::contains("Medical")  # Anything relating to medical information
    )
  }else{
    dplyr::select(data,
                  one_of(c("Site_BIDS", "Folder","Project_Wave_ID")),
                  dplyr::contains("Comment"),
                  dplyr::contains("Note"),
                  dplyr::ends_with("_Desc"),    # All freetext columns, may contain medical information
                  dplyr::ends_with("_Date"),     # All dates, may be able to trace participant
                  dplyr::contains("National"), # Anything with "national, can contain national ID
                  dplyr::contains("Medical")  # Anything relating to medical information
    )
  }
}

#' Anonymise the MOAS
#'
#' The function calls both [\code{select_sensitive}] and [\code{mutate_sensitive}]
#' to remove sensitive columns, and alter data in the MOAS to have a less
#' sensitive nature.
#'
#' @inheritParams filter_site
#' @inheritParams mutate_sensitive
#'
#' @return tibble
#' @importFrom magrittr '%>%'
#' @export
anonymize_moas <- function(data, scramble_ids = TRUE){
  select_sensitive(data, remove = TRUE) %>%
    mutate_sensitive(scramble_ids = scramble_ids)
}


## quiets concerns of R CMD check
if(getRversion() >= "2.15.1"){
  utils::globalVariables(c("CrossProject_ID2", "scramble_ids"))
}
