#' Get DBS information from a file
#'
#' @param file_path path to DBS excel-file
#' @param match_file path to the match-file for DBS
#' @param debug logical to keep debug information (default: FALSE)
#'
#' @return tibble of DBS data
#' @family dbs-functions
#' @family MOAS get-functions
#' @export
#'
#' @examples
#' \dontrun{
#' dbs_get(file_path = "~/LCBC/Projects/Cross_projects/MOAS/data-raw/Blood_data/docs/d2019-08-27_Vitas-P916Resultsreport_ver3-0_final_190822.xlsx"),
#'        match_file = "~/LCBC/Projects/Cross_projects/MOAS/data-raw/Blood_data/DBS_MOAS_match.tsv")
#'
#' dbs_get(file_path = "~/LCBC/Projects/Cross_projects/MOAS/data-raw/Blood_data/docs/d2019-08-27_Vitas-P916Resultsreport_ver3-0_final_190822.xlsx"),
#'        match_file = "~/LCBC/Projects/Cross_projects/MOAS/data-raw/Blood_data/DBS_MOAS_match.tsv",
#'        debug = TRUE)
#'  }
dbs_get <- function(file_path,
                    match_file = "data-raw/Blood_data/DBS_MOAS_match.tsv",
                    debug = FALSE){

  match <- readr::read_tsv(match_file) %>%
    dplyr::filter(trusted == 1)

  dt <- rio::import_list(file_path)
  dt$Frontpage <- NULL

  dt <- lapply(dt, function(x){
    names(x)[1:3] <- c("sample_id", "species", "sample_type")
    dplyr::select(x, sample_id:Note)
  } )

  # some har several analytes in the sheet. find them and make them long.
  # assumes sheet name is compatiable with column names for analytes
  nn <- purrr::map_lgl(dt, ~ "Analyte" %in% names(.x))

  for(i in which(!nn)){
    nm <- names(dt)[i]
    dt[[nm]] <- tidyr::gather(dt[[nm]],
                              Analyte, Result,
                              starts_with(nm))
  }

  # Make sure the Result column is numeric
  # Will throw warnings of NA, which is fine
  dt <- purrr::map(dt, ~ mutate(.x,
                                Result = ifelse(grepl("<", Result),
                                                .5 * LLOQ,
                                                as.numeric(Result))
                                )
                   )

  dt <- dt %>%
    dplyr::bind_rows() %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(Analyte = gsub("\\(|\\)|:|_|-", "", Analyte),
    )

  # Merge with the match file to keep only trusted samples.
  ret_dt <- match %>%
    dplyr::left_join(dt, by = "sample_id") %>%
    dplyr::rename(DBS_Units = Units, DBS_LLOQ = LLOQ,
                  DBS_Result = Result, DBS_Analyte = Analyte) %>%
    dplyr::distinct() %>%
    filter(!is.na(DBS_Result))

  if(debug){
    ret_dt
  }else{
    dplyr::select(ret_dt,
                  -sample_id, -trusted, -comment, -Note,
                  -species, -sample_type)
  }
}

#' Add DBS data to data frame
#'
#' calls [\code{dbs_get}] to get DBS information
#' from file and add it to the MOAS-like dta frame
#'
#' @param data a MOAs--like data frame the data should be added
#' to column-wise
#' @inheritParams dbs_get
#'
#' @return tibble
#' @family dbs-funcions
#' @family MOAS add-funcions
#' @export
#'
#' @examples
#' \dontrun{
#' dbs_add(MOAS_testing,
#'        file_path = "~/LCBC/Projects/Cross_projects/MOAS/data-raw/Blood_data/docs/d2019-08-27_Vitas-P916Resultsreport_ver3-0_final_190822.xlsx"),
#'        match_file = "~/LCBC/Projects/Cross_projects/MOAS/data-raw/Blood_data/DBS_MOAS_match.tsv"
#'  }
dbs_add <- function(data,
                    file_path,
                    match_file = "data-raw/Blood_data/DBS_MOAS_match.tsv"){

  ret_dt <- dbs_get(file_path, match_file, debug = FALSE) %>%
    select(-DBS_Units, -DBS_LLOQ) %>%
    dplyr::mutate(DBS_Analyte = paste0("DBS_", DBS_Analyte)) %>%
    tidyr::spread(DBS_Analyte, DBS_Result)

    dplyr::left_join(data, ret_dt)
}



## quiets concerns of R CMD check
if(getRversion() >= "2.15.1"){
  utils::globalVariables(c("DBS_Units", "DBS_LLOQ",
                           "DBS_Analyte", "DBS_Result",
                           "trusted", "sample_id",
                           "Note", "Analyte", "LLOQ",
                           "FID", "species",
                           "sample_type", "Units", "Result"
                           ))
}


