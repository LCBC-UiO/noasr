#' Get epigenetic data
#'
#' @param file_path path to excel file containing epigenetic
#' data
#'
#' @param match_path path to file that matches genetic ID to
#' CrossProject_ID
#' @param debug logical, if debugging informaition should be added.
#' Defaults to FALSE
#'
#' @family epigen-functions
#' @family MOAS get-functions
#' @export
epigen_get <- function(file_path,
                       match_path = "path/to/MOAS/data-raw/DNA/gID_MOAS_match.tsv",
                       debug = FALSE){

  if(!file.exists(file_path))
    stop(paste(file_path, "does not exist or is not a path. Please check the path and file name carefully."),
         call. = FALSE)

  if(!file.exists(match_path))
    stop(paste(match_path, "does not exist or is not a path. Please check the path and file name carefully."),
         call. = FALSE)

  epigen <- rio::import(file_path)
  epigen <- dplyr::mutate(epigen, SampleID = gsub("^X", "", SampleID ))
  epigen <- dplyr::rename(epigen, FID = SampleID)
  epigen <- dplyr::rename_at(epigen, dplyr::vars(-FID), function(x) paste0("EpiGen_", x))

  match <- read_tsv(match_path)
  match <- dplyr::filter(match, for_ewas == 1)
  match <- dplyr::rename_at(match, dplyr::vars(-FID, -Genetic_ID, -CrossProject_ID, -Project_Name, -Project_Wave),
                     function(x) paste0("EpiGen_debug_", x))

  ret_dt <- dplyr::left_join(epigen, match, by="FID")
  ret_dt <- dplyr::as_tibble(ret_dt)
  ret_dt <- dplyr::select(ret_dt, CrossProject_ID, dplyr::contains("Project"), dplyr::everything())

  if(debug){
    ret_dt
  } else {
    dplyr::select(ret_dt,
           -FID, -dplyr::starts_with("EpiGen_debug_"))
  }
}


#' Add epigenetic data to MOAS like data
#'
#' @param MOAS A MOAS-derived data frame you want data added to
#'
#' @inheritParams epigen_get
#'
#' @family epigen-functions
#' @family MOAS add-functions
#'
#' @export
epigen_add <- function(MOAS,
                       file_path,
                       match_path = "path/to/MOAS/data-raw/DNA/gID_MOAS_match.tsv",
                       debug = FALSE){

  if(is.null(MOAS)) stop("MOAS-type data is missing, please provide it. ")
  if(!any("data.frame" %in% class(MOAS))) stop("You need to provide the MOAS as an already loaded data.frame.",
                                               call.=FALSE)
  if(any(!c("CrossProject_ID", "Project_Name", "Project_Wave") %in% names(MOAS)))
    stop("One of 'CrossProject_ID', 'Project_Name', 'Project_Wave' is missing from the MOAS-like data. These are needed for merging.",
         call.=FALSE)

  MOAS <- dplyr::mutate(MOAS, CrossProject_ID = as.numeric(as.character(CrossProject_ID)))
  epigen_data <- epigen_get(file_path, match_path, debug)

  dt <- dplyr::left_join(MOAS, epigen_data, by=c("CrossProject_ID", "Project_Name", "Project_Wave"))

  dplyr::mutate(dt, CrossProject_ID = as.factor(CrossProject_ID))
}

## quiets concerns of R CMD check
if(getRversion() >= "2.15.1"){
  utils::globalVariables(c("for_ewas", "Genetic_ID",
                    "SampleID"
  ))
}
