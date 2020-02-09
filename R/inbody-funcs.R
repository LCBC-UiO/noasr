#' Read InBody data
#'
#' This function will read in any InBody
#' file, whether it has been cleaned manually, or
#' is completely raw and unaltered. Since this
#' is intended for the MOAS, is assumes IDs are stored
#' in a meaningful way in accordance with LCBC
#' data collection principles <ID.proj.wave>
#'
#' @param path path to file
#'
#' @return data frame
#' @export
#'
#' @examples
#' \dontrun{
#'
#' path <- "~/MOAS/data-raw/InBody/OrigData/2019_10/inbody770_data.csv"
#' inbody_read(path)
#'
#' }
inbody_read <- function(path){
  if(any(grepl('"', readLines(path)))){
    inbody_read_raw(path)
  }else{
    inbody_read_cleaned(path)
  }
}

#' Get InBody data
#'
#' Will read, and do some initial cleaning of
#' the InBody data. Calls two other internal functions
#' to read the InBody data that are either pre-cleaned
#' or completely raw. Will choose for you.
#'
#' @inheritParams inbody_read
#' @param keep.limits logical - Keep "Limit" columns or not
#' @param keep.kHz  logical - Keep "kHz" columns or not
#' @param keep.partials  logical - Keep "of" columns or not
#' @param keep.indices  logical - Keep columns Levels/Scores etc. or not
#' @importFrom dplyr mutate one_of select  rename_at vars contains select
#' @importFrom crayon red
#' @return data frame
#' @export
#' @examples
#' \dontrun{
#'
#' path <- "~/MOAS/data-raw/InBody/OrigData/2019_10/inbody770_data.csv"
#' inbody_get(path)
#' inbody_get(path, keep.limits = TRUE)
#' }
inbody_get <- function(path,
                       keep.limits = FALSE,
                       keep.kHz = FALSE,
                       keep.partials = FALSE,
                       keep.indices = FALSE
){

  if(!grepl("csv$", path)){
    cat(crayon::red("Path is a directory, provide a csv file\n"))
    stop(call. = FALSE)
  }

  data <- inbody_read(path) %>%
    dplyr::mutate(
      Test_Date_Time = strptime(Test_Date_Time, format="%m.%d.%Y %H:%M:%S") %>% as.POSIXct(),
      Date = as.character(as.Date(Test_Date_Time)),
      Time = substr(Test_Date_Time, start=12, stop=19),
      #TimeOfDay = factor_times(Time)
      ) %>%
    dplyr::select(-dplyr::one_of(c("Test_Date_Time")))

  if(!keep.limits) data <- dplyr::select(data, -dplyr::contains("Limit"))
  if(!keep.kHz) data <- dplyr::select(data, -dplyr::contains("kHz"))
  if(!keep.partials) data <- dplyr::select(data, -dplyr::contains("_of_"))
  if(!keep.indices) data <- dplyr::select(data, -dplyr::contains("Index"),
                                   -dplyr::contains("Level"), -dplyr::contains("Score"))

  data %>%
    dplyr::rename_at(dplyr::vars(-1:-3), function(x) paste0("InBody_", x)) %>%
    na_col_rm()
}

#' Get all Inbody data from folder
#'
#' Will given a folder directory,
#' read, clean and return all data contained
#' in inbody770.csv files.
#'
#' @param path folder path
#' @param ... additional arguments to [/code{inbody_get}]
#'@importFrom purrr map
#' @return data frame
#' @export
#' @examples
#' \dontrun{
#'
#' path <- "~/MOAS/data-raw/InBody/OrigData/"
#' inbody_get_all(path)
#' inbody_get_all(path, keep.limits = TRUE)
#' }
inbody_get_all <- function(path, ...){
  lfiles <- list.files(path = path,
                       recursive = TRUE, full.names = TRUE,
                       pattern = "inbody770_data.csv")

  suppressMessages(
    suppressWarnings(
      data <- purrr::map(lfiles, function(x) inbody_get(x, ...))
    )
  )

  suppressWarnings(
    data <- do.call(rbind, data)
  )

  data
}


#' Add InBody data to data
#'
#' Reads and cleans InBody data from a
#' file and adds it to MOAS like data.
#'
#' @param data MOAS-like data
#' @inheritParams inbody_get
#' @param suffix suffix to add if colum names conflict
#' @param ... Additional arguments to [/code{inbody_get}]
#'
#' @return data frame
#' @export
#' @importFrom dplyr rename_at vars one_of mutate select left_join
#' @importFrom magrittr '%>%'
#' @importFrom crayon yellow
#' @examples
#' \dontrun{
#'
#' path <- "~/MOAS/data-raw/InBody/OrigData/2019_10/inbody770_data.csv"
#' inbody_add(MOAS, path)
#' inbody_add(MOAS, path, keep.limits = TRUE)
#'
#' MOAS %>%
#'    inbody_add(path, keep.kHz= TRUE)
#' }
inbody_add <- function(data, path, suffix = ".x", ...){


  bd <- inbody_get(path, ...)

  cln <- names(data)[names(data) %in% names(bd)]
  cln <- cln[grep("CrossProject_ID|Project_Number|Project_Wave", cln, invert = T)]

  if(length(cln) > 0)  {
    cat(crayon::yellow("There are column names in InBody and provided data,
that are the same. Names in the original data are suffixed with", suffix))
    cat("\n")
    data <- data %>%
      dplyr::rename_at(dplyr::vars(dplyr::one_of(cln)), function(x) paste0(x, ".x"))
  }

  data %>%
    dplyr::mutate(CrossProject_ID = as.numeric(CrossProject_ID)) %>%
    dplyr::left_join(bd)
}

#' Add all InBody data from folder to data
#'
#' Add all InBody data contained in a folder
#' to MOAS-like data provided
#'
#' @inheritParams inbody_get_all
#' @inheritParams inbody_add
#'
#' @return data frame
#' @export
#' @examples
#' \dontrun{
#'
#' path <- "~/MOAS/data-raw/InBody/OrigData/"
#' inbody_add_all(MOAS, path)
#' inbody_add_all(MOAS, path, keep.limits = TRUE)
#'
#' MOAS %>%
#'    inbody_add_all(path, keep.kHz= TRUE)
#' }
inbody_add_all <- function(data, path, suffix = ".x", ...){

  bd <- inbody_get_all(path, ...)

  cln <- names(data)[names(data) %in% names(bd)]
  cln <- cln[grep("CrossProject_ID|Project_Number|Project_Wave", cln, invert = T)]

  if(length(cln) > 0)  {
    cat(crayon::yellow("There are column names in InBody and provided data,
that are the same. Names in the original data are suffixed with", suffix))
    cat("\n")
    data <- data %>%
      dplyr::rename_at(dplyr::vars(dplyr::one_of(cln)), function(x) paste0(x, ".x"))
  }

  data %>%
    dplyr::mutate(CrossProject_ID = as.numeric(CrossProject_ID)) %>%
    dplyr::left_join(bd)
}

# Helpers ----
#' @importFrom stringr str_replace str_remove str_replace_all str_remove_all str_replace_all
inbody_clean_names <- function(data){
  names(data) <- stringr::str_replace(names(data), "[0-9]. ", "_") %>%
    stringr::str_remove(".*_") %>%
    stringr::str_replace_all("%", "pc") %>%
    stringr::str_remove_all("[[:punct:]]") %>%
    stringr::str_replace_all("  | ", "_")
  data
}

#' @importFrom dplyr rename_at rename
#' @importFrom readr parse_number
inbody_split_ID <- function(data){

  # Old files have only Wave and Number, add Project_
  if("Wave" %in% names(data)){
    data <- dplyr::rename_at(data, dplyr::vars(Wave, Number),
                      function(x) paste0("Project_", x)) %>%
      dplyr::mutate(ID = readr::parse_number(ID)) %>%
      dplyr::rename(CrossProject_ID = ID)
  }


  # If Project_Number not in col names,
  # split ID to what we expect
  if(!"Project_Number" %in% names(data)){
    suppressWarnings(
      data <- dplyr::mutate(data, ID = stringr::str_remove(ID, "<|>")) %>%
        tidyr::separate(ID, c("CrossProject_ID", "Project_Number", "Project_Wave"))
    )

    if(any(is.na(data$Project_Wave ))){
      cat(crayon::red("Some InBody data does not have correct ID column in the raw data\n"))
      cat(crayon::red("Raw data needs additions to become '<xxxxxxx.yy.zz>' \n"))
      print(dplyr::filter(data, is.na(Project_Wave)) %>% dplyr::select(1:5))
    }
  }

  dplyr::mutate_at(data,
            dplyr::vars(CrossProject_ID, Project_Number, Project_Wave), as.integer)
}

# read in unaltered inbody data
inbody_read_raw <- function(path){
  x <- readr::read_csv(path, col_types = readr::cols())
  x <- inbody_clean_names(x)
  inbody_split_ID(x)
}

# read in pre-cleaned inbody data
inbody_read_cleaned <- function(path){
  x <- readr::read_csv2(path, col_types = readr::cols())
  x <- inbody_clean_names(x)
  inbody_split_ID(x)
}


## quiets concerns of R CMD check
if(getRversion() >= "2.15.1"){
  utils::globalVariables(c(
    "Test_Date_Time",
    "Time", "read_csv2",
    "Wave", "Number"
  ))
}

