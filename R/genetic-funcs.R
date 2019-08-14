#' Read in and sort PGS data
#'
#' Given the path containing PGS data, this function will read in
#' PGS data for the PGS's specified and the significance levels
#' provided. In order to correctly only keep rows of data from
#' verified sources, the genetic_match_file is necessary to
#' provide for a check.
#'
#' @param pgs character vector of the PGS wanted
#' @param s_levels character vector of the significance levels wanted
#' @param pgs_path path to the directory where the PGS's are stored
#' @param genetic_match_file path to the file containing the MOAS-genetics
#' matching and debugging information
#' @param include_cnt logical, whether to keep SNP count information
#' @param include_genetics_debug logical, whether to keep all columns
#' in the genetic_match_file in the final output
#'
#' @return a tibble / data.frame
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # Here we assume you know the path to the LCBC
#' # lagringshotell, you can substitute "~" in the
#' # paths with the path to the lagringshotell
#'
#' get_pgs( pgs = c("AD", "EduYears_2016", "Depression_Nagel2018"),
#'          s_levels = c("S1", "S7", "S11"),
#'          pgs_path = "~/LCBC/Projects/Cross_projects/Genetics/PGS/PGS_20190618/PGS_wAPOE/",
#'          genetic_match_file = "~/LCBC/Projects/Cross_projects/MOAS/data-raw/DNA/gID_MOAS_match.tsv",
#' )
#'
#' # You can also toggle adding the CNT columns from the PGS, by changing
#' # include_cnt to TRUE
#' get_pgs( pgs = c("AD", "EduYears_2016", "Depression_Nagel2018",
#'          s_levels = c("S1", "S7", "S11"),
#'          pgs_path = "~/LCBC/Projects/Cross_projects/Genetics/PGS/PGS_20190618/PGS_wAPOE/",
#'          genetic_match_file = "~/LCBC/Projects/Cross_projects/MOAS/data-raw/DNA/gID_MOAS_match.tsv",
#'          include_cnt = FALSE
#' )
#'
#' }
#'
#' @importFrom dplyr filter bind_cols select matches left_join
#' @importFrom pbapply pblapply
#' @importFrom readr read_tsv cols
get_pgs <- function(pgs = c("AD", "AD_Jansen"), s_levels = c("S1", "S7", "S11"),
                    pgs_path = "~/LCBC/Projects/Cross_projects/Genetics/PGS/PGS_20190618/PGS_wAPOE/",
                    genetic_match_file = "~/LCBC/Projects/Cross_projects/MOAS/data-raw/DNA/gID_MOAS_match.tsv",
                    include_cnt = FALSE,
                    include_genetics_debug = FALSE){

  if(!dir.exists(pgs_path))
    stop(paste(pgs_path, "does not exist. Please check the path carefully."))

  if(!file.exists(genetic_match_file))
    stop(paste(genetic_match_file, "does not exist or is not a path. Please check the path and file name carefully."))

  if(!all(s_levels %in% paste0("S", 1:12)))
    stop(paste0("s_levels must be one or more of ", paste0("S", 1:12, collapse=", ")))

  if(is.null(pgs)) stop("No PGS was requested, please provide PGS as a character vector")

  # Check is pgs's asked for exist
  pgs_alts <- list.dirs(pgs_path, full.names = FALSE)
  pgs_alts <- pgs_alts[-1] # Remove parent directory listing

  if(!all(pgs %in% pgs_alts)){
    stop(paste0("Some PGS's cannot be located. Please check spelling for: ",
                paste0(pgs[!pgs %in% pgs_alts], collapse=", ")))
  }
  rm(pgs_alts)

  genetic_match = readr::read_tsv(genetic_match_file,
                                  col_type = readr::cols())
  genetic_match <- dplyr::filter(genetic_match, trusted == 1)
  names(genetic_match)[5:13] <- paste("Genetics", names(genetic_match)[5:13], sep="_")

  if(!include_genetics_debug){
    genetic_match <- genetic_match[,c("FID", "IID", "Genetic_ID", "CrossProject_ID", "Genetics_european")]
  }

  opts <- expand.grid(paste0("/", pgs), s_levels)
  all_pgs <- list.files(pgs_path, recursive = TRUE, full.names = TRUE)
  all_pgs <- all_pgs[grepl(paste(opts[,1], opts[,2], "profile", sep="\\.", collapse="|"), all_pgs)]

  # lapply with progressbar
  pgs_data <- pbapply::pblapply(all_pgs, read_pgs_file)

  pgs_data <- dplyr::bind_cols(pgs_data)
  pgs_data <- dplyr::select(pgs_data, -dplyr::matches("ID[1234567890]"))

  pgs_data <- dplyr::left_join(genetic_match, pgs_data, by = c("FID", "IID"))

  if(!include_cnt){
    pgs_data <- dplyr::select(pgs_data, -dplyr::matches(paste(s_levels, "CNT", sep="_", collapse = "|")))
  }

  pgs_data
}

#' Get all PGS's
#'
#' This function calls on [\code{get_pgs}] to read in wanted
#' all PGS at specified significance levels (by default all 12)
#'
#' @inheritParams get_pgs
#'
#' @return a tibble / data.frame
#'
#' @export
#' @examples
#' \dontrun{
#'
#' # Here we assume you know the path to the LCBC
#' # lagringshotell, you can substitute "~" in the
#' # paths with the path to the lagringshotell
#'
#' # In this version you dont need to specify the
#' # PGS you want, it will take all it finds in the
#' # folder path provided.
#' #
#'
#' get_pgs_all(
#'     s_levels = c("S1", "S7", "S11"),
#'     pgs_path = "~/LCBC/Projects/Cross_projects/Genetics/PGS/PGS_20190618/PGS_wAPOE/",
#'     genetic_match_file = "~/LCBC/Projects/Cross_projects/MOAS/data-raw/DNA/gID_MOAS_match.tsv",
#' )
#' }
get_pgs_all <- function(s_levels = paste0("S", 1:12),
                        pgs_path = "~/LCBC/Projects/Cross_projects/Genetics/PGS/PGS_20190618/PGS_wAPOE/",
                        genetic_match_file = "~/LCBC/Projects/Cross_projects/MOAS/data-raw/DNA/gID_MOAS_match.tsv",
                        include_cnt = FALSE, include_genetics_debug = FALSE){

  pgs_alts <- list.dirs(pgs_path, full.names = FALSE)
  pgs_alts <- pgs_alts[-1] # Remove parent directory listing

  get_pgs(pgs = pgs_alts, s_levels,
          pgs_path = pgs_path,
          genetic_match_file = genetic_match_file,
          include_cnt = include_cnt,
          include_genetics_debug = include_genetics_debug)

}

#' Add PGS data to the MOAS
#'
#' This function calls on [\code{get_pgs}] to read in wanted
#' PGS at specified significance levels, and adds that data
#' to the MOAS-type data provided.
#'
#' @param MOAS data.frame of the full MOAS or MOAS sub-set
#' @inheritParams get_pgs
#'
#' @return a tibble / data.frame
#'
#' @export
#' @examples
#' \dontrun{
#'
#' # Here we assume you know the path to the LCBC
#' # lagringshotell, you can substitute "~" in the
#' # paths with the path to the lagringshotell
#'
#' add_pgs(pgs = c("AD", "EduYears_2016", "Depression_Nagel2018"),
#'     s_levels = c("S1", "S7", "S11"),
#'     pgs_path = "~/LCBC/Projects/Cross_projects/Genetics/PGS/PGS_20190618/PGS_wAPOE/",
#'     genetic_match_file = "~/LCBC/Projects/Cross_projects/MOAS/data-raw/DNA/gID_MOAS_match.tsv",
#' )
#' }
#' @importFrom dplyr mutate left_join
add_pgs <- function(MOAS, pgs = NULL, s_levels = c("S1", "S7", "S11"),
                    pgs_path = "~/LCBC/Projects/Cross_projects/Genetics/PGS/PGS_20190618/PGS_wAPOE/",
                    genetic_match_file = "~/LCBC/Projects/Cross_projects/MOAS/data-raw/DNA/gID_MOAS_match.tsv",
                    include_cnt = FALSE, include_genetics_debug = FALSE){

  if(is.null(MOAS)) stop("MOAS-type data is missing, please provide it. ")
  if(!any("data.frame" %in% class(MOAS))) stop("You need to provide the MOAS as an already loaded data.frame.")

  MOAS <- dplyr::mutate(MOAS, CrossProject_ID = as.numeric(as.character(CrossProject_ID)))
  pgs_data <- get_pgs(pgs = pgs, s_levels = s_levels,
                      pgs_path = pgs_path,
                      genetic_match_file = genetic_match_file,
                      include_cnt = include_cnt, include_genetics_debug = include_genetics_debug)
  pgs_data <- pgs_data[,c(1:2)*-1]

  new_data <- dplyr::left_join(MOAS, pgs_data)
  new_data <- dplyr::mutate(MOAS, CrossProject_ID = as.factor(CrossProject_ID))

  new_data
}

#' Add all PGS data to the MOAS
#'
#' This function calls on [\code{add_pgs}] to read in all
#' PGS at specified significance levels, and adds that data
#' to the MOAS-type data provided.
#'
#' @inheritParams add_pgs
#' @inheritParams get_pgs
#'
#' @return a tibble / data.frame
#'
#' @export
#' @examples
#' \dontrun{
#'
#' # Here we assume you know the path to the LCBC
#' # lagringshotell, you can substitute "~" in the
#' # paths with the path to the lagringshotell
#'
#' # In this version you dont need to specify the
#' # PGS you want, it will take all it finds in the
#' # folder path provided, and add them to the MOAS
#' # data you provided
#'
#' add_pgs_all(
#'     s_levels = c("S1", "S7", "S11"),
#'     pgs_path = "~/LCBC/Projects/Cross_projects/Genetics/PGS/PGS_20190618/PGS_wAPOE/",
#'     genetic_match_file = "~/LCBC/Projects/Cross_projects/MOAS/data-raw/DNA/gID_MOAS_match.tsv",
#' )
#' }
add_pgs_all <- function(MOAS = NULL, s_levels = paste0("S", 1:12),
                        pgs_path = "~/LCBC/Projects/Cross_projects/Genetics/PGS/PGS_20190618/PGS_wAPOE/",
                        genetic_match_file = "~/LCBC/Projects/Cross_projects/MOAS/data-raw/DNA/gID_MOAS_match.tsv",
                        include_cnt = FALSE, include_genetics_debug = FALSE){

  pgs_alts <- list.dirs(pgs_path, full.names = FALSE)
  pgs_alts <- pgs_alts[-1] # Remove parent directory listing

  add_pgs(MOAS = MOAS, pgs = pgs_alts,
          s_levels = s_levels,
          pgs_path = pgs_path,
          genetic_match_file = genetic_match_file,
          include_cnt = include_cnt,
          include_genetics_debug = include_genetics_debug)
}

read_pgs_file <- function(path, name = NULL){
  pgs_data <- read.table(path, header = TRUE, stringsAsFactors = FALSE)

  if(is.null(name)){
    name <- gsub("\\.profile", "", basename(path))
    name <- gsub("\\.", "_", name)
  }

  names(pgs_data)[3:6] <- paste("PGS", name, names(pgs_data)[3:6], sep="_")
  names(pgs_data)[6] <- gsub("_Score", "", names(pgs_data)[6])

  pgs_data
}

