#' Create new chapter for the Project documentation
#'
#' @param project_acronym acronym of chapter
#' @param project_number type of chapter (one of "rt", "sp", "fmri", "erp")
#' @param path path to Paradigm documentation
#' @export
#' @importFrom utils file.edit
docs_project_page <- function(project_acronym = NULL,
                              project_number = NULL,
                              path = "."){

  chapters <- list.files(path, "^[[:digit:]]")
  chapters <- get_chapter_num(chapters)

  if(is.null(project_acronym)) stop("I need a project_acronym for the file")
  if(length(project_acronym)>1) stop("project_acronym can only have one value")

  if(is.null(project_number)) stop(paste0("I need a project number for the file "))
  if(length(project_number)>1) stop("project_number can only have one value")
  if(project_number %in% chapters) stop(paste0("Project number ", project_number, " already exists as a chapter, double check if correct."))

  filename <- paste(project_number, project_acronym, sep = "-")

  template_rmd <- system.file("rmarkdown/templates/lcbc-projectdoc-page/skeleton",
                              "skeleton.Rmd", package = "MOAS")
  out <- paste0(path, "/", filename, '.Rmd')
  k <- file.copy(template_rmd, out)
  file.edit(out)
}


#' Order the chapters of the Paradigm documentation
#'
#' Over time, adding new chapters to the documentation
#' can be tedious as numbering will change. This
#' functions makes that easy, by ordering and numbering
#' the chapters by `type` and the alphabetically.
#'
#' @param path path to Paradigm_documentation folder
#'
#' @export
#' @importFrom dplyr filter mutate arrange
#' @importFrom purrr walk2
#' @importFrom tidyr unite
#' @importFrom magrittr '%>%'
docs_project_order_chapter <- function(path = "."){

  # Find existing chapters
  chapters <- list.files(path, "^[[:digit:]]")
  chapters <- chapters[grep("appendices", chapters, invert = TRUE)]

  last_chpt <- get_chapter_num(chapters[length(chapters)]) + 1

  appendix_orig <- list.files(path, "appendices.Rmd$")
  appendix_new <- rm_chapter_num(appendix_orig)
  appendix_new <- paste0(last_chpt, appendix_new)

  g <- file.rename(appendix_orig, appendix_new)
}


get_chapter_num <- function(string){

  t <- sapply(string,
              function(x) strsplit(x, "-")[[1]][1]
  )

  as.numeric(unname(t))
}
