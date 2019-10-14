#' Create new chapter for the Paradigm documentation
#'
#' @param name acronym of chapter
#' @param type type of chapter (one of "rt", "sp", "fmri", "erp")
#' @param path path to Paradigm documentation
#' @param bib logical, initiate bibTex file or not
#' @export
#' @importFrom utils file.edit
docs_paradigm_page <- function(name = NULL, type = NULL, path = ".", bib=TRUE){

  types <- c("rt", "sp", "fmri", "erp")

  if(is.null(name)) stop("I need a name for the file")
  if(length(name)>1) stop("name can only have one value")

  if(is.null(type)) stop(paste0("I need a type for the file, either ",
                                paste0(types, collapse=", ")))
  if(length(type)>1) stop("type can only have one value")
  if(!type %in% types) stop(paste0("type can only be one of either ",
                                   paste0(types, collapse=", ")))

  cnt <- length(list.files(path, pattern = type)) + 1

  filename <- paste(type, name, sep = "-")

  if(bib){
    if(!dir.exists(paste0(path, "/bibTex"))) dir.create(paste0(path, "/bibTex"))

    template_bib <- system.file("rmarkdown/templates/lcbc-paradigm-page/skeleton",
                                "skeleton.bib", package = "MOAS")
    out <- paste0("bibTex/", filename, '.bib')
    file.copy(template_bib, out)
    utils::file.edit(out)
  }

  template_rmd <- system.file("rmarkdown/templates/lcbc-paradigm-page/skeleton",
                              "skeleton.Rmd", package = "MOAS")
  out <- paste0(path, "/", sprintf("%02d", cnt), "-", filename, '.Rmd')
  file.copy(template_rmd, out)
  utils::file.edit(out)
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
docs_paradigm_order_chapter <- function(path = "."){

  types <- c("rt", "sp", "fmri", "erp")

  # Find existing chapters
  chapters <- data.frame(orig = list.files(path, "Rmd"),
                         stringsAsFactors = FALSE) %>%
    dplyr::filter(grepl(paste0("-", types, collapse="|"), orig)) %>%
    dplyr::mutate(raw = rm_chapter_num(orig),
           type = get_type(orig)) %>%
    dplyr::mutate(type = factor(type, levels = types)) %>%
    dplyr::arrange(type, raw) %>%
    dplyr::mutate(num = sprintf("%02d", seq_along(raw))) %>%
    tidyr::unite(new, c(num, raw), sep="")

  purrr::walk2(chapters$orig, chapters$new, ~ file.rename(.x, .y))
}

get_type <- function(string){

  t <- sapply(string,
              function(x) strsplit(x, "-")[[1]][2]
  )

  unname(t)
}

rm_chapter_num <- function(string){

  t <- sapply(string,
         function(x) paste0("-",
         strsplit(x, "-")[[1]][-1],
         collapse = "")
  )

  unname(t)
}

if(getRversion() >= "2.15.1"){
  utils::globalVariables(c("orig", "type", "new","num"))
}
