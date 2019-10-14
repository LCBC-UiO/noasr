#' Function to launch the LCBC shiny data explorer
#'
#' \code{launch_LCBCshiny} will launch the shiny app made to explore
#' and extract data from the MOAS.
#' Running it without supplied data will enable you to upload the
#' data you wish to explore via the browser.
#' If you have the data already loaded in your R environment, you
#' may pass this as an argument to the function to have the data
#' already loaded in the application.
#'
#' @param data The MOAS or a MOAS generated file.
#' @param ... any arguments to [\code{shiny::runApp}]
#'
#' @examples
#' \dontrun{
#' launch_LCBCshiny()
#'
#' launch_LCBCshiny(MOAS)
#' }
#' @importFrom shiny runApp
#'
#' @export

launch_LCBCshiny <- function(data, ...) {
  appDir <- system.file("shiny", "LCBCshiny", package = "MOAS")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `MOAS`.", call. = FALSE)
  }

  sapply(list.files(paste0(appDir,"/globalVars/"), full.names = TRUE), load, .GlobalEnv)

  if(!missing(data)){
    if(any(!baseCols %in% names(data))){
      stop(paste0("Data is missing key variables for shiny to run: ",
                  paste(baseCols[!baseCols %in% names(data)], collapse=", ")))
    }
    .GlobalEnv$inDATA <- data
    on.exit(rm(inDATA, envir=.GlobalEnv))
  }

  shiny::runApp(appDir, quiet = TRUE, ...)
}

## quiets concerns of R CMD check
if(getRversion() >= "2.15.1"){
  utils::globalVariables(c("baseCols",
                            "inDATA"))
}
