#' @export
launch_LCBCshiny <- function(data) {
  appDir <- system.file("shiny", "LCBCshiny", package = "MOAS")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `MOAS`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
