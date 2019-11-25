# borrowed from the usethis package
edit_file <- function (path) {

  if(!file.exists(path)) stop(paste("File does not exist.\n", path), call.=FALSE)

  if(interactive()){
    cat(crayon::blue("Modifying: "))
    cat(path)
    cat("\n")
    if (rstudioapi::isAvailable() && rstudioapi::hasFun("navigateToFile")) {
      rstudioapi::navigateToFile(path)
    }
    else {
      utils::file.edit(path)
    }
    invisible(path)
  }

}
