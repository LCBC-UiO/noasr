#' Check subject timepoint
#'
#' This function is intended for the RA's of LCBC to lookup,
#' using the MOAS, hich timepoint should be the next allocated
#' to a participant.
#'
#' @param ID 7-digit code, CrossProject_ID
#' @param MOAS A string with the path to, or the pre-loaded,
#' MOAS data.frame
#'
#' @return a data.frame with information on the ID
#' @export
check_tp <- function(ID=NULL, MOAS="//lagringshotell/sv-psi/LCBC/Projects/Cross_projects/MOAS/Data/MOAS.RData"){

  if(is.null(ID)){
    cat(crayon::red(paste("I need and ID to look for.")))
    stop()
  }

  if(is.null(MOAS)){
    cat(crayon::red(paste("I need the MOAS to function.",
                          "Give me either a string with the path to the file,",
                          "or a preloaded data.frame.")))
    stop()
  }else if(is.character(MOAS)){
    if(!file.exists(MOAS)){
      stop(" -- Cannot find MOAS in this path, check the path for errors.")
    }
  }

  MOAS <- get_moas(MOAS) %>%
    select(CrossProject_ID, Project_Name, Project_Wave, Test_Date, Subject_Timepoint) %>%
    distinct()

  t <- MOAS %>%
    filter(CrossProject_ID %in% ID)

  next_tp <- stringr::str_pad(round(max(t$Subject_Timepoint)+1,0), 2, 'left', '0')
  last_tp <- stringr::str_pad(round(max(t$Subject_Timepoint),0), 2, 'left', '0')

  cat(crayon::blue(
    paste0("The next timepoint for this subject ",ID ," is ", crayon::bold(next_tp),
           ". Unless it is still ",
           t[nrow(t), "Project_Name"], " wave ", t[nrow(t), "Project_Wave"],
           ", then timepoint should be ", crayon::bold(last_tp), ".\n\n")
  ))

  t
}


#' Launch shiny app to find subject timepoint
#'
#' A companion function to [\code{check_tp}], which
#' launches an interactive shiny app to find participant ID.
#'
#' @param ... arguments to [\code{shiny::runApp}]
#'
#' @export
launch_check_tp <- function(...){
  appDir <- system.file("shiny", "check_tp", package = "MOAS")
  if (appDir == "") {
    stop("Could not find shiny directory. Try re-installing `MOAS`.", call. = FALSE)
  }

  shiny::runApp(appDir, quiet = T, ...)
}
