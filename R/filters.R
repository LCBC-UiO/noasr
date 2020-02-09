
#' Utility function for reducing double/triple scans to a single row
#'
#'  returns a MOAS data.frame with one row per
#' participant and timepoint (i.e. removes double/triple scan entries).
#' For analyses not intending to use the power of double/triple scans,
#' or for data which will be widened and scanner type/site is not of
#' interest.
#' Only subject timepoints that have several entries
#' (i.e. several scan sites),
#' will be reduced. All timepoints will be retained.
#'
#' @details Available options for 'keep' are:
#' \itemize{
#'  \item{"long"}{keep data from scanner with most data (default)}
#'  \item{"ousAvanto"}{keep 'ousAvanto'}
#'  \item{"ousSkyra"}{keep 'ousSkyra'}
#'  \item{"ousPrisma"}{'ousPrisma'}
#' }
#'
#' @details Available options for 'tie' are:
#' \itemize{
#'  \item{"interval"}{keep data from scanner with longest data interval (default)}
#'  \item{"ousAvanto"}{keep 'ousAvanto'}
#'  \item{"ousSkyra"}{keep 'ousSkyra'}
#'  \item{"ousPrisma"}{'ousPrisma'}
#' }
#'
#'
#' @param data The MOAS or a MOAS generated file.
#' @param keep A string specifying which data from double/triple scans to keep.
#' @param tie string indicating given a tie in the "long" keep option, what to keep.
#' @param site_order string vector of the scanner priority given a tie between scanners
#' @param quiet logical, TRUE sets it to verbose

#' @return A MOAS type file with one line per subject and timepoint.

#' @examples
#' \dontrun{
#' filter_site(MOAS)
#' filter_site(MOAS, 'ousSkyra')
#' filter_site(MOAS, 'ousAvanto')
#' }
#'
#' @export
filter_site = function(data,
                       keep = "long",
                       tie = "interval",
                       site_order = c("ousPrisma", "ousSkyra", "ousAvanto"),
                       quiet = F) {

  keep <- match.arg(keep, c("long","ousAvanto","ousSkyra","ousPrisma"))

  if(!quiet){
    switch(keep,
           long = cat("Keeping data from scanner with most data from double/triple scanned.\n"),
           ousAvanto = cat("Keeping 'ousAvanto' from double/triple scanned.\n"),
           ousSkyra = cat("Keeping 'ousSkyra' from double/triple scanned.\n"),
           ousPrisma = cat("Keeping 'ousPrisma' from triple scanned, double scanned Avanto/Skyra removed from file.\n"))
  }

  # Decide which data to keep from double/triple scans
  if(keep == "long"){

    data2 <- find_long(data)

    if(tie == "interval"){
      stopifnot("Age" %in% names(data2))

      # Find those with longest interval
      data2 <- group_by(data2, CrossProject_ID, Site_Name)
      data2 <- dplyr::mutate(data2, interval = Age-min(Age))
      data2 <- dplyr::mutate(data2, n = max(interval))
      data2 <- dplyr::group_by(data2, CrossProject_ID)
      data2 <- dplyr::mutate(data2,
                             dups = ifelse( dup & n == max(n), TRUE, FALSE),
                             dup2 = ifelse(dup == dups, TRUE, FALSE))
      data2 <- dplyr::filter(data2, dup2)
      data2 <- dplyr::select(data2, -interval)

      # If there still are ties, keep scanner according
      # to the site_order vector
      data2 <- dplyr::mutate(data2, site = factor(Site_Name, levels = site_order))
      data2 <- dplyr::arrange(data2, CrossProject_ID, Subject_Timepoint, site)
      data2 <- dplyr::filter(data2, !duplicated(Subject_Timepoint))
      data2 <- dplyr::select(data2, -dups, -dup2, -dup, -n, -site)

    }else{
      data2 <- dplyr::mutate(data2,
                             dup = ifelse(dup & Site_Name != tie, FALSE, TRUE))
      data2 <- dplyr::filter(data2, dup)
      data2 <- dplyr::select(data2, -dup)
    }

  }else{
    data2 <- find_scanner(data, keep)
  }

  data2 <- dplyr::ungroup(data2)
  dplyr::as_tibble(data2)
}

#' Deprecated, use filter_site
#'
#' @param ... arguments to \code{\link{filter_site}}
#' @export
site_keeper <- function(...){
  cat(crayon::red("site_keeper will be deprecated in future versions, please use filter_site instead\n"))
  filter_site(...)
}

#' Filter out when a participant has trained
#'
#' Several LCBC projects have some memory training
#' experimental procedure. For some papers, this
#' might come in the way of answering hypotheses.
#' This function helps filter out participants data
#' after they have been exposed to training. Meaning
#' data before training remain, data after do not.
#'
#' @inheritParams filter_site
#' @param predicate a logical statement to identify
#' rows of data under memory experimentation
#'
#' @export
#' @return tibble
#' @examples
#'   dt <-  data.frame(
#'     CrossProject_ID = rep("1000000", 6),
#'     Site_Name = c("ousAvanto", "ousAvanto","ousAvanto",
#'                 "ousSkyra", "ousSkyra", "ousSkyra"),
#'     Subject_Timepoint = c(1:3,3:5),
#'     Age = c(8, 10, 14, 14, 17, 20),
#'     stringsAsFactors = FALSE
#'   )
#'   filter_site(dt)
filter_trainingexposed <- function(data, predicate){

  if(any(!c("CrossProject_ID", "Age") %in% names(data)))
    stop("CrossProject_ID and Age are necessary columns, make sure they are in the data",
         call. = FALSE)


  data <- dplyr::group_by(data, CrossProject_ID)
  data <- dplyr::arrange(data, Age)
  data <- dplyr::mutate(data,
                        TrainExposed = dplyr::case_when(
                          !duplicated(CrossProject_ID) ~ 0,  #set so that first obvservation always is without training exposure
                          {{predicate}} ~ 1 #set all fulfilling the predicate to be training exposed
                        )
  )
  data <- tidyr::fill(data, TrainExposed) # Will, per participant, fill inn the NA with the previous non NA value in a row-sequential manner
  data <- dplyr::mutate(data,
                        TrainExposed = ifelse(is.na(TrainExposed),
                                              0,
                                              TrainExposed)) # in case subsetting ruins the wranling
  data <- dplyr::filter(data, TrainExposed != 1)
  data <- dplyr::ungroup(data)
  data <- dplyr::select(data, -TrainExposed)
  dplyr::as_tibble(data)
}


# helpers ----
find_long <- function(data){
  data <- dplyr::group_by(data, CrossProject_ID, Site_Name)
  data <- dplyr::mutate(data, n_scans_at_site = dplyr::n())
  data <- dplyr::group_by(data, CrossProject_ID, Subject_Timepoint)
  data <- dplyr::mutate(data, n_replicates_at_tp = dplyr::n())
  data <- dplyr::group_by(data, CrossProject_ID)
  data <- dplyr::filter(data,
                        n_replicates_at_tp == 1 |
                          (n_replicates_at_tp > 1 & n_scans_at_site == max(n_scans_at_site)))
  data <- dplyr::group_by(data, CrossProject_ID, Subject_Timepoint)
  data <- dplyr::mutate(data, dup = dplyr::n() > 1)
  dplyr::select(data, -n_replicates_at_tp, -n_scans_at_site)
}

find_scanner <- function(data, keep){
  data <- dplyr::group_by(data, CrossProject_ID, Subject_Timepoint)
  data <- dplyr::add_tally(data)
  data <- dplyr::ungroup(data)
  data <- dplyr::mutate(data, Keep=ifelse(n==1,T, ifelse(Site_Name %in% keep, T, F)))
  data <- dplyr::filter(data, Keep)

  dplyr::select(data, -n, -Keep)
}

## quiets concerns of R CMD check
if(getRversion() >= "2.15.1"){
  utils::globalVariables(c("n","TrainExposed",
                           "CrossProject_ID",
                           "Keep", "site", "n_replicates_at_tp",
                           "Subject_Timepoint",
                           "dups", "dup", "dup2","interval",
                           "n_replicates_at", "n_scans_at_site",
                           "drop_na"))
}

