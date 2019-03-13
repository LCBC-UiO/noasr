#' Utility function for reducing double/triple scans to a single row
#'
#' \code{site_keeper} returns a MOAS data.frame with one row per
#' participant and timepoint (i.e. removes double/triple scan entries).
#' For analyses not intending to use the power of double/triple scans,
#' or for data which will be widened and scanner type/site is not of
#' interest.
#' Only subject timepoints that have several entries (i.e. several scan sites),
#' will be reduced. All timepoints will be retained.
#'
#'
#' @param data The MOAS or a MOAS generated file.
#' @param keep A string specifying which data from double/triple scans to keep.
#' Available options are:
#' 'long' - keep data from scanner with most data (default),
#' 'ousAvanto' = keep 'ousAvanto',
#' 'ousSkyra' = keep 'ousSkyra', or
#' 'ousPrisma' = keep 'ousPrisma'.
#' @param tie string indicating given a tie in the "long" keep option, what to keep.
#' Available options are:
#' 'interval' - keep data from scanner with longest data interval (default),
#' 'ousAvanto' = keep 'ousAvanto',
#' 'ousSkyra' = keep 'ousSkyra', or
#' 'ousPrisma' = keep 'ousPrisma'.
#' @param site_order string vector of the scanner priority given a tie between scanners
#' @param quiet logical, TRUE sets it to verbose

#' @return A MOAS type file with one line per subject and timepoint.

#' @examples
#' \dontrun{
#' site_keeper(MOAS)
#' site_keeper(MOAS, 'ousSkyra')
#' site_keeper(MOAS, 'ousAvanto')
#' }
#'
#' @importFrom dplyr group_by add_tally ungroup filter select mutate slice
#' @importFrom stringr str_split
#' @importFrom magrittr "%>%"
#'
#' @export
site_keeper = function(data, keep = "long", tie = "interval", site_order = c("ousPrisma", "ousSkyra", "ousAvanto"), quiet = F) {

  if(any(!keep %in% c("long","ousAvanto","ousSkyra","ousPrisma"))){
    stop(paste0("Unrecognised option '",keep,"' for keep. Options are: 'long','ousAvanto','ousSkyra','ousPrisma'"))
  }

  if(!quiet){
    switch(keep,
           long = message("Keeping data from scanner with most data from double/triple scanned."),
           ousAvanto = message("Keeping 'ousAvanto' from double/triple scanned."),
           ousSkyra = message("Keeping 'ousSkyra' from double/triple scanned."),
           ousPrisma = message("Keeping 'ousPrisma' from triple scanned, double scanned Avanto/Skyra removed from file."))
  }

  # Decide which data to keep from double/triple scans
  if(keep == "long"){

    data2 <- find_long(data)

    if(tie == "interval"){
      stopifnot("Age" %in% names(data2))

      # Find those with longest interval
      data2 <- data2 %>%
        group_by(CrossProject_ID, Site_Name) %>%
        mutate(interval = Age-min(Age)) %>%
        mutate(n = max(interval)) %>%
        group_by(CrossProject_ID) %>%
        mutate(dups = ifelse( dup & n == max(n), TRUE, FALSE),
               dup2 = ifelse(dup == dups, TRUE, FALSE)) %>%
        filter(dup2) %>%
        select(-interval)

      # If there still are ties, keep scanner according
      # to the site_order vector
      data2 <- data2 %>%
        mutate(site = factor(Site_Name, levels = site_order)) %>%
        arrange(CrossProject_ID, Subject_Timepoint, site) %>%
        filter(!duplicated(Subject_Timepoint)) %>%
        select(-dups, -dup2, -dup, -n, -site)

    }else{
      data2 <- data2 %>%
        mutate(dup = ifelse(dup & Site_Name != tie, FALSE, TRUE)) %>%
        filter( dup) %>%
        select(-dup)
    }

  }else{
    data2 <- find_scanner(data, keep)
  }

  data2 %>%
    dplyr::ungroup()
}


find_long <- function(data){
  data %>%
    dplyr::group_by(CrossProject_ID, Site_Name) %>%
    dplyr::mutate(n_scans_at_site = dplyr::n()) %>%
    dplyr::group_by(CrossProject_ID, Subject_Timepoint) %>%
    dplyr::mutate(n_replicates_at_tp = dplyr::n()) %>%
    dplyr::group_by(CrossProject_ID) %>%
    dplyr::filter(n_replicates_at_tp == 1 |
             (n_replicates_at_tp > 1 & n_scans_at_site == max(n_scans_at_site))) %>%
    dplyr::group_by(CrossProject_ID, Subject_Timepoint) %>%
    dplyr::mutate(dup = dplyr::n() > 1) %>%
    dplyr::select(-n_replicates_at_tp, -n_scans_at_site)
}

find_scanner <- function(data, keep){
  data %>%
    dplyr::group_by(CrossProject_ID, Subject_Timepoint) %>%
    dplyr::add_tally() %>%
    dplyr::ungroup() %>%
    dplyr::mutate(Keep=ifelse(n==1,T, ifelse(Site_Name %in% keep, T, F))) %>%
    dplyr::filter(Keep) %>%
    dplyr::select(-n, -Keep)
}

## quiets concerns of R CMD check
if(getRversion() >= "2.15.1"){
  utils::globalVariables(c("n",
                           "CrossProject_ID",
                           "Keep", "site", "n_replicates_at_tp",
                           "Subject_Timepoint",
                           "dups", "dup", "dup2","interval",
                           "n_replicates_at", "n_scans_at_site",
                           "drop_na"))
}
