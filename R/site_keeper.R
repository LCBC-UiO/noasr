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
#' @param quiet logical, TRUE sets it to verbose
#' Available options are:
#' 'long' - keep data from scanner with most data (default),
#' 'ousAvanto' = keep 'ousAvanto',
#' 'ousSkyra' = keep 'ousSkyra', or
#' 'ousPrisma' = keep 'ousPrisma'.
#'
#' @return A MOAS type file with one line per subject and timepoint.

#' @examples
#' \dontrun{
#' site_keeper(MOAS)
#' site_keeper(MOAS, 'ousSkyra')
#' site_keeper(MOAS, 'ousAvanto')
#' }
#'
#' @importFrom dplyr group_by add_tally ungroup filter select mutate
#' @importFrom magrittr "%>%"
#'
#' @export
site_keeper = function(data, keep = "long", quiet = F) {

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
  if(keep %in% "long"){

    data2 <- data %>%
      #filter(Site_Name != "noMRI") %>%
      group_by(CrossProject_ID, Site_Name) %>%
      mutate(n_scans_at_site = n()) %>%
      group_by(CrossProject_ID, Subject_Timepoint) %>%
      mutate(n_replicates_at_tp = n()) %>%
      group_by(CrossProject_ID) %>%
      filter(n_replicates_at_tp == 1 |
               (n_replicates_at_tp > 1 & n_scans_at_site == max(n_scans_at_site))) %>%
      group_by(CrossProject_ID, Subject_Timepoint) %>%
      mutate(tie = n() > 1) %>%
      filter(!tie | row_number() == 1L) %>%
      ungroup() %>%
      select(-n_replicates_at_tp, -n_scans_at_site, -tie)


  }else{

    data2 <- data %>%
      dplyr::group_by(CrossProject_ID, Subject_Timepoint) %>%
      dplyr::add_tally() %>%
      dplyr::ungroup() %>%
      dplyr::mutate(Keep=ifelse(n==1,T, ifelse(Site_Name %in% keep, T, F))) %>%
      dplyr::filter(Keep) %>%
      dplyr::select(-n, -Keep)
  }

  data2 %>%
    dplyr::ungroup()
}

## quiets concerns of R CMD check
if(getRversion() >= "2.15.1"){
  utils::globalVariables(c("n",
                           "CrossProject_ID",
                           "Keep",
                           "Subject_Timepoint"))
}
