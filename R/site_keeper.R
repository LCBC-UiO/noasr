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
#'
#' @return A MOAS type file with one line per subject and timepoint.

#' @examples
#' site_keeper(MOAS)
#' site_keeper(MOAS, 'ousSkyra')
#' site_keeper(MOAS, 'ousAvanto')
#'
#' @importFrom dplyr group_by add_tally ungroup filter select mutate
#' @importFrom magrittr "%>%"
#'
#' @export
site_keeper = function(data, keep = "long") {
  switch(keep,
         long = warning("Keeping data from scanner with most data from double/triple scanned."),
         ousAvanto = warning("Keeping 'ousAvanto' from double/triple scanned."),
         ousSkyra = warning("Keeping 'ousSkyra' from double/triple scanned."),
         ousPrisma = warning("Keeping 'ousPrisma' from triple scanned, double scanned Avanto/Skyra removed from file."))

  # Decide which data to keep from double/triple scans
  if(keep %in% "long"){
    data = data %>%
      dplyr::group_by(CrossProject_ID, Site_Name) %>%
      dplyr::add_tally() %>%
      dplyr::ungroup() %>%
      dplyr::group_by(CrossProject_ID) %>%
      dplyr::filter(max(n)==n) %>%
      dplyr::select(-n)

  }else{

    data = data %>% dplyr::group_by(CrossProject_ID, Subject_Timepoint) %>%
      dplyr::add_tally() %>%
      dplyr::ungroup() %>%
      dplyr::mutate(Keep=ifelse(n==1,T, ifelse(Site_Name %in% keep, T, F))) %>%
      dplyr::filter(Keep) %>%
      dplyr::select(-n, -Keep)
  }
  return(data)
}
