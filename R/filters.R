
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
#' @template data
#' @template site_var
#' @param keep A string specifying which data from double/triple scans to keep.
#' @param tie string indicating given a tie in the "long" keep option, what to keep.
#' @param site_order string vector of the scanner priority given a tie between scanners
#' @template verbose

#' @return A NOAS type file with one line per subject and timepoint.

#' @examples
#' # attach built-in noas example data to test
#' dt <- noas_example
#' filter_site(dt)
#' filter_site(dt, 'ousSkyra')
#' filter_site(dt, 'ousAvanto')
#' @importFrom dplyr mutate group_by arrange filter select
#' @importFrom dplyr ungroup as_tibble
#' @export
filter_site = function(data,
                       site_var = site_name,
                       keep = "long",
                       tie = "interval",
                       site_order = c("ousPrisma", "ousSkyra", "ousAvanto"),
                       verbose = TRUE) {

  keep <- match.arg(keep, c("long","ousAvanto","ousSkyra","ousPrisma"))

  if(verbose){
    switch(keep,
           long = cat("Keeping data from scanner with most data from double/triple scanned.\n"),
           ousAvanto = cat("Keeping 'ousAvanto' from double/triple scanned.\n"),
           ousSkyra = cat("Keeping 'ousSkyra' from double/triple scanned.\n"),
           ousPrisma = cat("Keeping 'ousPrisma' from triple scanned, double scanned Avanto/Skyra removed from file.\n"))
  }

  # Timepoint is not necessarily in the data,
  # calc a temp one here to use.
  data <- add_timepoint(data, .timepoint)

  # Decide which data to keep from double/triple scans
  if(keep == "long"){

    data2 <- find_long(data, {{site_var}})

    if(tie == "interval"){
      # Find those with longest interval
      data2 <- group_by(data2, subject_id, {{site_var}}) %>%
        mutate(.interval = visit_age-min(visit_age, na.rm = TRUE)) %>%
        mutate(.n = max(.interval, na.rm = TRUE))%>%
        group_by(subject_id) %>%
        mutate(.dups = ifelse(.dup & .n == max(.n, na.rm = TRUE), TRUE, FALSE),
               .dup2 = ifelse(.dup == .dups, TRUE, FALSE)) %>%
        filter(.dup2)

      # If there still are ties, keep scanner according
      # to the site_order vector
      data2 <- mutate(data2,
                      .site = factor({{site_var}}, levels = site_order)) %>%
        arrange(subject_id, .timepoint, .site) %>%
        filter(!duplicated(.timepoint)) %>%
        select(-.dups, -.dup2, -.dup,
               -.n, -.site, -.interval)

    }else{
      data2 <- mutate(data2,
                      .dup = ifelse(.dup & {{site_var}} != tie, FALSE, TRUE))
      data2 <- filter(data2, .dup)
      data2 <- select(data2, -.dup)
    }

  }else{
    data2 <- find_scanner(data, keep, {{site_var}})
  }

  ungroup(data2) %>%
    as_tibble() %>%
    select(-.timepoint)
}

# helpers ----
#' Detect long site data
#'
#' Used in filter_site to find the
#' correct isntances to remove.
#'
#' @template data
#' @template site_var
#'
#' @return data.frame where long data is filtered
#' @noRd
#' @importFrom dplyr group_by mutate filter select ungroup n
find_long <- function(data, site_var){
  group_by(data, subject_id, {{site_var}}) %>%
    mutate(.n_scans_at_site = n()) %>%
    group_by(subject_id, .timepoint) %>%
    mutate(.n_replicates_at_tp = n()) %>%
    group_by(subject_id) %>%
    filter(
      .n_replicates_at_tp == 1 |
        (.n_replicates_at_tp > 1 &
           .n_scans_at_site == max(.n_scans_at_site, na.rm = TRUE))) %>%
    group_by(subject_id, .timepoint) %>%
    mutate(.dup = n() > 1) %>%
    select(-.n_replicates_at_tp, -.n_scans_at_site) %>%
    ungroup()
}


#' Detect specific scanner
#'
#' In filter_site, when a specific scanner
#' is asked for given double/triple scan
#'
#' @template data
#' @param keep which scanner to keep
#' @template site_var
#' @noRd
#' @return data where double/triple scans are resolved based on keep.
#' @importFrom dplyr mutate group_by add_tally case_when filter select
find_scanner <- function(data, keep, site_var){
  group_by(data, subject_id, .timepoint) %>%
    add_tally(name = ".n") %>%
    ungroup() %>%
    mutate(.k = case_when(
      .n == 1 ~ TRUE,
      {{site_var}} %in% keep ~ TRUE,
      TRUE ~FALSE
    )) %>%
    filter(.k) %>%
    select(-.n, -.k)
}

## quiets concerns of R CMD check
if(getRversion() >= "2.15.1"){
  utils::globalVariables(c("site_name", ".timepoint", ".interval",
                           ".dups", ".dup2", ".n_replicates_at_tp",
                           ".n_scans_at_site", ".k", ".site"))
}

