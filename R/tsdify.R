#' Creates TSD folder names for MRI data location, or
#' project wave from TSD folder names
#'
#' \code{tsdify} is a function to use to either figure out what the
#' TSD folder location of MRI data is, or to figure out the project
#' wave from TSD folder names.
#'
#' @param data The MOAS or a MOAS generated file.
#' @param reverse logical. If FALSE (default) creates TSD folder
#' strings. If TRUE, finds project wave number from Folder information.
#'
#' @return either an string vector of folder names (default), or a
#' vector of integers

#' @examples
#'  tsdify(data)
#'  tsdify(data, reverse=TRUE)
#'
#' @importFrom magrittr "%>%"
#' @importFrom dplyr mutate filter summarise distinct select one_of
#' @importFrom stringr str_pad
#' @importFrom tidyr separate
#'
#' @export
tsdify = function(data, reverse = F) {

    # Specify columns you want, in the order you want them for folder names
    COLS = c("CrossProject_ID", "Subject_Timepoint", "Project_Number", "Project_Wave")

    if (reverse) {

        tmp = data %>%
          tidyr::separate(Folder, c(COLS, "Site_Number"), remove = F) %>%
          dplyr::select(-Subject_Timepoint)

        # NCPs in resting condition
        NCPs = tmp %>%
          dplyr::filter(as.numeric(Project_Wave) %in% 0) %>%
          dplyr::distinct() %>%
          dplyr::select(CrossProject_ID)

        tmp = tmp %>%
          dplyr::mutate(Project_Wave = as.numeric(Project_Wave)) %>%
          dplyr::mutate(ifelse(CrossProject_ID %in% NCPs$CrossProject_ID, Project_Wave + 1, Project_Wave))

        return(tmp$Project_Wave)

    } else {
        if (grepl("NCP_Group", names(data)) %>% any) {
            tmp = data %>% dplyr::select(dplyr::one_of(COLS), NCP_Group)

            # Fix NCP Project_wave so the correspnd to active or inactive condition
            tmp$Project_Wave = ifelse(tmp$NCP_Group %in% "Start rest", tmp$Project_Wave - 1, tmp$Project_Wave)
            tmp = tmp %>% dplyr::select(-NCP_Group)
        } else {
            tmp = data %>% dplyr::select(dplyr::one_of(COLS))
        }

        tmp$Subject_Timepoint = stringr::str_pad(tmp$Subject_Timepoint, 2, pad = "0")
        tmp$Project_Wave = stringr::str_pad(tmp$Project_Wave, 2, pad = "0")

        Folder = apply(tmp, 1, function(x) paste(x, collapse = "_"))

        if (grepl(";", data$Site_Number) %>% any) {
            Folder = apply(tmp, 1, function(x) paste(x, collapse = "_"))
            for (i in 1:length(Folder)) {
                Folder[i] = ifelse(!is.na(data$Site_Number[i]), paste(Folder[i], strsplit(data$Site_Number[i], ";")[[1]],
                  collapse = ";", sep = "_"), NA)
            }
        } else {

            tmp$Site_Number = stringr::str_pad(data$Site_Number, 2, pad = "0")
            Folder = apply(tmp, 1, function(x) paste(x, collapse = "_"))
        }

        return(ifelse(grepl("NA", Folder), NA, Folder))
    }

}
