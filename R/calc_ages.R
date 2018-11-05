#' Calculates ages based on birthdates and test/MRI dates, and
#' calculated subject timepoints etc.
#'
#' \code{calc_ages} Creates a dataframe where age and subject timepoint
#' have been added to the data. Both MRI and Test age are calculated,
#' and 'Age' is the mean between these two. If only of of MRI or Test
#' age are present, this one is used for Age. Also calculates intervals
#' between test and MRI, baseline to every subsequent testing, and
#' intervall between timepoints.
#'
#' @param data The MOAS or a MOAS generated file.
#'
#' @return a data frame with added/replaced age and timepoint variables.

#' @examples
#' \dontrun{
#' calc_ages(data)
#'}
#'
#' @importFrom stats ave na.omit time
#' @importFrom dplyr select select_ group_by ungroup summarise rename filter mutate lag first left_join arrange mutate_
#' @importFrom magrittr "%>%"
#'
#' @export
calc_ages = function(data) {

  # Create a single Date columns. with test-Date if MRI-date is missing
  data2 = data %>%
    dplyr::select(-dplyr::one_of(c("Subject_Timepoint","Interval_LastVisit","Interval_FirstVisit"))) %>%
    dplyr::mutate(Date = ifelse(!is.na(MRI_Date),
                                MRI_Date,
                                Test_Date) %>% as.Date(origin = "1970-01-01"))

  # Calculate mean dates for all projects and project waves. For those participants we are missing Dates, so we may sort the
  # data chronologically This is a necessary workaround because of participants having participated across projects, and
  # subject timepoints need to be chronological, not by project.
  DATES = data2 %>%
    dplyr::group_by(Project_Name, Project_Wave) %>%
    dplyr::summarise(Dates = mean.Date(Date, na.rm = T) )

  # Replace NA-dates in the data with the mean date for that project
  for (i in which(is.na(data2$Date))) {
    idx = (DATES$Project_Name %in% data2$Project_Name[i] & DATES$Project_Wave %in% data2$Project_Wave[i]) %>% which()
    data2$Date[i] = DATES$Dates[idx]
  }

  # Sort the data according to ID then Date
  data3 = data2 %>%
    tidyr::drop_na(CrossProject_ID) %>%
    dplyr::arrange(CrossProject_ID, Date) %>% 
    dplyr::select(CrossProject_ID, Project_Wave, Project_Number, MRI_Date, Test_Date, Birth_Date, Date, Age) %>% 
    dplyr::distinct()

  # Get ages in numeric years adjusting for leap years
  data3 = data3 %>%
    dplyr::mutate(MRI_Age = ifelse(!is.na(MRI_Date),
                                   as.numeric(MRI_Date-Birth_Date)/365.25,
                                   NA),
                  Test_Age = ifelse(!is.na(Test_Date),
                                    as.numeric(Test_Date-Birth_Date)/365.25,
                                    NA),
                  Age = ifelse((is.na(MRI_Age) & is.na(Test_Age)),
                               Age,
                               as.numeric(Date - Birth_Date)/365.25)
                  ) 

  # Subject timepoint needs a small workaround because of the double/triple scans and Novel_biomarkers round 4 with several
  # months between double scans
  tmp = data3 %>%
    dplyr::group_by(CrossProject_ID) %>%
    dplyr::mutate(Subject_Timepoint = dplyr::row_number()) %>%
    dplyr::mutate(lagAge = dplyr::lag(Age)) %>%
    dplyr::mutate(Interval_LastVisit = ifelse(is.na(lagAge), 0, Age-lagAge),
                  Interval_FirstVisit = Age-min(Age,na.rm=T)
    ) %>%
    dplyr::select(-lagAge) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(data2 %>% dplyr::select(-Age), .) %>%
    dplyr::filter(Project_Number != 90) 

  ## Workaround for Novel biomarkers, round 4 is messing up things.
  novelBM <- data3 %>%
    dplyr::select(CrossProject_ID, Project_Number, Project_Wave) %>%
    dplyr::distinct() %>%
    dplyr::group_by(CrossProject_ID) %>%
    dplyr::mutate(Subject_Timepoint = dplyr::row_number()) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(data2, ., by = c("CrossProject_ID", "Project_Number", "Project_Wave")) %>%
    dplyr::group_by(CrossProject_ID) %>%
    dplyr::mutate(lagAge = dplyr::lag(Age)) %>%
    dplyr::mutate(Interval_LastVisit = ifelse(is.na(lagAge), 0, Age-lagAge),
                  Interval_FirstVisit = Age-min(Age,na.rm=T)
    ) %>%
    dplyr::select(-lagAge) %>%
    dplyr::filter(Project_Number == 90) %>% 
    dplyr::ungroup()

  tmp %>%
    dplyr::ungroup() %>% 
    rbind.data.frame(novelBM) %>% 
    dplyr::group_by(CrossProject_ID) %>%
    dplyr::mutate(Interval_MRI_Test = ifelse(!(is.na(Test_Date) & is.na(MRI_Date)),
                                             Test_Date-MRI_Date, NA)) %>%
    dplyr::arrange(CrossProject_ID, Subject_Timepoint) %>%
    dplyr::select(-Date) %>%
    dplyr::ungroup()

}
