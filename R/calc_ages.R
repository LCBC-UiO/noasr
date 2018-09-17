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
#' calc_ages(data)
#'
#' @importFrom stats ave na.omit time
#' @importFrom dplyr select group_by summarise rename filter mutate lag first left_join arrange mutate_
#' @importFrom magrittr "%>%"
#'
#' @export
calc_ages = function(data) {

  # Create a single Date columns. with test-Date if MRI-date is missing
  data = data %>%
    dplyr::mutate(Date = ifelse(!is.na(MRI_Date), 
                                MRI_Date, 
                                Test_Date) %>% as.Date(origin = "1970-01-01"))

  # Calculate mean dates for all projects and project waves. For those participants we are missing Dates, so we may sort the
  # data chronologically This is a necessary workaround because of participants having participated across projects, and
  # subject timepoints need to be chronological, not by project.
  DATES = data %>%
    dplyr::group_by(Project_Name, Project_Wave) %>%
    dplyr::summarise(Dates = mean.Date(Date, na.rm = T) )

  # Replace NA-dates in the data with the mean date for that project
  for (i in which(is.na(data$Date))) {
    idx = (DATES$Project_Name %in% data$Project_Name[i] & DATES$Project_Wave %in% data$Project_Wave[i]) %>% which()
    data$Date[i] = DATES$Dates[idx]
  }

  # Sort the data according to ID then Date
  data = data %>%
    dplyr::filter(!is.na(CrossProject_ID)) %>%
    dplyr::arrange(CrossProject_ID, Date)

  # Get ages in numeric years adjusting for leap years
  data = data %>%
    dplyr::mutate(MRI_Age = ifelse(!is.na(MRI_Date), 
                                   as.numeric(MRI_Date-Birth_Date)/365.25, 
                                   NA),
           Test_Age = ifelse(!is.na(Test_Date), 
                             as.numeric(Test_Date-Birth_Date)/365.25, 
                             NA),
           Age = ifelse((is.na(MRI_Age) & is.na(Test_Age)), 
                        Age, 
                        as.numeric(Date - Birth_Date)/365.25)) %>%
    dplyr::mutate(Age = ifelse(is.na(Age), 
                               as.numeric(Date - Birth_Date)/365.25,
                               Age))

  # Subject timepoint needs a small workaround because of the double/triple scans and Novel_biomarkers round 4 with several
  # months between double scans
  
  tmp = data %>%
    dplyr::select(CrossProject_ID, Project_Number, Project_Wave, Age) %>%
    arrange(CrossProject_ID, Age) %>% 
    unique() %>%
    dplyr::group_by(CrossProject_ID) %>%
    dplyr::mutate(Subject_Timepoint = stats::ave(CrossProject_ID,CrossProject_ID, FUN = seq_along)) %>% 
    dplyr::mutate(lagAge = lag(Age)) %>% 
    dplyr::mutate(Interval_LastVisit = ifelse(is.na(lagAge), 0, Age-lagAge),
                  Interval_FirstVisit = Age-min(Age)
    )

  data = data %>%
    dplyr::select(-dplyr::one_of("Subject_Timepoint")) %>%
    dplyr::left_join(tmp, by = c("CrossProject_ID", "Project_Number", "Project_Wave")) %>%
    dplyr::group_by(CrossProject_ID) %>%
    dplyr::mutate(Interval_MRI_Test = ifelse(!is.na(Test_Date) & !is.na(MRI_Date), difftime(Test_Date,MRI_Date), NA))

  data %>%
    dplyr::arrange(CrossProject_ID, Subject_Timepoint) %>%
    dplyr::select(-Date) %>%
    as.data.frame()

}
