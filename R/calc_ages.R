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
    select(-matches("Interval_LastVisit|Interval_FirstVisit|Subject_Timepoint")) %>%
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
  data2 = data2 %>%
    tidyr::drop_na(CrossProject_ID) %>%
    dplyr::arrange(CrossProject_ID, Date)

  # Get ages in numeric years adjusting for leap years
  data2 = data2 %>%
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
  # tmp = data2 %>%
  #   filter(Project_Number != 90) %>%
  #   dplyr::select(CrossProject_ID, Project_Number, Project_Wave, Age) %>%
  #   arrange(CrossProject_ID, Age) %>%
  #   unique() %>%
  #   dplyr::group_by(CrossProject_ID) %>%
  #   dplyr::mutate(Subject_Timepoint = dplyr::row_number()) %>%
  #   dplyr::ungroup() %>%
  #   full_join(data2 %>% select(-Age),by = c("CrossProject_ID", "Project_Number", "Project_Wave")) %>%
  #   filter(Project_Number != 90)

  ## Workaround for Novel biomarkers round 4 that is messing up things.
  novelBM <- data2 %>%
    filter(Project_Number == 90) %>%
    select(CrossProject_ID, Project_Number, Project_Wave) %>%
    distinct() %>%
    dplyr::group_by(CrossProject_ID) %>%
    dplyr::mutate(Subject_Timepoint = dplyr::row_number()) %>%
    dplyr::ungroup() %>%
    full_join(data2,by = c("CrossProject_ID", "Project_Number", "Project_Wave")) %>%
    filter(Project_Number == 90)

  data3 = data2 %>%
    filter(Project_Number != 90)%>%
    bind_rows(novelBM)

  # Calculate the intervals
  data4 = data3 %>%
    dplyr::group_by(CrossProject_ID) %>%
    dplyr::mutate(lagAge = lag(Age)) %>%
    dplyr::mutate(
      Interval_FirstVisit = Age-min(Age),
      Interval_LastVisit = ifelse(is.na(lagAge), 0, Age-lagAge),
      tt = lag(ifelse(Interval_FirstVisit == lead(Interval_FirstVisit), T, F)),
      Interval_LastVisit = ifelse(tt == TRUE, lag(Interval_LastVisit), Interval_LastVisit),
      Interval_LastVisit = ifelse(is.na(tt),0, Interval_LastVisit),

      Interval_MRI_Test = ifelse(!is.na(Test_Date) & !is.na(MRI_Date),
                                 Test_Date-MRI_Date, NA)
    )

  # Calculate subject timepoint
  data4 = data4 %>%
    group_by(CrossProject_ID, Project_Wave, Project_Name) %>%
    mutate(tt = row_number(),
           tt = ifelse(tt==1, 1, 0)) %>%
    group_by(CrossProject_ID) %>%
    mutate(Subject_Timepoint = cumsum(tt)) %>%
    dplyr::select(-lagAge, -tt, -Date)

  data4 %>%
    dplyr::arrange(CrossProject_ID, Subject_Timepoint) %>%
    dplyr::ungroup()

}

## quiets concerns of R CMD check
if(getRversion() >= "2.15.1"){
  utils::globalVariables(c("MRI_Date",
                           "Test_Date",
                           "Project_Name",
                           "Project_Wave",
                           "Date",
                           "CrossProject_ID",
                           "Birth_Date",
                           "MRI_Age",
                           "Test_Age",
                           "Age",
                           "Project_Number",
                           "full_join",
                           "bind_rows",
                           "lagAge",
                           "Interval_FirstVisit",
                           "lead",
                           "tt",
                           "Interval_LastVisit",
                           "row_number",
                           "Subject_Timepoint"))
}

