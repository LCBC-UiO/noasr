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
#' @export
calc_ages <- function(data) {

  # Create a single Date columns. with test-Date if MRI-date is missing
  data2 <- select(data, -dplyr::matches("Interval_LastVisit|Interval_FirstVisit|Subject_Timepoint"))
  data2 <- dplyr::mutate(data2, Date = ifelse(!is.na(MRI_Date),
                                       MRI_Date,
                                       Test_Date))
  data2 <- dplyr::mutate(data2, Date = as.Date(Date, origin = "1970-01-01"))

  # Calculate mean dates for all projects and project waves. For those participants we are missing Dates, so we may sort the
  # data chronologically This is a necessary workaround because of participants having participated across projects, and
  # subject timepoints need to be chronological, not by project.
  DATES <- dplyr::group_by(data2, Project_Name, Project_Wave)
  DATES <- dplyr::summarise(DATES, Dates = mean.Date(Date, na.rm = T) )

  # Replace NA-dates in the data with the mean date for that project
  for (i in which(is.na(data2$Date))) {
    idx = which((DATES$Project_Name %in% data2$Project_Name[i] & DATES$Project_Wave %in% data2$Project_Wave[i]))
    data2$Date[i] = DATES$Dates[idx]
  }

  # Sort the data according to ID then Date
  data2 <- drop_na(data2, CrossProject_ID)
  data2 <- dplyr::arrange(data2, CrossProject_ID, Date)

  # Get ages in numeric years adjusting for leap years
  data2 = dplyr::mutate(data2,
                 MRI_Age = ifelse(!is.na(MRI_Date),
                                  as.numeric(MRI_Date-Birth_Date)/365.25,
                                  NA),
                 Test_Age = ifelse(!is.na(Test_Date),
                                   as.numeric(Test_Date-Birth_Date)/365.25,
                                   NA),
                 Age = ifelse((is.na(MRI_Age) & is.na(Test_Age)),
                              Age,
                              as.numeric(Date - Birth_Date)/365.25))
  data2 <- dplyr::mutate(data2,
                  Age = ifelse(is.na(Age),
                               as.numeric(Date - Birth_Date)/365.25,
                               Age))

  ## Workaround for Novel biomarkers round 4 that is messing up things.
  novelBM <- dplyr::filter(data2, Project_Number == 90)
  novelBM <- dplyr::select(novelBM, CrossProject_ID, Project_Number, Project_Wave)
  novelBM <- dplyr::distinct(novelBM)
  novelBM <- dplyr::group_by(novelBM, CrossProject_ID)
  novelBM <- dplyr::mutate(novelBM, Subject_Timepoint = dplyr::row_number())
  novelBM <- dplyr::ungroup(novelBM)
  novelBM <- dplyr::full_join(dplyr::filter(data2, Project_Number == 90),
                       by = c("CrossProject_ID", "Project_Number", "Project_Wave"))

  data3 = dplyr::filter(data2, Project_Number != 90)

  # Calculate the intervals
  data4 <- dplyr::bind_rows(data3, novelBM)
  data4 <- dplyr::group_by(data4, CrossProject_ID)
  data4 <- dplyr::mutate(data4, lagAge = dplyr::lag(Age))
  data4 <- dplyr::mutate(data4,
      Interval_FirstVisit = Age-min(Age),
      Interval_LastVisit = ifelse(is.na(lagAge), 0, Age-lagAge),
      tt = dplyr::lag(ifelse(Interval_FirstVisit == dplyr::lead(Interval_FirstVisit), T, F)),
      Interval_LastVisit = ifelse(tt == TRUE, dplyr::lag(Interval_LastVisit), Interval_LastVisit),
      Interval_LastVisit = ifelse(is.na(tt),0, Interval_LastVisit),

      Interval_MRI_Test = ifelse(!is.na(Test_Date) & !is.na(MRI_Date),
                                 Test_Date-MRI_Date, NA)
    )

  # Calculate subject timepoint
  data4 <- dplyr::group_by(data4, CrossProject_ID, Project_Wave, Project_Name)
  data4 <- dplyr::mutate(data4, tt = dplyr::row_number(),
           tt = ifelse(tt==1, 1, 0))
  data4 <- dplyr::group_by(data4, CrossProject_ID)
  data4 <- dplyr::mutate(data4, Subject_Timepoint = cumsum(tt))
  data4 <- dplyr::select(data4, -lagAge, -tt, -Date)

  data4 <- dplyr::arrange(data4, CrossProject_ID, Subject_Timepoint)
  dplyr::ungroup(data4)
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

