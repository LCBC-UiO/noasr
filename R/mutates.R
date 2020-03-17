#' Calculates ages based on birthdates and test/MRI dates
#'
#' \code{mutate_ages} Creates a dataframe where age and subject timepoint
#' have been added to the data. Both MRI and Test age are calculated,
#' and 'Age' is the mean between these two. If only one of MRI or Test
#' age are present, this one is used for Age.
#'
#' @param data The MOAS or a MOAS generated file.
#'
#' @return a data frame with added/replaced age and timepoint variables.

#' @examples
#' \dontrun{
#' mutate_ages(data)
#'}
#'
#' @importFrom dplyr select select_ group_by ungroup summarise
#' @importFrom dplyr rename filter mutate lag first left_join
#' @importFrom dplyr arrange as_tibble
#' @importFrom magrittr "%>%"
#'
#' @export
mutate_ages = function(data) {

  data2 <- mutate_dates(data)

  # Get ages in numeric years adjusting for leap years
  data2 = data2 %>%
    mutate(MRI_Age = ifelse(!is.na(MRI_Date),
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

  data4 %>%
    dplyr::arrange(CrossProject_ID, Age) %>%
    dplyr::select(-Date) %>%
    dplyr::as_tibble()

}



mutate_intervals <- function(data){

  data2 <- data %>%
    dplyr::select(-dplyr::matches("Interval_LastVisit|Interval_FirstVisit|Subject_Timepoint"))

  ## Workaround for Novel biomarkers round 4 that is messing up things.
  novelBM <- data2 %>%
    dplyr::filter(Project_Number == 90) %>%
    dplyr::select(CrossProject_ID, Project_Number, Project_Wave) %>%
    dplyr::distinct() %>%
    dplyr::group_by(CrossProject_ID) %>%
    dplyr::mutate(Subject_Timepoint = dplyr::row_number()) %>%
    dplyr::ungroup() %>%
    dplyr::full_join(data2,by = c("CrossProject_ID", "Project_Number", "Project_Wave")) %>%
    dplyr::filter(Project_Number == 90)

  data3 = data2 %>%
    dplyr::filter(Project_Number != 90)

  # Calculate the intervals
  data3 %>%
    dplyr::bind_rows(novelBM) %>%
    dplyr::group_by(CrossProject_ID) %>%
    dplyr::mutate(lagAge = dplyr::lag(Age)) %>%
    dplyr::mutate(
      Interval_FirstVisit = Age-min(Age),
      Interval_LastVisit = ifelse(is.na(lagAge), 0, Age-lagAge),
      tt = dplyr::lag(ifelse(Interval_FirstVisit == dplyr::lead(Interval_FirstVisit), T, F)),
      Interval_LastVisit = ifelse(tt == TRUE, dplyr::lag(Interval_LastVisit), Interval_LastVisit),
      Interval_LastVisit = ifelse(is.na(tt),0, Interval_LastVisit),

      Interval_MRI_Test = ifelse(!is.na(Test_Date) & !is.na(MRI_Date),
                                 Test_Date-MRI_Date, NA)
    )

}

mutate_tp <- function(data){

  nec <- c("CrossProject_ID", "Project_Wave", "Project_Name")
  nec <- nec[! nec %in% names(data)]
  if(length(nec) > 0)
    stop(paste0("Necessary columns are missing from the data: ",
                paste(nec, collase=", ")), call.=FALSE)

  # Calculate subject timepoint
  # There might be double/triple scanned
  # so standerd row_number might not be enough
  data %>%
    dplyr::group_by(CrossProject_ID, Project_Wave, Project_Name) %>%
    dplyr::mutate(tt = dplyr::row_number(),
           tt = ifelse(tt==1, 1, 0)) %>%
    dplyr::group_by(CrossProject_ID) %>%
    dplyr::mutate(Subject_Timepoint = cumsum(tt)) %>%
    dplyr::select(-tt)

}


#' Calculate or infer the mean data for
#' data collected
#'
#' The data in LCBC are usually collected in
#' two sets each time, one testing and one MRI.
#' These two are usually seperated by some days
#' to a month, but some times even more.
#' We also miss some dates of testing for some
#' participants. This causes some difficulties
#' when calculating ages etc.
#'
#' This function will create a new column named code[date],
#' which will contain wither the mean date between
#' MRI and test, given we have dates on both, or either
#' one of test or MRI date we have. If both are missing,
#' the function will add in the mean date for the specific
#' Project and Wave of collection, in order to properly
#' order the data in most likely chronological order.
#'
#'
#' @param data MOAS-like data
#'
#' @return data frame
#' @export
mutate_mean_date <- function(data){

  nec <- c("CrossProject_ID", "MRI_Date", "Test_Date", "Project_Wave", "Project_Name")
  nec <- nec[! nec %in% names(data)]
  if(length(nec) > 0)
    stop(paste0("Necessary columns are missing from the data: ",
                paste(nec, collase=", ")), call.=FALSE)

  # Create a single Date columns. with test-Date if MRI-date is missing
  data2 = data %>%
    dplyr::mutate(gg = dplyr::row_number()) %>%
    dplyr::group_by(gg) %>%
    dplyr::mutate(Date = dplyr::case_when(
       !is.na(MRI_Date) & !is.na(Test_Date) ~ mean_date(MRI_Date, Test_Date),
       !is.na(MRI_Date) & is.na(Test_Date) ~ MRI_Date,
       is.na(MRI_Date) & !is.na(Test_Date) ~ Test_Date,
    )
  ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-gg)

  # Calculate mean dates for all projects and project waves. For those participants we are missing Dates, so we may sort the
  # data chronologically This is a necessary workaround because of participants having participated across projects, and
  # subject timepoints need to be chronological, not by project.
  DATES = data2 %>%
    dplyr::group_by(Project_Name, Project_Wave) %>%
    dplyr::summarise(Dates = mean.Date(Date, na.rm = TRUE) )

  # Replace NA-dates in the data with the mean date for that project
  for (i in which(is.na(data2$Date))) {
    idx = which(DATES$Project_Name %in% data2$Project_Name[i] &
                DATES$Project_Wave %in% data2$Project_Wave[i])
    data2$Date[i] = DATES$Dates[idx]
  }

  # Sort the data according to ID then Date
  data2 %>%
    dplyr::filter(!is.na(CrossProject_ID)) %>%
    dplyr::arrange(CrossProject_ID, Date)
}

mean_date <- function(date1, date2){

  dts <- cbind(date1,date2)
  diff <- rowMeans(dts, na.rm=TRUE)

  as.Date(diff, origin = "1970-01-01")
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
                    "Subject_Timepoint",
                    "mutate_dates", "data2",
                    "data4", "gg"))
}

