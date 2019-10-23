#' Calculates ages based on birthdates and test/MRI dates
#'
#' \code{mutate_ages} Creates a dataframe where age and subject timepoint
#' have been added to the data. Both MRI and Test age are calculated,
#' and 'Age' is the mean between these two. If only of of MRI or Test
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
    mutate(Age = ifelse(is.na(Age),
                        as.numeric(Date - Birth_Date)/365.25,
                        Age))

  data4 %>%
    arrange(CrossProject_ID, Age) %>%
    select(-Date) %>%
    as_tibble()

}



mutate_intervals <- function(data){

  data2 <- data %>%
    select(-matches("Interval_LastVisit|Interval_FirstVisit|Subject_Timepoint"))

  ## Workaround for Novel biomarkers round 4 that is messing up things.
  novelBM <- data2 %>%
    filter(Project_Number == 90) %>%
    select(CrossProject_ID, Project_Number, Project_Wave) %>%
    distinct() %>%
    group_by(CrossProject_ID) %>%
    mutate(Subject_Timepoint = row_number()) %>%
    ungroup() %>%
    full_join(data2,by = c("CrossProject_ID", "Project_Number", "Project_Wave")) %>%
    filter(Project_Number == 90)

  data3 = data2 %>%
    filter(Project_Number != 90)

  # Calculate the intervals
  data3 %>%
    bind_rows(novelBM) %>%
    group_by(CrossProject_ID) %>%
    mutate(lagAge = lag(Age)) %>%
    mutate(
      Interval_FirstVisit = Age-min(Age),
      Interval_LastVisit = ifelse(is.na(lagAge), 0, Age-lagAge),
      tt = lag(ifelse(Interval_FirstVisit == lead(Interval_FirstVisit), T, F)),
      Interval_LastVisit = ifelse(tt == TRUE, lag(Interval_LastVisit), Interval_LastVisit),
      Interval_LastVisit = ifelse(is.na(tt),0, Interval_LastVisit),

      Interval_MRI_Test = ifelse(!is.na(Test_Date) & !is.na(MRI_Date),
                                 Test_Date-MRI_Date, NA)
    )

}

mutate_tp <- function(data){
  # Calculate subject timepoint
  # There might be double/triple scanned
  # so standerd row_number might not be enough
  data4 %>%
    group_by(CrossProject_ID, Project_Wave, Project_Name) %>%
    mutate(tt = row_number(),
           tt = ifelse(tt==1, 1, 0)) %>%
    group_by(CrossProject_ID) %>%
    mutate(Subject_Timepoint = cumsum(tt)) %>%
    select(-lagAge, -tt)

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
#' @param data
#'
#' @return
#' @export
#'
#' @importFrom dplyr group_by arrange filter mutate
#' @importFrom dplyr summarise arrange
#'
#' @examples
mutate_mean_date <- function(data){

  nec <- c("CrossProject_ID", "MRI_Date", "Test_Date", "Project_Wave", "Project_Name")
  nec <- nec[! nec %in% names(data)]
  if(length(nec) > 0)
    stop(paste0("Necessary missing columns are missing from the data: ",
                paste(nec, collase=", ")), call.=FALSE)




  # Create a single Date columns. with test-Date if MRI-date is missing
  data2 = mutate(data,
                 Date = as.Date(ifelse(!is.na(MRI_Date),
                                       MRI_Date,
                                       Test_Date),
                                origin = "1970-01-01"))

  # Calculate mean dates for all projects and project waves. For those participants we are missing Dates, so we may sort the
  # data chronologically This is a necessary workaround because of participants having participated across projects, and
  # subject timepoints need to be chronological, not by project.
  DATES = data2 %>%
    group_by(Project_Name, Project_Wave) %>%
    summarise(Dates = mean.Date(Date, na.rm = T) )

  # Replace NA-dates in the data with the mean date for that project
  for (i in which(is.na(data2$Date))) {
    idx = which(DATES$Project_Name %in% data2$Project_Name[i] & DATES$Project_Wave %in% data2$Project_Wave[i])
    data2$Date[i] = DATES$Dates[idx]
  }

  # Sort the data according to ID then Date
  data2 %>%
    filter(!is.na(CrossProject_ID)) %>%
    arrange(CrossProject_ID, Date)
}

## quiets concerns of R CMD check
if(getRversion() >= "2.15.1"){
  globalVariables(c("MRI_Date",
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

