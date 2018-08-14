Calc_ages = function(DATA){
  requireNamespace("tidyverse")
  
  #Create a single Date columns. with test-Date if MRI-date is missing
  DATA$Date = ifelse(!is.na(DATA$MRI_Date), DATA$MRI_Date, DATA$Test_Date) %>%
    as.Date(origin="1970-01-01")
  
  #Calculate mean dates for all projects and project waves. For those participants we are missing Dates, so we may sort the data chronologically
  #This is a necessary workaround because of participants having participated across projects, and subject timepoints need to be chronological, not by project.
  DATES = DATA %>% group_by(Project_Name, Project_Wave) %>% 
    summarise(Mean= mean.Date(Date, na.rm=T), 
              Min=min(Date, na.rm = T), 
              Max=max(Date, na.rm = T)) %>% 
    as.data.frame()
  DATES = DATES %>% select(1:3) %>% rename(Dates=Mean)
  
  #Replace NA-dates in the data with the mean date for that project
  for(i in which(is.na(DATA$Date))){
    idx=(DATES$Project_Name %in% DATA$Project_Name[i] & DATES$Project_Wave %in% DATA$Project_Wave[i]) %>% which()
    DATA$Date[i] = DATES$Dates[idx]
  }
  
  #Sort the data according to ID then Date
  DATA = DATA  %>% 
    filter(!is.na(CrossProject_ID)) %>% 
    arrange(CrossProject_ID, Date)
  
  #Get ages in numeric years adjusting for leap years
  DATA$MRI_Age = ifelse(!is.na(DATA$MRI_Date),difftime(DATA$MRI_Date,DATA$Birth_Date, units = "days")/365.25,NA)
  DATA$Test_Age = ifelse(!is.na(DATA$Test_Date),difftime(DATA$Test_Date,DATA$Birth_Date, units = "days")/365.25,NA)
  
  DATA$Age = ifelse((is.na(DATA$MRI_Age) & is.na(DATA$Test_Age)), DATA$Age, (DATA$Date-DATA$Birth_Date)/365.25 )
  DATA$Age = ifelse(is.na(DATA$Age), (DATA$Date-DATA$Birth_Date)/365.25 , DATA$Age)
  
  # Subject timepoint needs a small workaround because of the double/triple scans and Novel_biomarkers round 4 with several months between double scans
  
  tmp = DATA %>% 
    select(CrossProject_ID, Project_Number, Project_Wave) %>% 
    unique %>% 
    group_by(CrossProject_ID) %>% 
    mutate(
      Subject_Timepoint = ave(CrossProject_ID, CrossProject_ID, FUN=seq_along))
      
  DATA = DATA %>% 
    select(-one_of("Subject_Timepoint")) %>% 
    left_join(tmp, by=c("CrossProject_ID","Project_Number","Project_Wave")) %>% 
    group_by(CrossProject_ID) %>% 
    mutate(
  Interval_MRI_Test = ifelse(!is.na(Test_Date)&!is.na(MRI_Date),difftime(Test_Date,MRI_Date, units = "days"),NA),
      Interval_LastVisit = lag(Age,1),
      Interval_FirstVisit = first(Age)
    ) %>% 
    mutate(
      Interval_LastVisit = Age-Interval_LastVisit,
      Interval_FirstVisit = Age-Interval_FirstVisit
    ) %>% 
    mutate(
      Interval_LastVisit = ifelse(Interval_LastVisit == 0, lag(Interval_LastVisit,1), Interval_LastVisit)
    )
  
  DATA = DATA %>% 
    arrange(CrossProject_ID, Subject_Timepoint) %>% 
    select(-Date) %>% 
    as.data.frame()
  
  return(DATA)
}
