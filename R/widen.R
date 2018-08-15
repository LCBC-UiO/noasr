#' Widen the MOAS data.frame
#'
#' \code{widen} returns a widened data.frame by the columns specified
#'
#' This is a function to create a widened data.frame from the MOAS.
#' Widening of the data is necessary for performing ANOVAs and
#' other statistical analysis, particularly if intending to run
#' in SPSS.
#'
#' For MOAS which inclused double/triple scans, by='Site_Name',
#' must be run prior to by='Project_Wave'.
#'
#' @param data The MOAS or a MOAS generated file.
#' @param by Column which to widen by. Options are: 'Subject_Timepoint','Project_Wave','Site_Name', or 'Site_Number'.
#' @return A MOAS type file widened by selected column, prefixed with columns specifications

#' @examples
#' widen(MOAS, by='Site_Name')
#' widen(MOAS, by='Project_Wave')
#'
#' # Data with double/triple scans must be widened in a two-stage process
#' dt = widen(MOAS, by='Site_Name')
#' df = widen(dt, by='Project_Wave')
#'
#' @import tidyverse
#' @export

widen = function(data, by, ColumnList) {
    
    if (missing(ColumnList)) 
        ColumnList = data %>% select(-matches("MRI|PET|InBody")) %>% names()
    
    SEP = switch(by, none = "skip", Subject_Timepoint = "tp", Project_Wave = "W", Site_Name = "S", Site_Number = "S")
    
    if (is_empty(SEP)) 
        print(paste("There is no way to make wide by '", by, "'", sep = ""))
    
    
    if (SEP == "skip") {
        # Does nothing...
        DATA3 = data
    } else if (SEP %in% c("W", "tp")) {
        # If going by wave
        
        # Reorder columns so we can start manipulating the data.frame
        DATA2 = data %>% select(CrossProject_ID, Birth_Date, Sex, one_of(by), everything())
        
        # #Find the column that you want to make wide IndexColumn = grep(paste0(by,'$'), names(DATA2))
        DATA2 = DATA2 %>% gather(variable, val, -(1:4), na.rm = T) %>% distinct() %>% arrange_("CrossProject_ID", by)
        
        DATA2[, 4] = paste0(SEP, DATA2[, 4] %>% unlist)
        
        DATA4 = DATA2 %>% arrange(variable) %>% unite(temp, c(4, variable)) %>% distinct()
        
        # NBM w4 has spread in weeks/months between Curato and Oslo.Prisma. coerce these into mean age
        tmp = DATA4 %>% filter(CrossProject_ID > 9e+06 & temp %in% "W4_Age") %>% group_by(CrossProject_ID, Birth_Date, Sex, 
            temp) %>% summarise(val = as.character(mean(as.numeric(val)))) %>% as.data.frame() %>% na.omit()
        
        DATA4 = DATA4 %>% anti_join(tmp, by = c("CrossProject_ID", "Birth_Date", "Sex", "temp")) %>% rbind.data.frame(tmp) %>% 
            na.omit() %>% distinct
        
        
        ### This is where it usually goes wrong if there's something odd with the data
        DATA3 = DATA4 %>% spread(temp, val, convert = TRUE)
        ### 
        
        DATA3 = DATA3 %>% select(1:3, DATA4$temp %>% unique)
        
        # Else if going by site
    } else if (any(SEP %in% c("S", "T"))) {
        
        BY = data[, by]
        
        # Create a data.frame with only cognitive stuff and PET (i.e. things measured only once pr TP)
        DATAX = data %>% select(-matches("Folder|MRI|Site|PET", ignore.case = F)) %>% distinct()
        
        PET = data %>% select(CrossProject_ID, Subject_Timepoint, matches("^PET", ignore.case = F)) %>% na.omit() %>% distinct()
        
        if (!is_empty(PET) | nrow(PET) != 0) 
            DATAX = left_join(DATAX, PET, by = c("CrossProject_ID", "Subject_Timepoint"))
        
        DATAX = DATAX %>% select(CrossProject_ID, Birth_Date, Sex, Subject_Timepoint, everything())
        DATAX = DATAX %>% select(-matches("Interval|MRI")) %>% distinct() %>% drop_na_("CrossProject_ID")
        
        tmp = data %>% select((!names(data) %in% c(names(DATAX), by)) %>% which)
        
        # Create a widened data frame
        DATA2 = cbind.data.frame(data %>% select(CrossProject_ID, Subject_Timepoint), BY, tmp) %>% drop_na_("CrossProject_ID")
        names(DATA2)[grep("BY", names(DATA2))] = by
        IndexColumn = grep(by, names(DATA2))
        
        DATA4 = DATA2 %>% gather(variable, val, -(1:IndexColumn), na.rm = T) %>% na.omit() %>% distinct() %>% arrange(variable)
        DATA4[, IndexColumn] = paste(SEP, DATA4[, IndexColumn] %>% unlist(), sep = "")
        DATA4 = DATA4 %>% unite(temp, IndexColumn, variable)
        
        ### This is where it usually goes wrong if there's something odd with the data
        DATA2 = DATA4 %>% spread(temp, val, convert = TRUE)
        ### 
        
        
        DATA3 = DATAX %>% merge(DATA2, all = T, by = c("CrossProject_ID", "Subject_Timepoint")) %>% arrange(CrossProject_ID, 
            Subject_Timepoint)
        
        DATA3 = DATA3 %>% group_by(CrossProject_ID, Subject_Timepoint) %>% mutate(N_Scans = sum(N_Scans)) %>% as.data.frame() %>% 
            drop_na_("CrossProject_ID")
        names(DATA3) = gsub("_tmp", "", names(DATA3))  #Needed weird workaround for strange appendage to column names
        
    }
    
    # Order columns more nicely
    DATA3 = DATA3 %>% select(one_of(ColumnList[ColumnList %in% names(DATA3)]), everything()) %>% na.col.rm()
    
    return(DATA3)
}
