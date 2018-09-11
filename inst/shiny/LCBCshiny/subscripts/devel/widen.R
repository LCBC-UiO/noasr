widen = function(DATA, by, ColumnList){
  
  if(missing(ColumnList)) ColumnList = DATA %>% select(-matches("MRI|PET|InBody")) %>% names()
  
  SEP = switch(by,
               "none" = "skip",
               #"Subject_Timepoint" = "tp",
               "Project_Wave"      = "W",
               "Site_Name"         = "S",
               "Site_Number"       = "S",
               "Site_Tesla"        = "T"
  )
  
  if(is_empty(SEP)) print(paste("There is no way to make wide by '", by, "'", sep=""))
  
  
  if(SEP=="skip"){
    #Does nothing...
    DATA3 = DATA
  }else if(SEP=="W"){  #If going by wave
    
    #Reorder columns so we can start manipulating the data.frame
    DATA2 = DATA %>% select(CrossProject_ID,Birth_Date,Sex, one_of(by), everything())
    
    #Find the column that you want to make wide
    IndexColumn = grep(paste(by,"$",sep=""), names(DATA2))
    
    DATA2 = DATA2 %>% gather(variable, val, -(1:IndexColumn), na.rm=T)  %>% 
      distinct() %>% arrange_("CrossProject_ID",by)
    
    DATA2[,IndexColumn] = paste(SEP,DATA2[,IndexColumn],sep="")
    
    DATA3 = DATA2 %>% unite(temp, IndexColumn, variable) %>% distinct() 
    
    # NBM w4 has spread in weeks/months between Curato and Oslo.Prisma. coerce these into mean age
    tmp = DATA3 %>% filter(CrossProject_ID > 1600000 & temp == "w4_Age") %>% 
      group_by(CrossProject_ID,Birth_Date,Sex, temp) %>% 
      summarise(val=as.character(mean(as.numeric(val)))) %>% as.data.frame() %>% na.omit()
    
    DATA3 = DATA3 %>% 
      anti_join(tmp, by=c("CrossProject_ID","Birth_Date","Sex", "temp")) %>% 
      bind_rows(tmp) %>% na.omit() %>% distinct
    
    ### This is where it usually goes wrong if there's something odd with the data
    DATA3 = DATA3 %>% spread(temp, val)
    ### 
    
    #Else if going by site
  }else if(any(SEP %in% c("S","T"))){
    
    BY = DATA[,by]
    
    #Create a data.frame with only cognitive stuff and PET (i.e. things measured only once pr TP)
    DATAX = DATA %>% select(-matches("Folder|MRI|Site|PET", ignore.case = F)) %>% distinct() 
    
    PET = DATA %>% select(CrossProject_ID, Subject_Timepoint, matches("^PET", ignore.case = F)) %>% na.omit() %>% distinct()
    
    if(!is_empty(PET)|nrow(PET)!=0)  DATAX = left_join(DATAX, PET, by = c("CrossProject_ID", "Subject_Timepoint"))
    
    DATAX = DATAX %>% select(CrossProject_ID,Birth_Date,Sex,Subject_Timepoint, everything())
    DATAX = DATAX %>% select(-matches("Interval|MRI")) %>% distinct() %>% drop_na_("CrossProject_ID")
    
    tmp = DATA  %>% select((!names(DATA) %in% c(names(DATAX),by)) %>% which)
    
    #Create a widened data frame
    DATA2 = cbind.data.frame(DATA %>% select(CrossProject_ID,Subject_Timepoint),BY,tmp) %>% drop_na_("CrossProject_ID")
    names(DATA2)[grep("BY",names(DATA2))] = by
    IndexColumn = grep(by, names(DATA2))
    
    DATA2 = DATA2 %>% gather(variable, val, -(1:IndexColumn), na.rm=T) %>% 
      na.omit() %>% distinct() %>% arrange(CrossProject_ID,Subject_Timepoint) 
    DATA2[,IndexColumn] = paste(SEP,DATA2[,IndexColumn],sep="")
    DATA2 = DATA2 %>% unite(temp, IndexColumn, variable) 
    
    ### This is where it usually goes wrong if there's something odd with the data
    DATA2 = DATA2 %>% spread(temp, val) 
    ###
    
    
    DATA3 = DATAX %>% left_join(DATA2,  by = c("CrossProject_ID", "Subject_Timepoint"), all=T) %>% 
      arrange(CrossProject_ID, Subject_Timepoint)
    
    DATA3 = DATA3 %>% 
      group_by(CrossProject_ID,Subject_Timepoint) %>% 
      mutate(N_Scans=sum(N_Scans, na.rm = T)) %>% 
      as.data.frame() %>% 
      drop_na_("CrossProject_ID")
    names(DATA3) = gsub("_tmp","",names(DATA3)) #Needed weird workaround for strange appendage to column names
    
  }
  
  #Order columns more nicely
  DATA3 = DATA3 %>% select(one_of(ColumnList[ColumnList %in% names(DATA3)]), everything()) %>% na.col.rm()
  
  return(DATA3)  
}
