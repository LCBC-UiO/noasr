FS_LMM = function(DATA, Grouping = "Site_Name", Numeric = "Age", action = "Mean", keep = "long"){
  requireNamespace("tidyverse")
  
  data = cbind.data.frame(DATA,N=1:nrow(DATA))
  
  switch(keep,
         "long" = warning("Keeping data from scanner with most data from double/triple scanned."),
         "ousAvanto" = warning("Keeping 'ousAvanto' from double/triple scanned."),
         "ousSkyra" = warning("Keeping 'ousSkyra' from double/triple scanned."),
         "ousPrisma" = warning("Keeping 'ousPrisma' from triple scanned, double scanned Avanto/Skyra removed from file.")
  )
  
  # Decide which data to keep from double/triple scans
  if(keep %in% "long"){
    data = data %>% group_by(CrossProject_ID, Site_Name) %>% add_tally %>% 
      ungroup %>% group_by(CrossProject_ID) %>% filter(max(n)==n) %>% select(-n)
  }else{
    
    data = data %>% group_by(CrossProject_ID, Subject_Timepoint) %>% add_tally %>% 
      ungroup %>% mutate(Keep=ifelse(n==1,T, ifelse(Site_Name %in% keep, T, F))) %>%
      filter(Keep) %>% 
      select(-n, -Keep)
  }
  
  # Get data from participants who have FS data available
  dt = names(data)[grep("aparc", names(data))[1]]
  data = data %>% filter(!is.na(get(dt)))
  
  data = data %>% 
    mutate(Folder = as.character(Folder),
           Site_Number = as.character(Site_Number),
           Site_Name = as.character(Site_Name)
    ) %>% 
    ungroup()
  names(data)[1] = "ID"
  
  FS_data = data %>% 
    transmute(N=N, 
              fsid=as.character(Folder), 
              ID=ID,
              time=Interval_FirstVisit) %>% 
    na.omit() 
  
  # Remove omitted rows above in the incoming data. For merging purposes
  data = data %>% filter(N %in% FS_data$N)
  
  # Get the grouping data, and create one column with them pasted into eachother. Reduce factor levels to only the present ones
  GROUPS = data %>% 
    select(ID,N,one_of(Grouping)) %>% 
    mutate_all(funs(factor)) %>% 
    na.omit()
  
  #Create model.matrix (GLM) for the group variables
  Group.matrix = eval(parse(text=paste("model.matrix(~ ",paste(Grouping, collapse="+"),",data=GROUPS)"))) %>% as.data.frame()
  tmp=names(Group.matrix)[-1]; Group.matrix = Group.matrix[,-1] %>% as.data.frame()
  for(i in 1:length(Grouping)){ 
    names(Group.matrix) = gsub(Grouping[i], paste("1.",Grouping[i],":",sep=""), tmp)
    tmp = names(Group.matrix)
  }
  
  #Remove correspinding rows in the data frames
  FS_data = FS_data %>% filter(N %in% GROUPS$N); 
  data = data %>% filter(N %in% GROUPS$N)
  GROUPS = GROUPS %>% select(-N, -ID)
  
  # Get the numerical data
  NUMERIC = data %>% select(one_of(Numeric))
  
  #Combine the Four matrices
  FS_data = cbind.data.frame(FS_data,GROUPS,Group.matrix,NUMERIC)
  
  # Get the mean numeric values for each particiant
  MEANS = FS_data %>% 
    group_by(ID) %>%  
    select(one_of(Numeric))  %>% 
    summarise_all(funs(mean(.,na.rm=T))) %>% 
    as.data.frame() %>% na.omit()
  
  NumIdx = grep(paste(Numeric,collapse="|"), names(FS_data))
  
  if(action != "Delete"){
    FS_data =  switch(action,
                      #Replace missing values with the mean of other values for the same person
                      "Mean" = { 
                        tmp=FS_data
                        for(i in NumIdx){
                          idx=grep(names(tmp)[i],names(MEANS))
                          tmp[,i] = ifelse( is.na(tmp[,i]), MEANS[match(tmp$ID, MEANS$ID),idx], tmp[,i])
                        }
                        tmp
                        
                        #Replace all values with the mean of other values for the same person.
                      },"All" = {
                        FS_data %>%  
                          select(-one_of(Numeric)) %>% left_join(MEANS, by="ID")
                        
                        #Replace all values with the first instance for the same person.
                      },"First" = {
                        FIRSTS = FS_data %>% 
                          group_by(ID) %>% 
                          select(one_of(Numeric)) %>% 
                          na.omit() %>% 
                          summarise_all(funs(first(.))) %>% 
                          as.data.frame()
                        
                        FS_data %>%  
                          select(-one_of(Numeric)) %>% left_join(FIRSTS, by="ID") 
                      }
                      
    ) #Switch end
  }
  
  # Delete instances where there still is missing values, it means we cannot compute them
  # If action is set to "delete" then this the above ifs are circumvented and this line deletes all rows that include missing values.
  FS_data = FS_data %>% na.omit()
  data = data %>% filter(N %in% FS_data$N)
  
  if("Age" %in% Numeric){
    FS_data = FS_data %>% group_by(ID) %>% mutate(Age=first(Age)) %>% as.data.frame()
    warning("'Age' is set to base-line constant, to avoid colinearity with 'time'")
  }
  
  #Z-transform the numerical columns
  SCALED= apply(FS_data[NumIdx], 2, scale) %>% as.data.frame()
  names(SCALED) = paste("Z", names(SCALED), sep=".")
  
  FS_data = cbind.data.frame(FS_data,SCALED)
  
  #Time needs to be corrected, because we have been deleting cases with missing data.
  FS_data$Age_orig = data$Age
  FS_data$Site_Number = data$Site_Number
  
  FS_data = FS_data %>% group_by(ID) %>% 
    mutate(time = first(Age_orig)) %>% 
    mutate(time = Age_orig-time) %>% 
    as.data.frame() %>% select(-Age_orig,-N) 
  
  #Rename column two to what Freesurfer wants it to be.
  FS_data = FS_data %>% 
    mutate(`fsid-base`=paste("base",ID,Site_Number,sep="_")) %>% 
    select(-ID, -Site_Number)  %>% 
    select(fsid, `fsid-base`, time, one_of(Grouping), one_of(Numeric), everything())
  
  return(FS_data)
}

