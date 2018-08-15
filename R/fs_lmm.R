#' Create a data.frame that is ready for use in Freesufers linear
#' mixed models
#'
#' \code{fs_lmm} Creates a data.frame of subsetted row and formatted
#' columns ready for use in Freesurfers (6.0) linear mixed models.
#' Design matrices are created for the grouping.var factors, and numeric
#' variables are z-transformed.
#'
#' @param data The MOAS or a MOAS generated file.
#' @param grouping.var String vector specifying the column names of
#' categorical/ordinal factors.
#' @param numeric.var A string vector of column names for numeric,
#' scalar co-variates.
#' @param missing.action Action to take on missing data for the
#' numeric.vars. Options are:
#' 'delete' - deletes observations with any missing numeric.vars,
#' 'Mean' - replaces missing with mean for that participant,
#' 'All' - replaces all values with the mean for that participant,
#' 'first' - replaced all values with the first observation for that
#' participant.
#' @param keep For double/triple scans, which data should be kept.
#' option calls \code{site_keeper}.
#' @param file Optional string specifying file name to be saved,
#' omit extention.
#'
#' @return a data frame ready for Freesurfer LMM use.

#' @examples
#' site_keeper(MOAS)
#' site_keeper(MOAS, "ousSkyra")
#' site_keeper(MOAS, "ousAvanto")
#'
#' @importFrom utils type.convert write.table
#' @import tidyverse
#'
#' @export

fs_lmm = function(data,
                  grouping.var = "Site_Name",
                  numeric.var = "Age",
                  missing.missing.action = "Mean",
                  keep = "long",
                  file = NULL){
  requireNamespace("tidyverse", quietly = TRUE)

  data = cbind.data.frame(data,N=1:nrow(data))

  # Decide which data to keep from double/triple scans
  data = data %>% site_keeper(keep=keep)

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

  # Get the grouping.var data, and create one column with them pasted into eachother. Reduce factor levels to only the present ones
  GROUPS = data %>%
    select(ID,N,one_of(grouping.var)) %>%
    mutate_all(funs(factor)) %>%
    na.omit()

  #Create model.matrix (GLM) for the group variables
  Group.matrix = eval(parse(text=paste("model.matrix(~ ",paste(grouping.var, collapse="+"),",data=GROUPS)"))) %>% as.data.frame()
  tmp=names(Group.matrix)[-1]; Group.matrix = Group.matrix[,-1] %>% as.data.frame()
  for(i in 1:length(grouping.var)){
    names(Group.matrix) = gsub(grouping.var[i], paste("1.",grouping.var[i],":",sep=""), tmp)
    tmp = names(Group.matrix)
  }

  #Remove correspinding rows in the data frames
  FS_data = FS_data %>% filter(N %in% GROUPS$N);
  data = data %>% filter(N %in% GROUPS$N)
  GROUPS = GROUPS %>% select(-N, -ID)

  # Get the numerical data
  NUMERIC = data %>% select(one_of(numeric.var))

  #Combine the Four matrices
  FS_data = cbind.data.frame(FS_data,GROUPS,Group.matrix,NUMERIC)

  # Get the mean numeric values for each particiant
  MEANS = FS_data %>%
    group_by(ID) %>%
    select(one_of(numeric.var))  %>%
    summarise_all(funs(mean(.,na.rm=T))) %>%
    as.data.frame() %>% na.omit()

  NumIdx = grep(paste(numeric.var,collapse="|"), names(FS_data))

  if(missing.action != "Delete"){
    FS_data =  switch(missing.action,
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
                          select(-one_of(numeric.var)) %>% left_join(MEANS, by="ID")

                        #Replace all values with the first instance for the same person.
                      },"First" = {
                        FIRSTS = FS_data %>%
                          group_by(ID) %>%
                          select(one_of(numeric.var)) %>%
                          na.omit() %>%
                          summarise_all(funs(first(.))) %>%
                          as.data.frame()

                        FS_data %>%
                          select(-one_of(numeric.var)) %>% left_join(FIRSTS, by="ID")
                      }

    ) #Switch end
  }

  # Delete instances where there still is missing values, it means we cannot compute them
  # If missing.action is set to "delete" then this the above ifs are circumvented and this line deletes all rows that include missing values.
  FS_data = FS_data %>% na.omit()
  data = data %>% filter(N %in% FS_data$N)

  if("Age" %in% numeric.var){
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
    select(fsid, `fsid-base`, time, one_of(grouping.var), one_of(numeric.var), everything())

  if(!is.null(file)) write.table(FS_data, file=file, sep=",", dec=".")

  return(FS_data)
}

