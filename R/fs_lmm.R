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
#' 'mean' - replaces missing with mean for that participant,
#' 'all' - replaces all values with the mean for that participant,
#' 'first' - replaced all values with the first observation for that
#' participant.
#' @param keep For double/triple scans, which data should be kept.
#' option calls \code{site_keeper}.
#' @param file Optional string specifying file name to be saved
#'
#' @return a data frame ready for Freesurfer LMM use.

#' @examples
#' \dontrun{
#' site_keeper(MOAS)
#' site_keeper(MOAS, "ousSkyra")
#' site_keeper(MOAS, "ousAvanto")
#' }
#'
#' @importFrom dplyr filter mutate ungroup transmute select one_of mutate_all funs group_by summarise_all left_join first everything
#' @importFrom stats na.omit
#' @importFrom utils write.table
#' @importFrom magrittr "%>%"
#'
#' @export

fs_lmm = function(data,
                  grouping.var,
                  numeric.var,
                  missing.action = "mean",
                  keep = "long",
                  file){

  orig_data=data

  if(missing(grouping.var) | missing(numeric.var)){
    stop("Both grouping and numeric variables must be supplied.")
  }

  # Get data from participants who have FS data available
  dt = names(data)[grep("aparc", names(data))[1]]
  reqCols = c("CrossProject_ID","Site_Number","Project_Number","Project_Wave", "Folder","Interval_FirstVisit")
  if(any(!reqCols %in% names(data)) | is.na(dt) ){

    errString  = "Data must contain"
    reqColsS=paste(reqCols[!reqCols %in% names(data)], collapse=" ,")

    if(any(!reqCols %in% names(data))) errString = paste(errString, reqColsS)
    if(any(!reqCols %in% names(data)) & is.na(dt) ) errString = paste(errString, "and")
    if(is.na(dt) ) errString = paste(errString, "at least one 'aparc' column for data verification.")

    stop(errString)
  }


  noCol = !(c(grouping.var,numeric.var) %in% names(data) )
  if(any(noCol)){
    cols = paste(c(grouping.var,numeric.var)[noCol], collapse=", ")
    stop(paste("Check spelling, there are no columns named", cols))
  }

  if(any(!missing.action %in% c("mean","all","first","delete"))){
    stop(paste0("Unrecognised option '",missing.action,"' for missing.action. Options are: 'mean','first','all','delete'"))
  }

  data = cbind.data.frame(orig_data,N=1:nrow(orig_data))

  # Decide which data to keep from double/triple scans
  data = data %>% MOAS::site_keeper(keep, quiet=T)

  data = data %>% dplyr::filter(!is.na(get(dt)))

  data = data %>%
    dplyr::mutate_at(dplyr::vars(Folder,Site_Number,Site_Name),
                     dplyr::all_vars(as.character(.))) %>%
    dplyr::ungroup()
  names(data)[grep("CrossProject_ID",names(data))] = "ID"

  FS_data = data %>%
    dplyr::transmute(N=N,
                     fsid=as.character(Folder),
                     ID=ID,
                     time=Interval_FirstVisit) %>%
    stats::na.omit()

  # Remove omitted rows above in the incoming data. For merging purposes
  data = data %>% dplyr::filter(N %in% FS_data$N)

  # Get the grouping.var data, and create one column with them pasted into eachother. Reduce factor levels to only the present ones
  GROUPS = data %>%
    dplyr::select(ID,N,dplyr::one_of(grouping.var)) %>%
    dplyr::mutate_all(dplyr::funs(factor)) %>%
    stats::na.omit()

  #Create model.matrix (GLM) for the group variables
  Group.matrix = eval(parse(text=paste("model.matrix(~ ",paste(grouping.var, collapse="+"),",data=GROUPS)"))) %>% as.data.frame()
  tmp=names(Group.matrix)[-1]; Group.matrix = Group.matrix[,-1] %>% as.data.frame()
  for(i in 1:length(grouping.var)){
    names(Group.matrix) = gsub(grouping.var[i], paste("1.",grouping.var[i],":",sep=""), tmp)
    tmp = names(Group.matrix)
  }

  #Remove correspinding rows in the data frames
  FS_data = FS_data %>% dplyr::filter(N %in% GROUPS$N);
  data = data %>% dplyr::filter(N %in% GROUPS$N)
  GROUPS = GROUPS %>% dplyr::select(-N, -ID)

  # Get the numerical data
  NUMERIC = data %>% dplyr::select(dplyr::one_of(numeric.var))

  #Combine the Four matrices
  FS_data = cbind.data.frame(FS_data,GROUPS,Group.matrix,NUMERIC)

  # Get the mean numeric values for each particiant
  MEANS = FS_data %>%
    dplyr::group_by(ID) %>%
    dplyr::select(ID, dplyr::one_of(numeric.var))  %>%
    dplyr::summarise_all(dplyr::funs(mean(.,na.rm=T))) %>%
    as.data.frame() %>%
    stats::na.omit()

  NumIdx = grep(paste(numeric.var,collapse="|"), names(FS_data))

  if(missing.action != "delete"){
    FS_data =  switch(missing.action,
                      #Replace missing values with the mean of other values for the same person
                      "mean" = {
                        tmp=FS_data
                        for(i in NumIdx){
                          idx=grep(names(tmp)[i],names(MEANS))
                          tmp[,i] = ifelse( is.na(tmp[,i]), MEANS[match(tmp$ID, MEANS$ID),idx], tmp[,i])
                        }
                        tmp

                        #Replace all values with the mean of other values for the same person.
                      },"all" = {
                        FS_data %>%
                          dplyr::select(-dplyr::one_of(numeric.var)) %>%
                          dplyr::left_join(MEANS, by="ID")

                        #Replace all values with the first instance for the same person.
                      },"first" = {
                        FIRSTS = FS_data %>%
                          dplyr::group_by(ID) %>%
                          dplyr::select(dplyr::one_of(numeric.var)) %>%
                          stats::na.omit() %>%
                          dplyr::summarise_all(dplyr::funs(dplyr::first(.))) %>%
                          as.data.frame()

                        FS_data %>%
                          dplyr::select(-dplyr::one_of(numeric.var)) %>%
                          dplyr::left_join(FIRSTS, by="ID")
                      }

    ) #Switch end
  }

  # Delete instances where there still is missing values, it means we cannot compute them
  # If missing.action is set to "delete" then this the above ifs are circumvented and this line deletes all rows that include missing values.
  FS_data = FS_data %>% stats::na.omit()
  data = data %>% dplyr::filter(N %in% FS_data$N)

  if("Age" %in% numeric.var){
    FS_data = FS_data %>%
      dplyr::group_by(ID) %>%
      dplyr::mutate(Age=dplyr::first(Age)) %>%
      as.data.frame()
    warning("'Age' is set to base-line constant, to avoid colinearity with 'time'")
  }

  #Z-transform the numerical columns
  SCALED= apply(FS_data[NumIdx], 2, scale) %>% as.data.frame()
  names(SCALED) = paste("Z", names(SCALED), sep=".")

  FS_data = cbind.data.frame(FS_data,SCALED)

  #Time needs to be corrected, because we have been deleting cases with missing data.
  FS_data$Age_orig = data$Age
  FS_data$Site_Number = data$Site_Number

  FS_data = FS_data %>% dplyr::group_by(ID) %>%
    dplyr::mutate(time = dplyr::first(Age_orig)) %>%
    dplyr::mutate(time = Age_orig-time) %>%
    as.data.frame() %>%
    dplyr::select(-Age_orig,-N)

  #Rename column two to what Freesurfer wants it to be.
  FS_data = FS_data %>%
    dplyr::mutate(`fsid-base`=paste("base",ID,Site_Number,sep="_")) %>%
    dplyr::select(-ID, -Site_Number)  %>%
    dplyr::select(fsid, `fsid-base`, time, dplyr::one_of(grouping.var), dplyr::one_of(numeric.var), dplyr::everything())

  if(!missing(file)) utils::write.table(FS_data, file=file, sep=",", dec=".")

  return(FS_data)
}

## quiets concerns of R CMD check
if(getRversion() >= "2.15.1"){
  utils::globalVariables(c("Folder",
                           "Site_Number",
                           "Site_Name",
                           ".",
                           "N",
                           "ID",
                           "Interval_FirstVisit",
                           "Age",
                           "Age_orig",
                           "fsid",
                           "fsid-base"))
}
