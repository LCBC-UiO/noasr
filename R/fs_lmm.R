#' #' Create a data.frame that is ready for use in Freesufers linear
#' #' mixed models
#' #'
#' #' \code{fs_lmm} Creates a data.frame of subsetted row and formatted
#' #' columns ready for use in Freesurfers (6.0) linear mixed models.
#' #' Design matrices are created for the grouping.var factors, and numeric
#' #' variables are z-transformed.
#' #'
#' #' @param data The MOAS or a MOAS generated file.
#' #' @param grouping.var String vector specifying the column names of
#' #' categorical/ordinal factors.
#' #' @param numeric.var A string vector of column names for numeric,
#' #' scalar co-variates.
#' #' @param missing.action Action to take on missing data i the
#' #' numeric.var
#' #'
#' #' @param keep For double/triple scans, which data should be kept.
#' #' option calls \code{site_keeper}.
#' #' @param file Optional string specifying file name to be saved
#' #'
#' #' @return a data frame ready for Freesurfer LMM use.
#' #'
#' #' @details Available options for 'missing.action' are:
#' #' \itemize{
#' #'  \item{"delete"}{delete observations with any missing numeric.vars}
#' #'  \item{"mean"}{replace missing with mean for that participant}
#' #'  \item{"all"}{replace all values with the mean for that participant}
#' #'  \item{"first"}{replace all values with the first
#' #'  observation for that participant}
#' #' }
#' #'
#' #' @examples
#' #' \dontrun{
#' #' fs_lmm(MOAS,
#' #'        grouping.var = all_of(c("Sex", "Site_Name")),
#' #'        numeric.var = all_of(c("Age", "mean_thickness"))
#' #'        )
#' #' }
#' #' @export
#'
#' fs_lmm = function(data,
#'                   grouping.var = NULL,
#'                   numeric.var = NULL,
#'                   missing = "mean",
#'                   keep = "long",
#'                   file){
#'
#'   check_data(data)
#'
#'   missing <- match.arg(missing, c("mean","all","first","delete"))
#'
#'   data <- ungroup(data) %>%
#'     mutate(.N = dplyr::row_number())
#'
#'   orig_data <- data
#'
#'   fs <- dplyr::transmute(data,
#'                          .N,
#'                          # fsid = as.character(Folder),
#'                          `fsid-base` = subject_id) %>%
#'     stats::na.omit()
#'
#'   # Remove omitted rows above in the incoming data. For merging purposes
#'   data <- dplyr::filter(data, .N %in% fs$.N)
#' browser()
#'   # Get the grouping.var data, and create one column with them pasted into eachother. Reduce factor levels to only the present ones
#'   GROUPS <- dplyr::select(data, ID, .N, {{grouping.var}})
#'   GROUPS <- dplyr::mutate_all(GROUPS, dplyr::funs(factor))
#'   GROUPS <- stats::na.omit(GROUPS)
#'
#'   # Create model.matrix (GLM) for the group variables
#'   Group.matrix = eval(parse(text=paste("model.matrix(~ ",paste(grouping.var, collapse="+"),",data=GROUPS)"))) %>% as.data.frame()
#'   tmp = names(Group.matrix)[-1];
#'   Group.matrix = as.data.frame(Group.matrix[,-1])
#'
#'   for(i in 1:length(grouping.var)){
#'     names(Group.matrix) = gsub(grouping.var[i], paste("1.",grouping.var[i],":",sep=""), tmp)
#'     tmp = names(Group.matrix)
#'   }
#'
#'   #Remove correspinding rows in the data frames
#'   fs <- dplyr::filter(fs, N %in% GROUPS$N);
#'   data <- dplyr::filter(data, N %in% GROUPS$N)
#'   GROUPS <- dplyr::select(GROUPS, -N, -ID)
#'
#'   # Get the numerical data
#'   NUMERIC = dplyr::select(data, dplyr::one_of(numeric.var))
#'
#'   #Combine the Four matrices
#'   fs = cbind.data.frame(fs,GROUPS,Group.matrix,NUMERIC)
#'
#'   # Get the mean numeric values for each particiant
#'   MEANS = fs %>%
#'     dplyr::group_by(ID) %>%
#'     dplyr::select(ID, dplyr::one_of(numeric.var))  %>%
#'     dplyr::summarise_all(dplyr::funs(mean(.,na.rm=T))) %>%
#'     as.data.frame() %>%
#'     stats::na.omit()
#'
#'   NumIdx = grep(paste(numeric.var,collapse="|"), names(fs))
#'
#'   if(missing.action != "delete"){
#'     fs =  switch(missing.action,
#'                  #Replace missing values with the mean of other values for the same person
#'                  "mean" = {
#'                    tmp=fs
#'                    for(i in NumIdx){
#'                      idx=grep(names(tmp)[i],names(MEANS))
#'                      tmp[,i] = ifelse( is.na(tmp[,i]), MEANS[match(tmp$ID, MEANS$ID),idx], tmp[,i])
#'                    }
#'                    tmp
#'
#'                    #Replace all values with the mean of other values for the same person.
#'                  },"all" = {
#'                    fs %>%
#'                      dplyr::select(-dplyr::one_of(numeric.var)) %>%
#'                      dplyr::left_join(MEANS, by="ID")
#'
#'                    #Replace all values with the first instance for the same person.
#'                  },"first" = {
#'                    FIRSTS = fs %>%
#'                      dplyr::group_by(ID) %>%
#'                      dplyr::select(dplyr::one_of(numeric.var)) %>%
#'                      stats::na.omit() %>%
#'                      dplyr::summarise_all(dplyr::funs(dplyr::first(.))) %>%
#'                      as.data.frame()
#'
#'                    fs %>%
#'                      dplyr::select(-dplyr::one_of(numeric.var)) %>%
#'                      dplyr::left_join(FIRSTS, by="ID")
#'                  }
#'
#'     ) #Switch end
#'   }
#'
#'   # Delete instances where there still is missing values, it means we cannot compute them
#'   # If missing.action is set to "delete" then this the above ifs are circumvented and this line deletes all rows that include missing values.
#'   fs = fs %>% stats::na.omit()
#'   data = data %>% dplyr::filter(N %in% fs$N)
#'
#'   if("Age" %in% numeric.var){
#'     fs = fs %>%
#'       dplyr::group_by(ID) %>%
#'       dplyr::mutate(Age=dplyr::first(Age)) %>%
#'       as.data.frame()
#'     warning("'Age' is set to base-line constant, to avoid colinearity with 'time'")
#'   }
#'
#'   #Z-transform the numerical columns
#'   SCALED= apply(fs[NumIdx], 2, scale) %>% as.data.frame()
#'   names(SCALED) = paste("Z", names(SCALED), sep=".")
#'
#'   fs = cbind.data.frame(fs,SCALED)
#'
#'   #Time needs to be corrected, because we have been deleting cases with missing data.
#'   fs$Age_orig = data$Age
#'   fs$Site_Number = data$Site_Number
#'
#'   fs = fs %>% dplyr::group_by(ID) %>%
#'     dplyr::mutate(time = dplyr::first(Age_orig)) %>%
#'     dplyr::mutate(time = Age_orig-time) %>%
#'     as.data.frame() %>%
#'     dplyr::select(-Age_orig,-N)
#'
#'   #Rename column two to what Freesurfer wants it to be.
#'   fs = fs %>%
#'     dplyr::mutate(`fsid-base`=paste("base",ID,Site_Number,sep="_")) %>%
#'     dplyr::select(-ID, -Site_Number)  %>%
#'     dplyr::select(fsid, `fsid-base`, time, dplyr::one_of(grouping.var), dplyr::one_of(numeric.var), dplyr::everything())
#'
#'   if(!missing(file)) utils::write.table(fs, file=file, sep=",", dec=".", row.names = FALSE)
#'
#'   return(fs)
#' }
#'
#' ## quiets concerns of R CMD check
#' if(getRversion() >= "2.15.1"){
#'   utils::globalVariables()
#' }
