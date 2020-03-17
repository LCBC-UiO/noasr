#' Widen the MOAS data.frame
#'
#' This is a function to create a widened data.frame from the MOAS.
#' Widening of the data is necessary for performing ANOVAs and
#' other statistical analysis, particularly if intending to run
#' in SPSS.
#'
#' For MOAS which inclused double/triple scans, by="Site_Name",
#' must be run prior to by="Project_Wave".
#'
#' @param data The MOAS or a MOAS generated file.
#' @param by Column which to widen by
#' @param keep Option to provide to [site_keeper()] for
#' filtering the double/triple scanned
#' @return A MOAS type file widened by selected column,
#' prefixed with columns specifications
#' @details 'by' options are:
#' \itemize{
#' \item{Subject_Timepoint}
#' \item{Project_Wave}
#' \item{Site_Name}
#' \item{Site_Number}
#' }
#' @examples
#' \dontrun{
#' widen(MOAS, by="Site_Name")
#' widen(MOAS, by="Project_Wave")
#'
#' # Data with double/triple scans must be widened
#' # in a two-stage process
#' dt = widen(MOAS, by="Site_Name")
#' df = widen(dt, by="Project_Wave")
#' }
#'
#' @importFrom dplyr select matches select_ one_of everything
#' @importFrom dplyr distinct arrange_ arrange filter
#' @importFrom dplyr group_by summarise anti_join group_by_at
#' @importFrom dplyr vars add_tally mutate
#' @importFrom purrr is_empty safely
#' @importFrom stats na.omit
#' @importFrom tidyr gather unite_ spread unite
#' @importFrom magrittr "%>%"
#' @export

widen = function(data, by, keep=NULL){

  ColumnList = data %>%
    dplyr::select(-dplyr::matches("MRI|PET|InBody")) %>%
    names()

  by <- match.arg(by, c("none", "Subject_Timepoint", "Project_Wave", "Site_Name", "Site_Number"))
  
  SEP = switch(by,
               "none" = "skip",
               "Subject_Timepoint" = "tp",
               "Project_Wave"      = "W",
               "Site_Name"         = "S",
               "Site_Number"       = "S"
  )

  BY = data %>% dplyr::select(!!by)

  if(!is.null(keep)){
    data = data %>%
      filter_site(keep=keep)
  }

  # browser()

  if(SEP=="skip"){
    #Does nothing...
    DATA3 = data
  }else if(SEP %in% c("W","tp")){  #If going by wave or tp

    DATA <- data

    # Paste separator infront of the by
    DATA[,by] = paste0(SEP,unlist(DATA[,by]))

    # Merge wave and project together to spread it, or else will fail with multi-project participants
    if(by == "Project_Wave"){
      DATA <- tidyr::unite(DATA, Project_Wave, c(Project_Name, Project_Wave), sep=".")
    }

    COLS = c("CrossProject_ID", "Birth_Date", "Sex", by)
    COLS = COLS[COLS %in% names(DATA)]

    # Reorder columns so we can start manipulating the data.frame
    DATA2 = DATA %>%
      dplyr::select(dplyr::one_of(COLS), by, dplyr::everything())

    DATA2 = DATA2 %>%
      tidyr::gather(variable, val, -(1:4), na.rm=T)  %>%
      dplyr::distinct() %>%
      dplyr::arrange_("CrossProject_ID",by)

    DATA4 = DATA2 %>%
      dplyr::arrange(variable) %>%
      tidyr::unite(temp, c(by,variable)) %>%
      dplyr::distinct()

    # browser()
    # Some projects have had scanning at two times within the same wave, but with
    # some time distance. We need to get that merged into a single time to make it
    # widen properly.
    tmp <- DATA4 %>% 
      dplyr::filter(grepl("Age$", temp)) %>% 
      dplyr::group_by_at(dplyr::vars(CrossProject_ID:temp)) %>% 
      dplyr::summarise(val = mean(as.numeric(val))) %>% 
      stats::na.omit()

    DATA4 = DATA4 %>%
      dplyr::anti_join(tmp, by=c("CrossProject_ID","Birth_Date","Sex", "temp")) %>%
      rbind.data.frame(tmp) %>%
      stats::na.omit() %>%
      dplyr::distinct()

    # If this data does not contain anything to widen
    if(!"temp" %in% names(DATA4)){
      stop(paste("This data has nothing to widen by", by), call. = FALSE)
    }

    ### This is where it usually goes wrong if there's something odd with the data
    DATA3 = DATA4 %>%
      safely_spread(temp, val, convert = TRUE)
    ###

    if(is.null(DATA3$result)){

      rrr <- grab_error_slice(DATA3)

      print(DATA4 %>%
        dplyr::slice(rrr))

      stop("There are duplicate entries. check the output above.")
    }

    DATA3 = DATA3$result %>%
      dplyr::select(1:3, unique(DATA4$temp))

    #Else if going by site
  }else if(any(SEP %in% c("S","T"))){
    # Define key columns that must stay in
    COLS = c("CrossProject_ID", "Project_Name", "Project_Wave", "Project_Wave_ID", "Subject_Timepoint")
    COLS = COLS[COLS %in% names(data)]

    #Create a data.frame with only cognitive stuff and PET (i.e. things measured only once pr TP)
    DATAX = data %>%
      dplyr::select(-dplyr::matches("Folder|MRI|Site|Interval", ignore.case = F)) %>%
      dplyr::select(dplyr::one_of(COLS), dplyr::everything()) %>%
      dplyr::distinct() %>%
      dplyr::filter(!is.na(CrossProject_ID))

    # Create a widened data frame
    
      DATA2 = suppressWarnings(data %>%
        dplyr::select(-dplyr::one_of(names(DATAX)[!grepl(paste(COLS,collapse="|"), names(DATAX))])) %>%
        dplyr::select(dplyr::one_of(COLS),by, dplyr::everything()) %>%
        dplyr::filter(!is.na(CrossProject_ID)) %>%
        dplyr::distinct() %>%
        tidyr::gather(temp, val, -dplyr::one_of(c(COLS, by))) %>%
        stats::na.omit() %>%
        dplyr::distinct() %>%
        dplyr::arrange(temp)
    )

    # If this data does not contain anything to widen
    if(!exists("DATA2")){
      stop(paste("This data has nothing to widen by", by), call. = FALSE)
    }

    DATA2[,by] = paste(SEP,DATA2[,by] %>% unlist(),sep="")
    DATA2 = DATA2 %>%
      tidyr::unite(temp, c(temp,by))

    DATA4 = DATA2 %>%
      safely_spread(temp, val, convert = TRUE)

    if(is.null(DATA4$result)){

      rrr <- grab_error_slice(DATA4)

      print(DATA2 %>%
              dplyr::slice(rrr))

      stop("There are duplicate entries. check the output above.", call. = FALSE)
    }

    DATA3 = DATAX %>%
      merge(DATA4$result, all=T, by = COLS) %>%
      dplyr::arrange(CrossProject_ID, Subject_Timepoint)

    DATA3 = DATA3 %>%
      dplyr::group_by(CrossProject_ID,Subject_Timepoint) %>%
      dplyr::mutate(N_Scans=sum(N_Scans)) %>% as.data.frame() %>%
      dplyr::filter(!is.na(CrossProject_ID))
  }

  #Order columns more nicely
  DATA3 %>%
    dplyr::select(dplyr::one_of(ColumnList[ColumnList %in% names(DATA3)]),
                  dplyr::everything()) %>%
    na_col_rm()
}

safely_spread <- purrr::safely(tidyr::spread)

grab_error_slice <-   function(data){
  t <- data$error$message %>%
    as.character() %>%
    stringr::str_split(":")

    gsub("[[:alpha:]]|[[:punct:]]", "", t[[1]][2]) %>%
    stringr::str_split("\\\n| ") %>%
    unlist() %>%
    as.numeric() %>%
    stats::na.omit() %>%
    as.numeric()
}


## quiets concerns of R CMD check
if(getRversion() >= "2.15.1"){
  utils::globalVariables(c("variable",
                           "val",
                           "CrossProject_ID",
                           "temp",
                           "Birth_Date",
                           "Sex",
                           "n",
                           "Subject_Timepoint",
                           "N_Scans"))
}
