#' Widen the MOAS data.frame
#'
#' \code{widen} returns a widened data.frame by the columns specified
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
#' @param by Column which to widen by. Options are: "Subject_Timepoint","Project_Wave","Site_Name", or "Site_Number".
#' @param keep Option to provide to [site_keeper()] for filtering the double/triple scanned
#' @return A MOAS type file widened by dplyr::selected column, prefixed with columns specifications

#' @examples
#' \dontrun{
#' widen(MOAS, by="Site_Name")
#' widen(MOAS, by="Project_Wave")
#'
#' # Data with double/triple scans must be widened in a two-stage process
#' dt = widen(MOAS, by="Site_Name")
#' df = widen(dt, by="Project_Wave")
#' }
#'
#' @importFrom dplyr select matches one_of everything distinct arrange_ arrange filter group_by summarise anti_join left_join mutate
#' @importFrom purrr is_empty
#' @importFrom stats na.omit
#' @importFrom tidyr gather unite spread drop_na_
#' @importFrom magrittr "%>%"
#' @export
widen = function(data, by, keep=NA){

  ColumnList = data %>% dplyr::select(-dplyr::matches("MRI|PET|InBody")) %>% names()

  SEP = switch(by,
               "none" = "skip",
               "Subject_Timepoint" = "tp",
               "Project_Wave"      = "W",
               "Site_Name"         = "S",
               "Site_Number"       = "S"
  )

  BY = data %>% select_(by)

  if(!is.na(keep)){
    data = data %>%
      site_keeper(keep=keep)
  }

  if(purrr::is_empty(SEP)) stop(paste("There is no way to make wide by '", by, "'", sep=""))

  if(SEP=="skip"){
    #Does nothing...
    DATA3 = data
  }else if(SEP %in% c("W","tp")){  #If going by wave

    COLS = c("CrossProject_ID", "Birth_Date", "Sex", by)

    # Reorder columns so we can start manipulating the data.frame
    DATA2 = data %>%
      dplyr::select(one_of(COLS), by, dplyr::everything())

    DATA2 = DATA2 %>%
      tidyr::gather(variable, val, -(1:4), na.rm=T)  %>%
      dplyr::distinct() %>%
      dplyr::arrange_("CrossProject_ID",by)

    # Paste separator infront of the by
    DATA2[,by] = paste0(SEP,DATA2[,by] %>% unlist)

    DATA4 = DATA2 %>%
      dplyr::arrange(variable) %>%
      tidyr::unite_("temp", c(by, "variable") ) %>%
      dplyr::distinct()

    # NBM w4 has spread in weeks/months between Curato and Oslo.Prisma. coerce these into mean age
    tmp = DATA4 %>%
      dplyr::filter(CrossProject_ID > 9000000 &
                      grepl(paste0(c(paste0(SEP,"4_Age"), paste0(SEP,"4_Interval")), collapse="|"),temp)) %>%
      dplyr::group_by(CrossProject_ID,Birth_Date,Sex, temp) %>%
      dplyr::summarise(val=as.character(mean(as.numeric(val)))) %>% as.data.frame() %>% stats::na.omit()

    DATA4 = DATA4 %>%
      dplyr::anti_join(tmp, by=c("CrossProject_ID","Birth_Date","Sex", "temp")) %>%
      rbind.data.frame(tmp) %>%
      stats::na.omit() %>%
      dplyr::distinct()

    # If this data does not contain anything to widen
    if(!"temp" %in% names(DATA4)){
      stop(paste("This data has nothing to widen by", by))
    }

    ### This is where it usually goes wrong if there's something odd with the data
    DATA3 = DATA4 %>%
      tidyr::spread(temp, val, convert = TRUE)
    ###

    DATA3 = DATA3 %>%
      dplyr::select(1:3,DATA4$temp %>% unique)

    #Else if going by site
  }else if(any(SEP %in% c("S","T"))){
    # Define key columns that must stay in
    COLS = c("CrossProject_ID", "Project_Name", "Project_Wave", "Project_Wave_ID", "Subject_Timepoint")

    #Create a data.frame with only cognitive stuff and PET (i.e. things measured only once pr TP)
    DATAX = data %>%
      dplyr::select(-dplyr::matches("Folder|MRI|Site|Interval", ignore.case = F)) %>%
      dplyr::select(one_of(COLS), dplyr::everything()) %>%
      dplyr::distinct() %>%
      tidyr::drop_na_("CrossProject_ID")

    # Create a widened data frame
    DATA2 = data %>%
      dplyr::select(-one_of(names(DATAX)[!grepl(paste(COLS,collapse="|"), names(DATAX))])) %>%
      dplyr::select(one_of(COLS),by, dplyr::everything()) %>%
      tidyr::drop_na_("CrossProject_ID") %>%
      dplyr::distinct() %>%
      tidyr::gather(temp, val, -one_of(c(COLS,by))) %>%
      stats::na.omit() %>%
      dplyr::distinct() %>%
      dplyr::arrange(temp)

    # If this data does not contain anything to widen
    if(!exists("DATA2")){
      stop(paste("This data has nothing to widen by", by))
    }

    DATA2[,by] = paste(SEP,DATA2[,by] %>% unlist(),sep="")
    DATA2 = DATA2 %>%
      tidyr::unite(temp, c(temp,by))

    ### This is where it usually goes wrong if there's something odd with the data
    DATA4 = DATA2 %>%
      tidyr::spread(temp, val, convert = TRUE)
    ###

    DATA3 = DATAX %>%
      merge(DATA4, all=T, by = COLS) %>%
      dplyr::arrange(CrossProject_ID, Subject_Timepoint)

    DATA3 = DATA3 %>%
      dplyr::group_by(CrossProject_ID,Subject_Timepoint) %>%
      dplyr::mutate(N_Scans=sum(N_Scans)) %>% as.data.frame() %>%
      tidyr::drop_na_("CrossProject_ID")
  }

  #Order columns more nicely
  DATA3 %>%
    dplyr::select(dplyr::one_of(ColumnList[ColumnList %in% names(DATA3)]),
                  dplyr::everything()) %>%
    na.col.rm()
}
