#' Create a data.frame that is ready for use in Freesufers linear
#' mixed models
#'
#' \code{fs_lmm} Creates a data.frame of subsetted row and formatted
#' columns ready for use in Freesurfers (6.0) linear mixed models.
#' Design matrices are created for the grouping.var factors, and numeric
#' variables are z-transformed.
#'
#' @template data
#' @param formula right-hand formula for your model. (ex. ~ age * sex)
#' @template site_var
#' @param folder_var unqoted column name with folder-information of MRI data
#' from the 'mri_info' table.
#' @param missing Action to take on missing data that are numeric
#' @param ... other arguments to \code{\link{utils}{model.matrix}}
#' @param file Optional string specifying file name to be saved
#'
#' @return a data frame ready for Freesurfer LMM use.
#'
#' @details Available options for 'missing.action' are:
#' \itemize{
#'  \item{"delete"}{delete observations with any missing numeric.vars}
#'  \item{"mean"}{replace missing with mean for that participant}
#'  \item{"all"}{replace all values with the mean for that participant}
#'  \item{"first"}{replace all values with the first
#'  observation for that participant}
#' }
#' @importFrom stats model.matrix update
#' @importFrom utils capture.output type.convert write.table
#' @importFrom dplyr mutate arrange group_by ungroup
#' @importFrom dplyr filter row_number as_tibble across
#' @importFrom dplyr left_join anti_join rename_with all_of
# #' @importFrom dplyr where
#' @examples
#' \dontrun{
#' fs_lmm(data,
#'        age * sex * cog
#'      )
#' }
#' @export
fs_lmm = function(data,
                  formula,
                  site_var,
                  folder_var,
                  missing = "mean",
                  ...,
                  file = NULL){
  stopifnot(class(formula) == "formula")
  check_data(data)

  if(!any("age" == names(data)))
    stop("'age' must be in the data, even if not in the formula, in order to sort the data correctly.\n",
         call. = FALSE)


  missing <- match.arg(missing, c("mean","all","first","delete"))

  cols <- all.vars(formula)

  if(any(is.na(data$age))){
    NAs <- filter(data, is.na(age)) %>%
      select(.N, subject_id, project_id, wave_code,
             {{site_var}}, {{folder_var}})
    warning("There are `NA` values in the 'age' column. These data points will be removed.\n",
            paste0(capture.output(NAs), collapse="\n"),
            "\n",
            call. = FALSE)
    data <- filter(data, !is.na(age))
  }

  data <- arrange(data, subject_id, age) %>%
    mutate(.N = row_number(),
           fsid = {{folder_var}},
           `fsid-base` = sprintf("base_%s_%s",
                                 subject_id,
                                 site_name2number({{site_var}}))
    )

  # to keep track of removed data.
  orig_data <- data %>%
    select(.N, subject_id, project_id, wave_code,
           {{site_var}}, {{folder_var}})

  data <- select(data,
                 .N, subject_id, fsid, `fsid-base`,
                 all_of(cols))

  data <- fix_numeric(data, missing) %>%
    select(-subject_id)

  new_formula <- update(formula, ~. + .N)

  mmatrix <- model.matrix(new_formula, data, ...) %>%
    as_tibble() %>%
    type.convert() %>%
    mutate(across(where(is.mnumber) & !.N, custom_scale)) %>%
    rename_with(col_rename, where(is.mnumber) & !.N)
  names(mmatrix)[1] <- "intercept"

  fs <- left_join(data, mmatrix, by = ".N", suffix = c("", "_z"))

  removed <- anti_join(orig_data, fs, by = ".N") %>%
    select(-.N)

  if(nrow(removed) > 0){
    warning("Some data have been removed.\n",
            paste0(capture.output(removed), collapse="\n"),
            "\n",
            call. = FALSE)
  }

  fs <- select(fs, -.N)

  if(!is.null(file))
    write.table(fs, file = file,
                sep = ",", dec = ".",
                row.names = FALSE)

  return(fs)
}

fix_numeric <- function(data, missing){
  if(missing != "delete"){
    func <- switch(missing,
                   first = replace_all_first,
                   mean = replace_na_mean,
                   all = replace_all_mean)

    group_by(data, subject_id) %>%
      mutate(across(where(is.numeric), func)) %>%
      ungroup()

  }else{
    na_rows <- apply(select(data, where(is.numeric)), 1, function(x) any(is.na(x)))
    filter(data, na_rows)
  }
}

replace_na_mean <- function(x){
  x[is.na(x)] <- mean(x, na.rm = TRUE)
  x
}

replace_all_mean <- function(x){
  mean(x, na.rm = TRUE)
}

replace_all_first <- function(x){
  x <- x[!is.na(x)]
  x[1]
}

custom_scale <- function(x){
  scale(x)[,1]
}

col_rename <- function(x){
  paste0(x, "_z")
}

is.mnumber <- function(x){
  !all(x %in% c(1,0))
}

site_name2number <- function(x){
  sapply(x, switch,
         ousAvanto    = 11,
         ousSkyra     = 12,
         ousPrisma    = 13,
         ntnuAvanto   = 20,
         curatoAvanto = 21
  )
}

## quiets concerns of R CMD check
if(getRversion() >= "2.15.1"){
  globalVariables(c("where", ".N", "project_id", "wave_code",
                  "subject_id", "fsid", "fsid-base"))
}
