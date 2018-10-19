#' Cleans data from SPSS into a data.frame
#'
#' \code{clean_DF} cleans SPSS imported data of it's many
#' labels and attributed, for easier reading and handling in R
#'
#' @param data a data.frame
#'
#' @return a cleaned data.frame

#' @examples
#' \dontrun{
#' clean_DF(data)
#' }
#' @importFrom magrittr "%>%"
#' @importFrom dplyr select one_of
#' @importFrom utils type.convert
#'
#' @export
#'
clean_DF = function(data) {

    data = apply(data, 2, function(x) as.character(x)) %>% as.data.frame(stringsAsFactors = F)

    # Grab any column with an alhpabetic character or what is date
    COLS = apply(data, 2, function(x) grepl("[a-zA-Z]|\\:", x) %>% any()) %>%
      unlist() %>% unname() + grepl("date|dato", names(data), ignore.case = T)
    COLS = ifelse(COLS == 1, TRUE, FALSE)

    OUT = data %>% dplyr::select(dplyr::one_of(names(data)[COLS]))

    # Remove data classes and labels to enable easier handling.
    data %>%
      apply(2, function(x) utils::type.convert(as.character(x))) %>%
      as.data.frame() %>%
      dplyr::select(-dplyr::one_of(names(OUT))) %>%
        cbind.data.frame(OUT)
}
