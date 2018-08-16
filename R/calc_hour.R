#' Calculates time in decimal hours from HH:MM format.
#'
#' \code{calc_hour} calculates time in decimal hours from HH:MM format.
#'
#' @param data String or time data in 'HH:MM' format
#'
#' @return a numeric vector of decimal hours

#' @examples
#'  calc_hour(data)
#'
#' @importFrom dplyr mutate
#' @importFrom magrittr "%>%"
#'
#' @export
calc_hour = function(data) {

    tmp = strsplit(as.character(data), ":")

    DATA2 = data.frame(matrix(nrow = length(tmp), ncol = 3))

    for (i in 1:length(tmp)) {
        n = (data[i] %>% MOAS::count_chars(":", .)) + 1

        if (n < ncol(DATA2))
            tmp[[i]] = c(tmp[[i]], "")
        DATA2[i, ] = tmp[[i]]
    }
    DATA2 = as.data.frame(apply(DATA2, 2, as.numeric))
    names(DATA2) = c("H", "M", "S")

    DATA2 = DATA2 %>% dplyr::mutate(M = as.numeric(M)/60, S = S/60/60)
    DATA2$time = rowSums(DATA2, na.rm = T)


    return(DATA2$time)
}
