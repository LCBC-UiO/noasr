#' Utility function to count the number of occurances
#' of a specific character in a string
#'
#' \code{count_chars} counts the number of times a
#' character occurs in a string
#'
#' @param char Character to count.
#' @param s string vector
#'
#' @return vector of integers

#' @examples
#' count_chars("h", "How many times is 'h' in this string?")
#'
#'
#' @export
#'

count_chars = function(char, s) {
  s2 <- gsub(char,"",s)
  return (nchar(s) - nchar(s2))
}
