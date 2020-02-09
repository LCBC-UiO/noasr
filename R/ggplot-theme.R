base_size_text <- 10

#' ggplot theme for lcbc
#'
#' @param base_size text size
#' @export
theme_lcbc <- function (base_size = base_size_text) {
  ggplot2::theme_grey(base_size = base_size, base_family = "Avenir") %+replace%
    ggplot2::theme(
      line = lcbc_line(lcbc_cols("light grey")),
      text = lcbc_text(lcbc_cols("black")),
      panel.background  = lcbc_rect(),
      panel.grid = lcbc_line(lcbc_cols("white")),
      panel.grid.minor = lcbc_line(lcbc_cols("white"),
                                   linetype = "longdash"),
      plot.background = lcbc_rect(),
      plot.subtitle = lcbc_text(lcbc_cols("dark grey"),
                                face = "italic", vjust = 1, hjust = 0),
      axis.line = lcbc_line(lcbc_cols("light grey")),
      legend.background = lcbc_rect(),
      legend.key = lcbc_rect()
    )
}

#' ggplot minimal theme for lcbc
#'
#' @param base_size text size
#' @export
theme_lcbc_minimal <- function (base_size = base_size_text) {
  theme_lcbc() %+replace%
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
    )
}

#' ggplot dark theme for lcbc
#'
#' @param base_size text size
#' @export
theme_lcbc_dark <- function (base_size = base_size_text) {
  theme_lcbc() %+replace%
    ggplot2::theme(
      line = lcbc_line(lcbc_cols("dark grey")),
      text = lcbc_text(lcbc_cols("white")),
      panel.background  = lcbc_rect(lcbc_cols("black")),
      panel.grid = lcbc_line(lcbc_cols("dark grey")),
      panel.grid.minor = lcbc_line(lcbc_cols("dark grey"),
                                   linetype = "longdash"),
      plot.background = lcbc_rect(lcbc_cols("black")),
      plot.subtitle = lcbc_text(lcbc_cols("light grey"),
                                face = "italic", vjust = 1, hjust = 0),
      axis.line = lcbc_line(lcbc_cols("white")),
      axis.text = lcbc_text(lcbc_cols("light grey")),
      legend.background = lcbc_rect(),
      legend.key = lcbc_rect()
    )
}

#' ggplot minimal theme for lcbc
#'
#' @param base_size text size
#' @export
theme_lcbc_void <- function (base_size = base_size_text) {
  theme_lcbc() %+replace%
    ggplot2::theme(
      panel.background  =  ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      plot.background = ggplot2::element_blank(),
      axis.line = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      axis.title =  ggplot2::element_blank(),
      axis.ticks =  ggplot2::element_blank()
    )
}

#' ggplot minimal theme for lcbc
#'
#' @param base_size text size
#' @export
theme_lcbc_dark_minimal <- function (base_size = base_size_text) {
  theme_lcbc_dark() %+replace%
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
    )
}

#' ggplot minimal theme for lcbc
#'
#' @param base_size text size
#' @export
theme_lcbc_dark_void <- function (base_size = base_size_text) {
  theme_lcbc_dark() %+replace%
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.line = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank()
    )
}

lcbc_text <- function(colour,
                      size = base_size_text,
                      lineheight = 0.9,
                      hjust = 0.5,
                      vjust = 0.5,
                      angle = 0,
                      face = "plain"){

  ggplot2::element_text(family = "Avenir",
                        face = face,
                        colour =  colour,
                        hjust = hjust,
                        vjust = vjust,
                        angle = angle,
                        size = size,
                        lineheight = lineheight,
                        margin = ggplot2::margin(),
                        debug = FALSE)
}

lcbc_line <- function(colour, linetype = "solid"){
  ggplot2::element_line(colour = colour,
                        size = .6,
                        linetype =  linetype,
                        lineend = "round")
}

lcbc_rect <- function(fill = "transparent", colour = NA){
  ggplot2::element_rect(fill = fill, colour = colour)
}
