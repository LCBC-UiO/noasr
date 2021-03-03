base_size_text <- 10

#' ggplot theme for lcbc
#'
#' @param base_size text size
#' @export
#' @importFrom ggplot2 theme_grey theme '%+replace%'
theme_lcbc_grid <- function (base_size = base_size_text) {
  theme_grey(base_size = base_size,# base_family = lcbc_font()
             ) %+replace%
    theme(
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
#' @importFrom ggplot2 theme element_blank '%+replace%'
theme_lcbc <- function (base_size = base_size_text) {
  theme_lcbc_grid() %+replace%
    theme(
      panel.grid = element_blank(),
      panel.grid.minor = element_blank()
    )
}

#' ggplot dark theme for lcbc
#'
#' @param base_size text size
#' @export
#' @importFrom ggplot2 theme '%+replace%'
theme_lcbc_dark_grid <- function (base_size = base_size_text) {
  theme_lcbc_grid() %+replace%
    theme(
      line = lcbc_line(lcbc_cols("dark grey")),
      text = lcbc_text(lcbc_cols("white")),
      panel.background  = lcbc_rect(lcbc_cols("black")),
      panel.grid = lcbc_line(lcbc_cols("dark grey")),
      panel.grid.minor = lcbc_line(lcbc_cols("dark grey"),
                                   linetype = "longdash"),
      plot.background = lcbc_rect(lcbc_cols("black")),
      plot.subtitle = lcbc_text(lcbc_cols("light grey"),
                                face = "italic", vjust = 1, hjust = 0),
      plot.caption = lcbc_text(lcbc_cols("light grey"),
                               face = "italic", hjust=1.05, size=15),
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
#' @importFrom ggplot2 theme element_blank '%+replace%'
theme_lcbc_void <- function (base_size = base_size_text) {
  theme_lcbc_grid() %+replace%
    theme(
      panel.background  =  element_blank(),
      panel.grid = element_blank(),
      panel.grid.minor = element_blank(),
      plot.background = element_blank(),
      axis.line = element_blank(),
      axis.text = element_blank(),
      axis.title =  element_blank(),
      axis.ticks =  element_blank()
    )
}

#' ggplot minimal theme for lcbc
#'
#' @param base_size text size
#' @export
#' @importFrom ggplot2 theme element_blank '%+replace%'
theme_lcbc_dark <- function (base_size = base_size_text) {
  theme_lcbc_dark_grid() %+replace%
    theme(
      panel.grid = element_blank(),
      panel.grid.minor = element_blank()
    )
}

#' ggplot minimal theme for lcbc
#'
#' @param base_size text size
#' @export
#' @importFrom ggplot2 theme element_blank '%+replace%'
theme_lcbc_dark_void <- function (base_size = base_size_text) {
  theme_lcbc_dark() %+replace%
    theme(
      panel.grid = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank()
    )
}

#' @importFrom ggplot2 element_text margin
lcbc_text <- function(colour,
                      size = base_size_text,
                      lineheight = 0.9,
                      hjust = 0.5,
                      vjust = 0.5,
                      angle = 0,
                      face = "plain"){

  element_text(#family = lcbc_font(),
               face = face,
               colour =  colour,
               hjust = hjust,
               vjust = vjust,
               angle = angle,
               size = size,
               lineheight = lineheight,
               margin = margin(),
               debug = FALSE)
}

#' @importFrom ggplot2 element_line
lcbc_line <- function(colour, linetype = "solid"){
  element_line(colour = colour,
               size = .6,
               linetype =  linetype,
               lineend = "round")
}

#' @importFrom ggplot2 element_rect
lcbc_rect <- function(fill = "transparent", colour = NA){
  element_rect(fill = fill, colour = colour)
}
