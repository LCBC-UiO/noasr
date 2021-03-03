
#' LCBC logo graphics object
#'
#' @param type type of logo to get
#' @param alpha opacity of logo
#' @noRd
#' @return graphics object (grob)
#' @importFrom grDevices rgb
#' @importFrom png readPNG
#' @importFrom grid rasterGrob
lcbc_logo_grob <- function(type = "main", alpha = 0.4){

  logo_dir <- system.file("logos", package = "noasr")


  logos <- list.files(logo_dir)
  logos <- gsub("LCBC_|\\.png", "", logos)

  type <- match.arg(type, logos)

  img <- readPNG(list.files(logo_dir, pattern = type, full.names = TRUE),
                 FALSE, FALSE)

  img <- matrix(rgb(img[,,1], img[,,2], img[,,3],
                    img[,,4] * alpha), nrow=dim(img)[1])

  rasterGrob(img, interpolate=TRUE)
}

#' Add LCBC logo to plot
#'
#' in a ggplot, adding the logo should be added
#' early in the plot building
#'
#' @param type type of logo to get
#' @param alpha opacity of logo
#' @param ... arguments to \link{ggplot2}{\code{annotation_custom}}
#'
#' @export
#' @importFrom ggplot2 annotation_custom
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(x = wt, y = disp, colour = cyl)) +
#'   geom_point() +
#'   add_lcbc_logo()
add_lcbc_logo <- function(type = "main", alpha = .4,
                          ...){
  g <- lcbc_logo_grob(type, alpha = alpha)

  annotation_custom(grob = g, ...)
}

