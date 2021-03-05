
#' Colour scale constructor for lcbc colours
#'
#' @param palette Character name of palette in lcbc_palettes
#' @param discrete Boolean indicating whether colour aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to \code{\link{ggplot2}{discrete_scale()}} or
#'            \code{\link{ggplot2}{scale_colour_gradientn()}}, used respectively when discrete is TRUE or FALSE
#' @importFrom ggplot2 scale_colour_manual discrete_scale scale_colour_gradientn
#' @importFrom ggplot2 scale_fill_manual scale_fill_gradientn
#' @export
scale_colour_lcbc <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- lcbc_pal(palette = palette, reverse = reverse)

  if (discrete) {
    ggplot2::discrete_scale("colour", paste0("lcbc_", palette), palette = pal, ...)
  } else {
    ggplot2::scale_colour_gradientn(colours = pal(256), ...)
  }
}

#' @export
#' @rdname scale_colour_lcbc
scale_color_lcbc <- scale_colour_lcbc

#' Fill scale constructor for lcbc colours
#'
#' @inheritParams scale_colour_lcbc
#' @export
scale_fill_lcbc <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- lcbc_pal(palette = palette, reverse = reverse)

  if (discrete) {
    ggplot2::discrete_scale("fill", paste0("lcbc_", palette), palette = pal, ...)
  } else {
    ggplot2::scale_fill_gradientn(colours = pal(256), ...)
  }
}


# projects ----

#' Colour scale constructor for proj colours
#'
#' @param palette Character name of palette in project_palettes
#' @param discrete Boolean indicating whether colour aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to \code{\link{ggplot2}{discrete_scale()}} or
#'            \code{\link{ggplot2}{scale_colour_gradientn()}}, used respectively when discrete is TRUE or FALSE
#' @importFrom ggplot2 scale_colour_manual discrete_scale scale_colour_gradientn
#' @importFrom ggplot2 scale_fill_manual scale_fill_gradientn
#' @export
scale_colour_proj <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- project_pal(palette = palette, reverse = reverse)

  if(length(palette) == 1 && palette == "named"){
    ggplot2::scale_colour_manual(values = pal, ...)
  }else if (discrete) {
    ggplot2::discrete_scale("colour", paste0("proj_", palette), palette = pal, ...)
  } else {
    ggplot2::scale_colour_gradientn(colours = pal(256), ...)
  }
}

#' @export
#' @rdname scale_colour_proj
scale_color_proj <- scale_colour_proj


#' Fill scale constructor for proj colours
#'
#' @inheritParams scale_colour_proj
#' @export
scale_fill_proj <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- project_pal(palette = palette, reverse = reverse)

  if(length(palette) == 1 && palette == "named"){
    ggplot2::scale_colour_manual(values = pal, ...)
  }else if (discrete) {
    ggplot2::discrete_scale("fill", paste0("proj_", palette), palette = pal, ...)
  } else {
    ggplot2::scale_fill_gradientn(colours = pal(256), ...)
  }
}

## quiets concerns of R CMD check
if(getRversion() >= "2.15.1"){
  utils::globalVariables(c("palette",
                           "discrete",
                           "reverse",
                           "%+replace%"))
}

