
#' Colour scale constructor for lcbc colors
#'
#' @param palette Character name of palette in lcbc_palettes
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_color_gradientn(), used respectively when discrete is TRUE or FALSE
#' @export
scale_colour_lcbc <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- lcbc_pal(palette = palette, reverse = reverse)

  if (discrete) {
    ggplot2::discrete_scale("color", paste0("lcbc_", palette), palette = pal, ...)
  } else {
    ggplot2::scale_colour_gradientn(colours = pal(256), ...)
  }
}

#' Color scale constructor for lcbc colors
#' @param ... arguments to \code{\link{scale_colour_lcbc}}
#' @export
scale_color_lcbc <- function(...) {
  scale_colour_lcbc(...)
}

#' Fill scale constructor for lcbc colors
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

#' Colour scale constructor for proj colors
#'
#' @param palette Character name of palette in project_palettes
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_color_gradientn(), used respectively when discrete is TRUE or FALSE
#' @export
scale_colour_proj <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- project_pal(palette = palette, reverse = reverse)

  if(palette == "named"){
    ggplot2::scale_color_manual(values = pal, ...)
  }else if (discrete) {
    ggplot2::discrete_scale("color", paste0("proj_", palette), palette = pal, ...)
  } else {
    ggplot2::scale_colour_gradientn(colours = pal(256), ...)
  }
}

#' Color scale constructor for proj colors
#' @param ... arguments to \code{\link{scale_colour_proj}}
#' @export
scale_color_proj <- function(...) {
  scale_colour_proj(...)
}

#' Fill scale constructor for proj colors
#'
#' @inheritParams scale_colour_proj
#' @export
scale_fill_proj <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- project_pal(palette = palette, reverse = reverse)

  if(palette == "named"){
    ggplot2::scale_color_manual(values = pal, ...)
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

