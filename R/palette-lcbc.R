lcbc_colors <- c(
  `light green`  = "#c5da84",
  `dark green`   = "#94c11f",

  `light grey`   = "#b2b2b2",
  `dark grey`    = "#626262",

  `light blue`   = "#66bfdd",
  `dark blue`    = "#009fe3",

  `white`    = "#e2e2e2",
  `black`    = "#111111"
)

#' Function to extract lcbc colors as hex codes
#'
#' @param ... Character names of lcbc_colors
#' @export
lcbc_cols <- function(...) {
  cols <- c(...)

  if (is.null(cols))
    return (lcbc_colors)

  cols <- match.arg(cols,
            names(lcbc_colors),
            several.ok = TRUE)

  lcbc_colors[cols]
}

lcbc_palettes <- list(
  `main`  = lcbc_cols("dark blue", "light blue","light green","dark green"),

  `light`  = lcbc_cols("light blue", "white" ,"light green"),

  `dark`   = lcbc_cols("dark blue", "black" ,"dark green"),

  `blue` = lcbc_cols("light blue", "dark blue"),

  `green` = lcbc_cols("light green", "dark green"),

  `grey`  = lcbc_cols("white", "light grey", "dark grey", "black")
)

#' Return function to interpolate a lcbc color palette
#'
#' @param palette Character name of palette in lcbc_palettes
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments to pass to colorRampPalette()
#' @export
lcbc_pal <- function(palette = "main", reverse = FALSE, ...) {
  palette <- match.arg(palette, names(lcbc_palettes))

  pal <- lcbc_palettes[[palette]]

  if (reverse) pal <- rev(pal)

  grDevices::colorRampPalette(pal, ...)
}
