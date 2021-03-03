project_colors <- c(
  `NDev`  = "#c5da84",
  `MemP`  = "#f94333",
  `NCP`   = "#3b567d",
  `MoBa`  = "#f6ae33",
  `Loci`   = "#249596",
  `MemC`   = "#ae2e23",
  `ACon`   = "#fa685b",
  `S2C`    = "#155858"
)

#' Function to extract project colors as hex codes
#'
#' @param ... Character names of project_colors
#'
project_cols <- function(...) {
  cols <- c(...)

  if (is.null(cols))
    return (project_colors)

  cols <- match.arg(cols,
                    names(project_colors),
                    several.ok = TRUE)

  project_colors[cols]
}

project_palettes <- list(
  `main`  = project_cols(names(project_colors)),

  `training`  = project_cols("Loci", "NCP" , "S2C"),

  `memp`   = project_cols("MemP", "MemC" ,"ACon"),

  `named` = project_colors
)

#' Return function to interpolate a project color palette
#'
#' @param palette Character name of palette in project_palettes
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments to pass to colorRampPalette()
#'
project_pal <- function(palette = "main", reverse = FALSE, ...) {
  palette <- match.arg(palette, names(project_palettes))

  pal <- project_palettes[[palette]]

  if (reverse) pal <- rev(pal)

  if(palette == "named"){
    pal
  }else{
    grDevices::colorRampPalette(pal, ...)
  }
}
