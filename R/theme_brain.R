#' ggbrain plot theme
#'
#' @description a set of themes created for the ggbrain plots. Use ggplot2::theme() to tweak.
#' @author Athanasia Mowinckel
#'
#' @details
#' \describe{
#'
#' \item{`theme_brain`}{
#' Default theme for ggbrain. Transparent background, no axis lines, and no grid.}
#'
#' \item{`theme_darkbrain`}{
#' Dark equivalent to theme_brain, with black background, and light text.}
#'
#' @examples


#' @seealso [ggplot()], [aes()], [geom_polygon()], [coord_fixed()] from the ggplot2 package
#'
#' @export
#' @rdname ggtheme
theme_brain = function(){

  theme(
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    panel.background = element_blank(),
    plot.background = element_blank()
  )
}

#' @export
#' @rdname ggtheme
theme_darkbrain = function(){

  theme(
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    panel.background = element_blank(),
    plot.background = element_rect(fill="black"),
    text = element_text(colour="lightgrey"),
    axis.text = element_text(colour="lightgrey")
  )

}
