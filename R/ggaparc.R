#' Plot Freesurfer aparc areas
#'
#' \code{ggaparc} plots and returns a ggplot object of plotted
#' aparc areas.
#' @author Athanasia Mowinckel and Didac Pineiro
#'
#' @param data A data.frame to use for plot aesthetics. Must include a
#' column called "area" corresponding to aparc areas.
#'
#' @param plot.areas Character vector, plots only areas specified in the vector.
#' @param mapping ggplot2 aethetics (cannot include x and y aethetics)
#' @param na.fill string or HEX code for the fill of the area without values
#' @param colour string or HEX code for the colour of the outlines of each area
#' @param size Numeric, size of the line outlining each area
#' @param show.legend logical, toggle on or off legend.
#' @param ...
#'
#' @return a ggplot object
#'
#' @importFrom ggplot2 ggplot aes geom_polygon coord_fixed
#'
#' @examples
#' ggaparc()
#' ggaparc(na.fill = "transparent",mapping=aes(fill=area))
#' ggaparc(colour="black", size=.7, mapping=aes(fill=area)) + theme_void()

#' @seealso [ggplot()], [aes()], [geom_polygon()], [coord_fixed()] from the ggplot2 package
#'
#' @export
ggaparc = function(data = NULL, plot.areas=NULL, mapping = NULL,
                   colour="white", size=.3, show.legend = NA,
                   na.fill="grey",...){

  fileLoc = paste0(system.file("data","geobrain", package = "MOAS"),"/geobrain.RData")
  load(fileLoc)

  tt=c("bankssts","caudalanteriorcingulate","caudalmiddlefrontal","cuneus","entorhinal","fusiform","inferiorparietal",
    "inferiortemporal","isthmuscingulate","lateraloccipital","lateralorbitofrontal","lingual","medialorbitofrontal","middletemporal",
    "parahippocampal","paracentral","parsopercularis","parsorbitalis","parstriangularis","pericalcarine","postcentral","posteriorcingulate",
    "precentral","precuneus","rostralanteriorcingulate","rostralmiddlefrontal","superiorfrontal","superiorparietal","superiortemporal",
    "supramarginal","frontalpole","temporalpole","transversetemporal","insula","WhiteSurfArea")

  if(!is.null(plot.areas)){
    if(any(!plot.areas %in% geobrain$area)){
      stop(paste("There is no", plot.areas, "in aparc data. Check spelling. Options are:",paste0(geobrain$area %>% unique,collapse=", ")))
    }
    geobrain = geobrain %>% filter(area %in% plot.areas)
  }

  geoData = if(is.null(data)){
    geobrain
  }else{
    geobrain %>% dplyr::left_join(data, by="area") %>% na.omit()
  }

  ggplot2::ggplot(data = geobrain, ggplot2::aes(x=long, y=lat, group=area)) +
    ggplot2::geom_polygon(
      size=size,
      colour=colour,
      fill=na.fill) +
    ggplot2::geom_polygon(
      data=geoData,
      mapping=mapping,
      size=size,
      colour=colour,
      show.legend = show.legend) +
    ggplot2::coord_fixed()
}





