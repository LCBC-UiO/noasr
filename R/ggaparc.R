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
#' @param hemisphere String to choose hemisphere to plot. Any of c("lh","rh")[default].
#' @param na.fill String or HEX code for the fill of the area without values
#' @param colour String or HEX code for the colour of the outlines of each area
#' @param size Numeric, size of the line outlining each area
#' @param show.legend logical, toggle on or off legend.
#' @param ...
#'
#' @return a ggplot object
#'
#' @importFrom dplyr filter left_join
#' @importFrom ggplot2 ggplot aes geom_polygon coord_fixed
#' @importFrom stats na.omit
#' @importFrom tidyr separate
#'
#' @examples
#' ggaparc()
#' ggaparc(na.fill = "transparent",mapping=aes(fill=area))
#' ggaparc(colour="black", size=.7, mapping=aes(fill=area)) + theme_void()

#' @seealso [ggplot()], [aes()], [geom_polygon()], [coord_fixed()] from the ggplot2 package
#'
#' @export
ggaparc = function(data = NULL, plot.areas=NULL,
                   position="dispersed",
                   hemisphere = c("rh","lh"),
                   mapping = NULL, alpha=NA,
                   colour="white", size=.3, show.legend = NA,
                   na.fill="grey",...){

  fileLoc = system.file("data","geobrain", package = "MOAS")
  nn = load(paste0(fileLoc,"/geobrain.bilateral.Rda"))
  geobrain = get(nn) %>%
    tidyr::separate(aparc, c("hemi","area"),remove = F,convert = T) %>%
    dplyr::filter(hemi %in% hemisphere) %>%
    dplyr::mutate(aparc=ifelse(grepl("medialwall",aparc),NA,aparc),
                  area=ifelse(grepl("medialwall",area),NA,area))

  if(position=="stacked"){
    stack = geobrain %>%
      filter(hemi %in% "rh") %>%
      summarise(ymax=max(lat),xmax=max(long)) %>%
      round(0)+1

    geobrain = geobrain %>%
      mutate(lat=ifelse(hemi %in% "lh",
                        lat + stack$ymax, lat),
             long=ifelse(hemi %in% "lh",
                         long - stack$xmax, long)
      )
  }


  if(!is.null(plot.areas)){
    if(any(!plot.areas %in% geobrain$area)){
      stop(paste("There is no", plot.areas, "in aparc data. Check spelling. Options are:",paste0(geobrain$area %>% unique,collapse=", ")))
    }
    geobrain = geobrain %>% dplyr::filter(area %in% plot.areas)
  }

  geoData = if(!is.null(data)){
    geobrain %>%
      dplyr::left_join(data)
  }else{
    geobrain
  } %>%
    stats::na.omit()


  gg = ggplot2::ggplot(data = geobrain, ggplot2::aes(x=long, y=lat, group=id)) +
    ggplot2::geom_polygon(
      size=size,
      colour=colour,
      fill=na.fill,
      alpha=alpha)

  if(!is.null(mapping)){
    gg = gg +
      ggplot2::geom_polygon(
        data=geoData,
        mapping=mapping,
        size=size,
        colour=colour,
        show.legend = show.legend)
  }

  gg + ggplot2::coord_fixed()

}





