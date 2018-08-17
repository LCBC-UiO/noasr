#'@export

ggaparc = function(data,...){

  fileLoc = paste0(system.file("data","geobrain", package = "MOAS"),"geobrain.RData")
  load(fileLoc)

  ggplot2::ggplot(data = geobrain2) +
    ggplot2::geom_polygon(
      ggplot2::aes(x = long,
                   y = lat,
                   group = as.factor(area),
                   fill=area), color = "white", size = .5) +
    coord_fixed()

}





