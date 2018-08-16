#'

ggaparc = function(){
  ### Ids corresponding to parcelations in this image
  GeoAparc = cbind(
    id = c(0, 1, 4,7, 8,9,15,17,20,21,22,23,28,32,34,39,41,49),
    area = c("parstriangularis","smn","motor","ipl","stg","mtg","loc","rmf",
             "lvmpfc","bankssts","insula","spc","itg","visual","cmf","parsorbitalis",
             "supfrontal","transverse")
  ) %>% as.data.frame(stringsAsFactors=F)


  ##### main routine #####
  #Open geo object (shape file)
  fileLoc = system.file("data","geobrain", package = "MOAS")
  geobrainOGR <- rgdal::readOGR(dsn = "fileLoc", layer = "geobrain")

  #transform to data.frame
  geobrain <- ggplot2::fortify(geobrainOGR, region="GEO_ID")
  #utils::head(geobrain)


  ## RAW PLOT commentary:
  # as the raw plot is far from perfect there are several polygons that do not correspond
  # to any aparc. Yet due to the way the image is transformed most of the lower indices should correspond
  # to real regions

  #### some cleaning can be done #####
  # to remove unlikely polygons: geobrain2 <- geobrain[as.numeric(geobrain$id) < 50,]
  # to check individual regions: geobrain2 <- geobrain[as.numeric(geobrain$id) == 12]
  # to retain more complex polygons: sort(table(as.numeric(geobrain$id)))

  # here the following combination worked well - though some manual labour
#
#   geobrain2 <- geobrain[as.numeric(geobrain$id) < 50,]
#   sort(table(as.numeric(geobrain$id)))

  geobrain2 <- geobrain %>%
    dplyr::filter(id %in% GeoAparc$id)

  ggplot2::ggplot(data = geobrain2) +
    ggplot2::geom_polygon(
      ggplot2::aes(x = long,
                   y = lat,
                   group = as.factor(id),
                   fill=as.numeric(id)), color = "white", size = .5) +
    coord_fixed() +
    ggplot2::theme_classic()  +
    ggplot2::theme(legend.position = "none")

}





