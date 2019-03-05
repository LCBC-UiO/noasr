library(MOAS); library(shinycssloaders); library(plotly); library(shiny);
library(Cairo); library(rio); library(svglite); library(DT); library(RColorBrewer)
library(tidyverse);

#Source all the necessary functions
lapply(list.files("subscripts/", pattern=".R$",full.names = T), source)

lapply(list.files("globalVars/", pattern=".RData$",full.names = T), load, envir=.GlobalEnv)

ConversionTab = rio::import("Documentation/Project_Harmonization.xlsx") %>%
   dplyr::select(1:5)
