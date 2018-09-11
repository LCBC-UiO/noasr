library(MOAS); library(shinycssloaders); library(plotly); library(shiny);
library(Cairo); library(rio); library(svglite); library(DT); library(RColorBrewer)
library(tidyverse);

#Source all the necessary functions
lapply(list.files("subscripts/", pattern=".R$",full.names = T), source)


ConversionTab = rio::import("Documentation/Project_Harmonization.xlsx") %>%
   dplyr::select(1:5)


# Get some global variables
load("globalVars/Vars.RData")


