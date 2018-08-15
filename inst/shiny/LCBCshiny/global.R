library(shinycssloaders); library(plotly); library(shiny);
library(tidyverse); library(rio); library(svglite); library(DT); library(RColorBrewer)

#Source all the necessary functions
lapply(list.files("subscripts/", pattern=".R$",full.names = T), source)


ConversionTab = import("Documentation/Project_Harmonization.xlsx") %>%
   select(1:5)


# Get some global variables
load("globalVars/Vars.RData")


