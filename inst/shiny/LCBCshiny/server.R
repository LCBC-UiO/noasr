#library(plotly); library(shiny); library(tidyverse); library(rio); library(svglite); library(DT)

shiny::shinyServer(function(input, output, session) source("appFiles/app.R", local=TRUE))


