library(shinycssloaders); library(plotly); library(shiny); 
library(tidyverse); library(rio); library(svglite); library(DT); library(RColorBrewer)

#Source all the necessary functions
lapply(list.files("subscripts/", pattern=".R$",full.names = T), source)

OS = ifelse(grepl("mac",sessionInfo()$running), "Mac",
            
            ifelse(grepl("Windows",sessionInfo()$running), "Win", "Lin"))

# # Get the data
# if(OS=="Win"){ #Needs to check windows first, as will fail in second check if OS is Win
#   
# }else if( ifelse(OS=="Lin",  grepl("tsd.usit.no", system("uname -n", intern = T)) , FALSE)){
#   inDATA=get_file("MOAS.RData")
#   ConversionTab = import("Documentation/Project_Harmonization.xlsx") %>% 
#     select(1:5) 
#   #Show unsanitized logs if at TSD (were safe here..)
#   options(shiny.sanitize.errors = FALSE)
# }else if (OS=="Mac" & "Projects" %in% system("ls ~/LCBC", intern = T)){
#   inDATA=get_file("~/LCBC/Projects/Cross_projects/MOAS/Data/MOAS.RData")
#   ConversionTab = import("~/LCBC/Projects/Cross_projects/MOAS/Documentation/Project_Harmonization.xlsx") %>% 
#     select(1:5) 
# }else{
  ConversionTab = import("Documentation/Project_Harmonization.xlsx") %>% 
    select(1:5) 
# }

# Get some global variables
load("globalVars/Vars.RData")


