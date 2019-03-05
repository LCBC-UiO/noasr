# This file compiles all the necessary packages in R to run and develop this shiny App.
# If none of these packages are installed, this script will take some time. 

# Once this is complete, open the app.R file in rstudio. In the "script" window of Rstudio,
# the top right corner will now have a button labeled "Run app", push this to start the
# shiny app. You may choose to run it in "External" (browser), "Window", or "Viewer pane". 
# I recomment launching in browser. 

# Running this outside TSD, will prompt you to upload the necessary file to work with 
# (MOAS.RData). If you are on a mac, and have mounted the LH exactly as described in the 
# 'Mac acces to LH' document, the file should be automatically loaded.

LIBS=c("rio","tidyverse","DT","plotly","png","svglite")
REPO="https://cran.rstudio.com/"

#installs only those that are not already installed
sapply(LIBS[!LIBS %in% installed.packages()], install.packages, repos=REPO) 

#install package "rio" with all formats
if(!"readODS" %in% installed.packages()){
  library(rio); 
  install_formats(repos=REPO)
}

## If developing the app, launch these too.
## If run via command line do: 'Rscript --vanilla INSTALL.R dev
## Else run the lines within the if manually.
if (length(args)!=0) {
  LIBS= c("shiny","devtools","rsconnect","packrat","RCurl", "rstudioapi", "RJSONIO", "evaluate")
  sapply(LIBS[!LIBS %in% installed.packages()], install.packages, repos=REPO)
}
