DownloadOptionsTable = c(
  "Semicolon separated" = "csv2" ,
  "Tab separated" = "tsv" ,
  "Pipe separated" = "psv" ,
  "R" = "rda",
  "Excel (>=2007)" = "xlsx",
  "JSON" = "json",
  "SAS" = "sas7bdat",
  "Stata" = "dta",
  "MATLAB" = "mat",
  "OpenDocument Spreadsheet" = "ods",
  "Feather R/Python interchange" = "feather",
  "Fixed-width" = "fwf"
)
save(DownloadOptionsTable, file="inst/shiny/LCBCshiny/globalVars/DownloadOptionsTable.RData")

WidenChoices = c(
  "Keep as is (long)"  = "none",
  "Widen by site" = "Site_Name",
  "Widen by site then wave" = "s2w",
  "Widen by wave" = "Project_Wave"
)
save(WidenChoices, file="inst/shiny/LCBCshiny/globalVars/WidenChoices.RData")


# Requires being on the lagringshotell to run
ConversionTab = rio::import("../../../../Documentation/Project_Harmonization.xlsx") %>%
  select(1:5)
save(ConversionTab, file="inst/shiny/LCBCshiny/globalVars/ConversionTab.RData")
