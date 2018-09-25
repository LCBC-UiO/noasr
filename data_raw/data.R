Projects = data.frame(
  Project_Number = c(10:17,90),
  Project_Name = c("NDev","MemP","NCP","MoBa","Loci","MemC","ACon","S2C",
                   "Novel_biomarkers"),
  Project_Experimental = c(NA,NA, "Memory training",NA,"Memory training",
                           NA, NA, "Memory training (VR)",NA),
  stringsAsFactors = F
)
save(Projects, file="data/Projects.RData")

Sites = data.frame(
  Site_Number = c(11,12,13,20,21),
  Site_Name = c("ousAvanto","ousSkyra","ousPrisma","ntnuAvanto","curatoAvanto"),
  Site_Tesla = c(1.5,3,3,1.5,1.5),
  stringsAsFactors = F
)
save(Sites, file="data/Sites.RData")

baseCols = c("Folder","CrossProject_ID","Sex","Birth_Date",
             "Subject_Timepoint","Project_Wave","Project_Name",
             "SIte_Name","Site_Number", "Site_Tesla","N_Scans",
             "Age","Interval_FirstVisit")
save(baseCols,file="data/baseCols.RData")

# Only works on lagringshotel
variables = rio::import("../../../../Documentation/Project_Harmonization.xlsx") %>%
  select(1:5)
save(variables, file="data/variables.RData")
