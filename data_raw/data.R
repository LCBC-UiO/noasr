Projects = data.frame(
  Project_Number = 10:17,
  Project_Name = c("NDev","MemP","NCP","MoBa","Loci","MemC","ACon","S2C"),
  stringsAsFactors = F
)

Sites = data.frame(
  Site_Number = c(11,12,13,20,21),
  Site_Name = c("ousAvanto","ousSkyra","ousPrisma","ntnuAvanto","curatoAvanto"),
  Site_Tesla = c(1.5,3,3,1.5,1.5),
  stringsAsFactors = F
)

baseCols = c("Folder","CrossProject_ID","Sex","Birth_Date",
             "Subject_Timepoint","Project_Wave","Project_Name",
             "SIte_Name","Site_Number", "Site_Tesla","N_Scans",
             "Age","Interval_FirstVisit")

save(baseCols,Sites,Projects,
     file="data/data.RData")
