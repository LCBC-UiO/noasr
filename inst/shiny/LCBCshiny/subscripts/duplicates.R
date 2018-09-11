duplicates = function(DATA, COLUMN){
  tmp = DATA %>% select(CrossProject_ID, COLUMN) %>%
    filter(which(duplicated()))
  group_by(CrossProject_ID) %>% duplicated_(COLUMN)
  
}