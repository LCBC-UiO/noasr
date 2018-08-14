Factor_times = function(TimeVector){
  
  DATA2 = as.data.frame(hms(TimeVector)); names(DATA2) = "Time"
  
  DATA2$TimeOfDay = ifelse(DATA2$Time@hour >=5 & DATA2$Time@hour < 12, "Morning", 
                           ifelse(DATA2$Time@hour >= 12 & DATA2$Time@hour < 17, "Afternoon", 
                                  ifelse( DATA2$Time@hour >= 17 & DATA2$Time@hour < 21, "Evening","Night")))
  
  return(DATA2$TimeOfDay)
}