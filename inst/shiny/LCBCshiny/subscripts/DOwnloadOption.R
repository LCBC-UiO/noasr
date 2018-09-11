DownloadOption = function(option){
  switch(option,
         csv   = return(list(SEP=",", EXT="csv")),
         csv2  = return(list(SEP=";", EXT="csv")),
         dat   = return(list(SEP="\t", EXT="dat")),
         txt   = return(list(SEP=" ", EXT="txt")),
         xlsx  = return(list(EXT="xlsx")),
         sav   = return(list(EXT="sav"))
  )
}
