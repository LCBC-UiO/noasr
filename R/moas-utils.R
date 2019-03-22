get_moas <- function(MOAS){
  if(is.character(MOAS)){
    nn <- load(MOAS)
    get(nn)
  }else{
    MOAS
  }
}

