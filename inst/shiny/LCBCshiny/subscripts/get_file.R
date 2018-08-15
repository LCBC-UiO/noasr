get_file = function(PATH){
  # load the file into new environment and get it from there
  e = new.env()
  name = load(PATH, envir = e)
  
  return( e[[name]])
}