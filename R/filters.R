filter_sensitive <- function(MOAS){

  filter(MOAS,
         contains("Comment"),
         contains("Note"),
         ends_with("Desc"),    # All freetext columns, may contain medical information
         contains("Date"),     # All dates, may be able to trace participant
         contains("National"), # Anything with "national, can contain national ID
         contains("Medical"),  # Anything relating to medical information
  )

}
