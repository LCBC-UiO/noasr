.onLoad <- function(libname, pkgname) {

  OS <- switch(Sys.info()[1],
               "Linux" = "linux",
               "Darwin" = "osx",
               "Windows" = "win"
  )

  font_files <- list.files("inst/avenir-font/", "ttf$", full.names = TRUE)

  switch(OS,
         "linux" = {
           if(!dir.exists('~/.fonts')) dir.create('~/.fonts')
           k <- sapply(font_files, file.copy,  "~/.fonts")
           system('fc-cache -f ~/.fonts')
         },
         "osx" = {
           k <- sapply(font_files, file.copy,  "~/Library/Fonts")
         },
         "win" = {
           extrafont::loadfonts(device = "win", quiet = TRUE)
         }
  )

  suppressWarnings(
    suppressMessages(
      extrafont::font_import(pattern = "Avenir", prompt = FALSE)
    )
  )
}
