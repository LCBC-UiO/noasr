noas_example <-  dplyr::tibble(
  subject_id = c(rep("1000000", 6),
                 rep("1000010", 4)),
  project_id = c(rep("MemP", 6), rep("MemC", 4)),
  wave_code = c(c(1,2,3,3,4,5), 1:4),
  site_name = c("ousAvanto", "ousAvanto","ousAvanto",
                "ousSkyra", "ousSkyra", "ousSkyra",
                rep("ousSkyra", 4)),
  mri_info_folder = c(paste(rep("1000000", 6), 1:6, sep="_"),
             paste(rep("1000010", 4), 1:4, sep="_")
  ),
  visit_age = c(8, 10, 14, 14, 17, 20, 22, 28, 33, 40),
  cog = c(16, 14, 16, NA, 15, 15, 14, 13, NA, 10),
  sex = c(rep("female", 6), rep("male", 4))
)

usethis::use_data(noas_example, overwrite = TRUE)
