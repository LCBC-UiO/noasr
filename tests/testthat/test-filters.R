test_that("filter_site works", {
  dt <-  dplyr::tibble(
    subject_id = rep("1000000", 6),
    project_id = "MemP",
    wave_code = c(1,2,3,3,4,5),
    site_name = c("ousAvanto", "ousAvanto","ousAvanto",
                  "ousSkyra", "ousSkyra", "ousSkyra"),
    visit_age = c(8, 10, 14, 14, 17, 20),
  )

  # Check default action
  expect_output(filter_site(dt),"scanner with most data")
  expect_output(filter_site(dt, keep = "ousAvanto"),"ousAvanto")
  expect_output(filter_site(dt, keep = "ousSkyra"),"ousSkyra")
  expect_output(filter_site(dt, keep = "ousPrisma"),"ousPrisma")

  expect_equal(filter_site(dt, verbose = FALSE),
               dplyr::tibble(
                 subject_id = rep("1000000", 5),
                 project_id = "MemP",
                 wave_code = c(1,2,3,4,5),
                 site_name = c(rep("ousAvanto", 2), rep("ousSkyra", 3)),
                 visit_age = c(8, 10, 14, 17, 20)
               )
  )

  # Check site order action
  expect_equal(filter_site(dt,
                           site_order = c("ousAvanto", "ousSkyra", "ousPrisma"),
                           verbose = FALSE),
               dplyr::tibble(
                 subject_id = rep("1000000", 5),
                 project_id = "MemP",
                 wave_code = c(1,2,3,4,5),
                 site_name = c(rep("ousAvanto", 3), rep("ousSkyra", 2)),
                 visit_age = c(8, 10, 14, 17, 20)
               )
  )

  expect_equal(filter_site(dt,
                           site_order = c("ousAvanto", "ousPrisma", "ousSkyra"),
                           verbose = FALSE),
               dplyr::tibble(
                 subject_id = rep("1000000", 5),
                 project_id = "MemP",
                 wave_code = c(1,2,3,4,5),
                 site_name = c(rep("ousAvanto", 3), rep("ousSkyra", 2)),
                 visit_age = c(8, 10, 14, 17, 20)
               )
  )

  expect_equal(filter_site(dt,
                           site_order = c("ousAvanto", "ousPrisma", "ousSkyra"),
                           verbose = FALSE),
               dplyr::tibble(
                 subject_id = rep("1000000", 5),
                 project_id = "MemP",
                 wave_code = c(1,2,3,4,5),
                 site_name = c(rep("ousAvanto", 3), rep("ousSkyra", 2)),
                 visit_age = c(8, 10, 14, 17, 20)
               )
  )

  # Check tie options
  expect_equal(filter_site(dt, tie = "ousAvanto",
                           verbose = FALSE),
               dplyr::tibble(
                 subject_id = rep("1000000", 5),
                 project_id = "MemP",
                 wave_code = c(1,2,3,4,5),
                 site_name = c(rep("ousAvanto", 3), rep("ousSkyra", 2)),
                 visit_age = c(8, 10, 14, 17, 20)
               )
  )

  expect_equal(filter_site(dt, tie = "ousSkyra",
                           verbose = FALSE),
               dplyr::tibble(
                 subject_id = rep("1000000", 5),
                 project_id = "MemP",
                 wave_code = c(1,2,3,4,5),
                 site_name = c(rep("ousAvanto", 2), rep("ousSkyra", 3)),
                 visit_age = c(8, 10, 14, 17, 20)
               )
  )



  # Check when keep != "long"
  expect_equal(filter_site(dt, keep="ousAvanto",
                           verbose = FALSE),
               dplyr::tibble(
                 subject_id = "1000000",
                 project_id = "MemP",
                 wave_code = c(1,2,3,4,5),
                 site_name = c(rep("ousAvanto", 3), rep("ousSkyra", 2)),
                 visit_age = c(8, 10, 14, 17, 20)
               ))

  expect_equal(filter_site(dplyr::mutate(dt,
                                         site_name = gsub("Skyra", "Prisma", site_name)),
                           keep="ousPrisma",
                           verbose = FALSE),
               dplyr::tibble(
                 subject_id = "1000000",
                 project_id = "MemP",
                 wave_code = c(1,2,3,4,5),
                 site_name = c(rep("ousAvanto", 2), rep("ousPrisma", 3)),
                 visit_age = c(8, 10, 14, 17, 20),
               ))

  expect_error(filter_site(dt,
                           keep="something",
                           verbose = FALSE),
               "should be one of")
})
