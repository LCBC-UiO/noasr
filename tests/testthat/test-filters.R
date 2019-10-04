context("test-site_keeper")

test_that("filter_site works", {
  dt <-  data.frame(
    CrossProject_ID = rep("1000000", 6),
    Site_Name = c("ousAvanto", "ousAvanto","ousAvanto",
                  "ousSkyra", "ousSkyra", "ousSkyra"),
    Subject_Timepoint = c(1:3,3:5),
    Age = c(8, 10, 14, 14, 17, 20),
    stringsAsFactors = FALSE
  )

  # Check default action
  expect_output(filter_site(dt),"scanner with most data")
  expect_equal(filter_site(dt, quiet = TRUE),
               data.frame(
                 CrossProject_ID = rep("1000000", 5),
                 Site_Name = c(rep("ousAvanto", 2), rep("ousSkyra", 3)),
                 Subject_Timepoint = 1:5,
                 Age = c(8, 10, 14, 17, 20),
                 stringsAsFactors = FALSE)
  )

  # Check site order action
  expect_equal(filter_site(dt,
                           site_order = c("ousAvanto", "ousSkyra", "ousPrisma"),
                           quiet = TRUE),
               data.frame(
                 CrossProject_ID = rep("1000000", 5),
                 Site_Name = c(rep("ousAvanto", 3), rep("ousSkyra", 2)),
                 Subject_Timepoint = 1:5,
                 Age = c(8, 10, 14, 17, 20),
                 stringsAsFactors = FALSE)
  )

  expect_equal(filter_site(dt,
                           site_order = c("ousAvanto", "ousPrisma", "ousSkyra"),
                           quiet = TRUE),
               data.frame(
                 CrossProject_ID = rep("1000000", 5),
                 Site_Name = c(rep("ousAvanto", 3), rep("ousSkyra", 2)),
                 Subject_Timepoint = 1:5,
                 Age = c(8, 10, 14, 17, 20),
                 stringsAsFactors = FALSE)
  )

  expect_equal(filter_site(dt,
                           site_order = c("ousAvanto", "ousPrisma", "ousSkyra"),
                           quiet = TRUE),
               data.frame(
                 CrossProject_ID = rep("1000000", 5),
                 Site_Name = c(rep("ousAvanto", 3), rep("ousSkyra", 2)),
                 Subject_Timepoint = 1:5,
                 Age = c(8, 10, 14, 17, 20),
                 stringsAsFactors = FALSE)
  )

  # Check tie options
  expect_equal(filter_site(dt, tie = "ousAvanto",
                           quiet = TRUE),
               data.frame(
                 CrossProject_ID = rep("1000000", 5),
                 Site_Name = c(rep("ousAvanto", 3), rep("ousSkyra", 2)),
                 Subject_Timepoint = 1:5,
                 Age = c(8, 10, 14, 17, 20),
                 stringsAsFactors = FALSE)
  )

  expect_equal(filter_site(dt, tie = "ousSkyra",
                           quiet = TRUE),
               data.frame(
                 CrossProject_ID = rep("1000000", 5),
                 Site_Name = c(rep("ousAvanto", 2), rep("ousSkyra", 3)),
                 Subject_Timepoint = 1:5,
                 Age = c(8, 10, 14, 17, 20),
                 stringsAsFactors = FALSE)
  )



  # Check when keep != "long"
  expect_equal(filter_site(dt, keep="ousAvanto",
                           quiet = TRUE),
               data.frame(
                 CrossProject_ID = "1000000",
                 Site_Name = c(rep("ousAvanto", 3), rep("ousSkyra", 2)),
                 Age = c(8, 10, 14, 17, 20),
                 Subject_Timepoint = c(1:5),
                 stringsAsFactors = FALSE
               ))

  expect_equal(filter_site(dt, keep="ousSkyra",
                           quiet = TRUE),
               data.frame(
                 CrossProject_ID = "1000000",
                 Site_Name = c(rep("ousAvanto", 2), rep("ousSkyra", 3)),
                 Age = c(8, 10, 14, 17, 20),
                 Subject_Timepoint = c(1:5),
                 stringsAsFactors = FALSE
               ))

})
