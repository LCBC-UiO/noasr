context("test-site_keeper")

test_that("site_keeper works", {
  dt <-  tibble(
    CrossProject_ID = rep("1000000", 5),
    Site_Name = c("ousSkyra", "ousSkyra", "ousAvanto",
                  "ousAvanto", "ousAvanto"),
    Subject_Timepoint = c(1:2,2:4)
  )

  dt2 <-

  expect_message(site_keeper(dt),"scanner with most data")
  expect_equal(site_keeper(dt),
               tibble(
    CrossProject_ID = "1000000",
    Site_Name = c("ousSkyra", "ousAvanto",
                  "ousAvanto", "ousAvanto"),
    Subject_Timepoint = c(1:4)
  ))

  expect_equal(site_keeper(dt, keep="ousAvanto"),
               tibble(
                 CrossProject_ID = "1000000",
                 Site_Name = c("ousSkyra", "ousAvanto",
                               "ousAvanto", "ousAvanto"),
                 Subject_Timepoint = c(1:4)
               ))

  expect_equal(site_keeper(dt, keep="ousSkyra"),
               tibble(
                 CrossProject_ID = "1000000",
                 Site_Name = c("ousSkyra", "ousSkyra",
                               "ousAvanto", "ousAvanto"),
                 Subject_Timepoint = c(1:4)
               ))

})
