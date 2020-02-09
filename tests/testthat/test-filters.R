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
  expect_output(filter_site(dt, keep = "ousAvanto"),"ousAvanto")
  expect_output(filter_site(dt, keep = "ousSkyra"),"ousSkyra")
  expect_output(filter_site(dt, keep = "ousPrisma"),"ousPrisma")

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

  # expect_equal(filter_site(dt, keep="ousPrisma",
  #                          quiet = TRUE),
  #              data.frame(
  #                CrossProject_ID = "1000000",
  #                Site_Name = c(rep("ousAvanto", 2), rep("ousSkyra", 3)),
  #                Age = c(8, 10, 14, 17, 20),
  #                Subject_Timepoint = c(1:5),
  #                stringsAsFactors = FALSE
  #              ))

  expect_error(filter_site(dt,
                           keep="something",
                           quiet = TRUE),
               "should be one of")

  expect_output(site_keeper(dt,
                            keep="ousSkyra",
                            quiet = TRUE),
                "site_keeper will be deprecated")

})

test_that("filter_trainexposed works",{


  dt <- data.frame(
    CrossProject_ID= c(rep(1:3, each=3),3),
    Age = c(10,13,14,
            23,27,34,
            55, 56, 70, 73),
    Project_Name = c(rep("HUK",9), "MEM"),
    Project_Wave = c(rep(1:3, 3), 1),
    training = c("baseline", "rest", "train",
                 "baseline", "train", "rest",
                 "baseline", "train", "train", "baseline"),
    stringsAsFactors = FALSE
  )

 dt_f <- filter_trainingexposed(dt, grepl("train", training))

 expect_false(any(grepl("train", dt_f$training)))

 expect_equal(class(dt_f), c("tbl_df", "tbl", "data.frame"))
 expect_equal(nrow(dt_f), 4)


 dt_f <- filter_trainingexposed(dt, grepl("rest", training))

 expect_false(any(grepl("rest", dt_f$training)))

 expect_equal(class(dt_f), c("tbl_df", "tbl", "data.frame"))
 expect_equal(nrow(dt_f), 7)

 expect_error(
   filter_trainingexposed(select(dt, - Age), grepl("rest", training)),
   "necessary columns"
   )

})
