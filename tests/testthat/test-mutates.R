
# mutate_tp
test_that("mutate_tp works",{
  dt <- data.frame(
    CrossProject_ID = c(rep(1,4), rep(2,2),3,4),
    Project_Name = c(rep("MemP", 5), rep("MemC", 3)),
    Project_Wave = c(1:3, 3, 2, rep(1,3)),
    Age = c(10:12, 12:13, 14, 16, 18)
  )

  dt2 <- mutate_tp(dt)
  expect_equal( dt2$Subject_Timepoint,
                c(1, 2, 3, 3, 1, 2, 1, 1))
  expect_length(dt2, 5)

})

# mutate_mean_date ----
test_that("mutate_mean_date works", {

  dt <- data.frame(
    CrossProject_ID = c(rep(1,3), rep(2,2),3,4),
    Project_Name = c(rep("MemP", 4), rep("MemC", 3)),
    Project_Wave = c(1:3, 2, rep(1,3)),
    Test_Date = c("12.02.2012", "22.05.2015", "03.10.2017",
                  NA, "12.02.2012", NA, "22.05.2015"),
    MRI_Date = c("18.02.2012", "02.06.2015", "28.09.2017",
                 NA, NA, "22.05.2015", "30.05.2015"),
    stringsAsFactors = FALSE
  ) %>%
    mutate(MRI_Date = as.Date(MRI_Date, format="%d.%m.%Y"),
           Test_Date = as.Date(Test_Date, format="%d.%m.%Y"))

  dt2 <- mutate_mean_date(dt)
  expect_equal(dt2$Date, structure(
    c(15385, 16582.5, 17439.5, 15382, 16582.5, 16577, 16581),
    class = "Date"))
  expect_equal(names(dt2), c("CrossProject_ID", "Project_Name",
                             "Project_Wave", "Test_Date",
                             "MRI_Date", "Date"))

  expect_error(mutate_mean_date(select(dt, -Project_Name)),
               "columns are missing")

})


test_that("mean_date works",{

  expect_error(mean_date("2012-08-10", "2012-08-05"),
               "numeric")

  expect_equal(mean_date(as.Date("2012-08-10"), as.Date("2012-08-05")),
               structure(15559.5, class = "Date"))

})
