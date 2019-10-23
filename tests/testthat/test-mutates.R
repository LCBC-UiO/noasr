test_that("mutate_mean_date works", {

  dt <- data.frame(
    CrossProject_ID = c(rep(1,3), rep(2,2),3,4),
    Project_Name = c(rep("MemP", 4), rep("MemC", 3)),
    Project_Wave = c(1:3, 2, rep(1,3)),
    Test_Date = c("12.02.2012", "22.05.2015", "03.10.2017",
                  NA, "12.02.2012", NA, "22.05.2015"),
    MRI_Date = c("18.02.2012", "02.06.2015", "28.09.2017",
                 NA, NA, "22.05.2015", "30.05.2015")
  )

  mutate_mean_date(dt)

})
