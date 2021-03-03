dt <- data.frame(
  subject_id = c(rep(1,4), rep(2,2),3,4),
  project_id = c(rep("MemP", 5), rep("MemC", 3)),
  wave_code = c(1:3, 3, 2, rep(1,3)),
  age = c(10:12, 12:13, 14, 16, 18)
)

# add_timepoint
test_that("add_timepoint works",{
  dt2 <- add_timepoint(dt)
  expect_equal( dt2$timepoint,
                c(1, 2, 3, 3, 1, 2, 1, 1))
  expect_length(dt2, 5)


  dt2 <- add_timepoint(dt, tp)
  expect_equal( dt2$tp,
                c(1, 2, 3, 3, 1, 2, 1, 1))
  expect_length(dt2, 5)

  expect_error(add_timepoint(dplyr::select(dt, -age)),
               "needs 'age'")
})


# add_interval
test_that("add_interval works",{
  dt2 <- add_interval(dt)
  expect_equal( dt2$interval,
                c(0,1,1,1,0,1,0,0))
  expect_length(dt2, 5)

  dt2 <- add_interval(dt, interval_visit)
  expect_equal( dt2$interval_visit,
                c(0,1,1,1,0,1,0,0))
  expect_length(dt2, 5)

  expect_error(add_interval(dplyr::select(dt, -age)),
               "needs 'age'")
})

# add_interval
test_that("add_interval_baseline works",{
  dt2 <- add_interval_baseline(dt)
  expect_equal( dt2$interval_baseline,
                c(0,1,2,2,0,1,0,0))
  expect_length(dt2, 5)

  dt2 <- add_interval_baseline(dt, baseline_int)
  expect_equal( dt2$baseline_int,
                c(0,1,2,2,0,1,0,0))
  expect_length(dt2, 5)

  expect_error(add_interval_baseline(dplyr::select(dt, -age)),
               "needs 'age'")
})

