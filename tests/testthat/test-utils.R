test_that("mean dates", {
  dates <- as.Date(c("2020-01-20", "1998-01-20"))
  x <- mean_date(dates[1], dates[2])
  expect_equal(x, as.Date("2009-01-19"), tolerance = 0.0001)
  expect_equal(class(x), "Date")
})


test_that("as_date", {
  dates <- c("2020-01-20", "1998-01-20")
  x <- as_date(dates)
  expect_equal(class(x), "Date")

  expect_error(as_date(x), "is not TRUE")
})

test_that("check data", {
  expect_error(check_data(list()),
               "is not a data.frame")

  expect_error(check_data(data.frame()),
               "Data is missing necessary columns")

  expect_error(check_data(noas_example[,-1]),
               "subject_id")

})
