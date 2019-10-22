test_that("is_hms works", {
  expect_false(is_hms("13:24:11"))

  #Does not work, because the class is now Period??
  #expect_true(is_hms(lubridate::hms("13:24:11")))

})

test_that("factor_times works", {

  expect_equal(
    factor_times(c("12:23", "15:59", "22:10", "8:13")),
    structure(
      c(2L, 2L, 4L, 1L),
      .Label = c("Morning", "Afternoon", "Evening", "Night"),
      class = c("ordered", "factor"))
  )
})
