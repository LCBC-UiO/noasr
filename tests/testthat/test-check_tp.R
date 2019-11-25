test_that("check_tp works", {
  expect_error(check_tp(), "need an ID")
  expect_error(check_tp(100401), "need the MOAS")
  expect_error(check_tp(100401, "wrong/path/MOAS.RData"),
               "Cannot find MOAS")


})
