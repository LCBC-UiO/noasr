test_that("na_col_rm works", {
  df <- data.frame(
    ID = 1:3,
    Age = c(22, 25,17),
    Sex = NA,
    group = NA
  )

  df <- na_col_rm(df)
  expect_length(df, 2)
  expect_equal(nrow(df), 3)
  expect_equal(names(df), c("ID","Age"))

})

test_that("get_moas works", {

  dt <- data.frame(ID = c(10:12),
                   Age  = 22:24)

  dt2 <- get_moas(dt)
  expect_equal(dt, dt2)

  ff <- paste0(here::here("test.RData"))
  save(dt, file = ff)
  dt2 <- get_moas(ff)
  expect_equal(dt, dt2)
  unlink(ff)
})
