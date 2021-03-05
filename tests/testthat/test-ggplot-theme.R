library(ggplot2)
library(dplyr)
library(vdiffr)

p <- mtcars %>%
  ggplot(aes(x = wt, y = disp, colour = cyl)) +
  geom_point()

context("ggplot2-themes")
test_that("themes works", {
  expect_doppelganger("theme lcbc", p + theme_lcbc())
  expect_doppelganger("theme lcbc grid", p + theme_lcbc_grid())
  expect_doppelganger("theme lcbc void", p + theme_lcbc_void())

  expect_doppelganger("theme lcbc dark", p + theme_lcbc_dark())
  expect_doppelganger("theme lcbc dark grid", p + theme_lcbc_dark_grid())
  expect_doppelganger("theme lcbc dark void", p + theme_lcbc_dark_void())

})
