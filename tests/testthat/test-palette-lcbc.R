test_that("lcbc_cols works", {
  expect_error(lcbc_cols("grey"), "should be one of")
  expect_equal(lcbc_cols("light grey"),
               c(`light grey` = "#b2b2b2"))
  expect_equal(lcbc_cols("light grey", "light blue", "dark green"),
               c(`light grey` = "#b2b2b2",
                 `light blue` = "#66bfdd",
                 `dark green` = "#94c11f")
  )
})

test_that("lcbc_pal works", {
  expect_equal(class(lcbc_pal()), "function")

  expect_equal(lcbc_pal(reverse = TRUE)(4),
               c("#94C11F", "#C5DA84", "#66BFDD", "#009FE3")
  )

  expect_equal(lcbc_pal("blue")(4),
               c("#66BFDD", "#44B4DF", "#22A9E1", "#009FE3")
  )
})
