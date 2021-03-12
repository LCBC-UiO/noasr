test_that("test lcbc palette", {
  expect_equal(names(lcbc_colors),
               c("light green", "dark green", "light grey", "dark grey", "light blue",
                 "dark blue", "white", "black"))
})


test_that("test lcbc_cols", {
  expect_equal(names(lcbc_cols()),
               c("light green", "dark green", "light grey", "dark grey", "light blue",
                 "dark blue", "white", "black"))

  expect_equal(lcbc_cols("dark grey"), c(`dark grey` = "#626262"))
  expect_equal(lcbc_cols("light green"), c(`light green` = "#c5da84"))
  expect_equal(lcbc_cols("white"), c(white = "#e2e2e2")
  )

})

test_that("test lcbc_palettes", {
  expect_equal(names(lcbc_palettes),
               c("main", "light", "dark", "blue", "green", "grey")
  )
})

test_that("test lcbc_pal", {
  expect_equal(lcbc_pal()(1), "#009FE3" )
  expect_equal(lcbc_pal()(10), c("#009FE3", "#22A9E1", "#44B4DF", "#66BFDD", "#85C8BF", "#A5D1A1",
                                 "#C5DA84", "#B4D162", "#A4C940", "#94C11F"))
  expect_equal(lcbc_pal("light")(5),
               c("#66BFDD", "#A3D0DF", "#E2E2E2", "#D3DEB3", "#C5DA84") )
})

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




