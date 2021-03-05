test_that("test project palette", {
  expect_equal(names(project_colors),
               c("NDev", "MemP", "NCP", "MoBa", "Loci", "MemC", "ACon", "S2C"
               ))
})


test_that("test project_cols", {
  expect_equal(names(project_cols()),
               c("NDev", "MemP", "NCP", "MoBa", "Loci", "MemC", "ACon", "S2C"
               ))

  expect_equal(project_cols("MemP"), c(MemP = "#f94333"))
  expect_equal(project_cols("S2C"), c(S2C = "#155858"))

  expect_error(project_cols("S2c"),
               "should be one of")
})

test_that("test project_palettes", {
  expect_equal(names(project_palettes),
               c("main", "training", "memp", "named")
  )
})

test_that("test project_pal", {
  expect_equal(project_pal()(1), "#C5DA84")
  expect_equal(project_pal()(10), c("#C5DA84", "#ED6445", "#8F4D5C", "#797364", "#DEAB3E", "#3B978B",
                                    "#805049", "#CF473B", "#C7645A", "#155858") )
  expect_equal(project_pal("memp")(5),
               c("#F94333", "#D3382B", "#AE2E23", "#D34B3F", "#FA685B") )

  expect_equal(project_pal("memp", reverse = TRUE)(5),
               c("#FA685B", "#D34B3F", "#AE2E23", "#D3382B", "#F94333") )

  expect_equal(project_pal("named"),
               c(NDev = "#c5da84", MemP = "#f94333", NCP = "#3b567d", MoBa = "#f6ae33",
                 Loci = "#249596", MemC = "#ae2e23", ACon = "#fa685b", S2C = "#155858"
               ))
})





