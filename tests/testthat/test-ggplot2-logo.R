library(ggplot2)
library(vdiffr)

p <- ggplot(mtcars,
            aes(x = wt, y = disp, colour = cyl)) +
  geom_point()

context("ggplot2-logo")
test_that("multiplication works", {

  expect_doppelganger("lcbc logo main", p +  lcbc_logo())
  expect_doppelganger("lcbc logo brain", p +  lcbc_logo("brain"))
  expect_doppelganger("lcbc logo long", p +  lcbc_logo("long"))
  expect_doppelganger("lcbc logo text", p +  lcbc_logo("text"))
  expect_doppelganger("lcbc logo textrect", p +  lcbc_logo("textrect"))
  expect_doppelganger("lcbc logo wide", p +  lcbc_logo("wide"))
  expect_doppelganger("lcbc logo custom",
                      p +
                        lcbc_logo(alpha = 1,
                                  xmin = 4.7, xmax = 6.7,
                                  ymin = -10 , ymax = 30
                        ) +
                        coord_cartesian(clip = "off") +
                        theme(plot.margin = unit(c(1, 2, 3, 1), "lines"))
  )

})
