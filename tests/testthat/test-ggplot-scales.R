library(ggplot2)
library(vdiffr)

context("ggplot2-scales-lcbc")
p <- ggplot(mtcars, aes(x = wt))

test_that("lcbc scales works", {

  expect_doppelganger("LCBC Continuous colour",
                      p +
                        geom_point(aes(y = disp, colour = cyl)) +
                        scale_colour_lcbc(discrete = FALSE)
  )

  expect_doppelganger("LCBC Continuous colour dark",
                      p +
                        geom_point(aes(y = disp, colour = cyl)) +
                        scale_colour_lcbc("dark", discrete = FALSE)
  )

  expect_doppelganger("LCBC Continuous colour reverse",
                      p +
                        geom_point(aes(y = disp, colour = cyl)) +
                        scale_colour_lcbc(discrete = FALSE, reverse = TRUE)
  )


  expect_doppelganger("LCBC Continuous fill",
                      p +
                        geom_histogram(aes(fill = ..count..), bins = 30) +
                        scale_fill_lcbc(discrete = FALSE)
  )


  expect_doppelganger("LCBC Discrete colour",
                      p +
                        geom_point(aes(y = disp, colour = factor(cyl)))  +
                        scale_colour_lcbc(discrete = TRUE)
  )

  p_warn <- expect_warning(ggplot(mtcars, aes(x = factor(cyl))) +
                   geom_histogram(stat = "count",
                                  aes(fill = factor(cyl))) +
                   scale_fill_lcbc(discrete = TRUE),
                   "Ignoring unknown parameters"
                 )

  expect_doppelganger("LCBC Discrete fill", p_warn)
})


context("ggplot2-scales-proj")
test_that("project scales works", {

  expect_doppelganger("Project Continuous colour",
                      p +
                        geom_point(aes(y = disp, colour = cyl)) +
                        scale_colour_proj(discrete = FALSE)
  )

  expect_doppelganger("Project Continuous colour vector",
                      p +
                        geom_point(aes(y = disp, colour = cyl)) +
                        scale_colour_proj(c("MemP", "NCP", "MoBa"), discrete = FALSE)
  )

  expect_doppelganger("Project Continuous colour reverse",
                      p +
                        geom_point(aes(y = disp, colour = cyl)) +
                        scale_colour_proj(c("MemP", "NCP", "MoBa"),
                                          discrete = FALSE, reverse = TRUE)
  )


  expect_doppelganger("Project Continuous fill",
                      p +
                        geom_histogram(aes(fill = ..count..)) +
                        scale_fill_proj(discrete = FALSE)
  )


  expect_doppelganger("Project Discrete colour",
                      p +
                        geom_point(aes(y = disp, colour = factor(cyl)))  +
                        scale_colour_proj(c("MemP", "NCP", "MoBa"), discrete = TRUE)
  )

  p_warn <- expect_warning(
    noas_example %>%
      dplyr::mutate(project_id = gsub("MemC", "NCP", project_id)) %>%
      ggplot(aes(project_id)) +
      geom_histogram(stat = "count", aes(fill = project_id))  +
      scale_fill_proj("named"),
    "Ignoring unknown parameters"
  )
  expect_doppelganger("Project named fill", p_warn)

  expect_doppelganger("Project named colour",
                      noas_example %>%
                        dplyr::mutate(project_id = gsub("MemC", "NCP", project_id)) %>%
                        ggplot(aes(project_id, visit_age)) +
                        geom_point(aes(colour = project_id))  +
                        scale_colour_proj("named")
  )

  p_warn <- expect_warning(ggplot(mtcars, aes(x = factor(cyl))) +
                             geom_histogram(stat = "count",
                                            aes(fill = factor(cyl))) +
                             scale_fill_proj(c("MemP", "NCP", "MoBa"), discrete = TRUE),
                           "Ignoring unknown parameters"
  )

  expect_doppelganger("Project Discrete fill", p_warn)
})
