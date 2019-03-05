context("test-widen")
library(tidyverse)

dt <-  tibble(
  CrossProject_ID = rep("1000000", 5),
  Site_Name = c("ousSkyra", "ousSkyra", "ousAvanto",
                "ousAvanto", "ousAvanto"),
  Subject_Timepoint = c(1:2,2:4),
  Age = c(10,12,12,14,18)
)

test_that("multiplication works", {


})
