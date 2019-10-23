test_that("mutate_sensitive works", {
  set.seed(20)
  dt <- data.frame(
    CrossProject_ID = factor(c(rep(10, 2), 11, 13, rep(14, 3))),
    Age = c(21:22, 55, 33, 44:46),
    Test_Age = c(21:22, 55, 33, 44:46),
    CVLT_A = NA
  )

  ret_dt <- mutate_sensitive(dt)

  expect_equal(nrow(ret_dt), nrow(dt))
  expect_equal(ncol(ret_dt), ncol(dt))
  expect_equal(names(ret_dt), names(dt))
  expect_equal(ret_dt$Age,
               c(21, 22, 55, 33, 44, 45, 46))
  expect_equal(ret_dt$Test_Age,
               c(21, 22, 55, 33, 44, 45, 46))
  expect_equal(ret_dt$CVLT_A,
               rep(NA, nrow(dt)))

  # check IDs. its random, so check against
  # what should be repeated
  expect_true(ret_dt$CrossProject_ID[1] == ret_dt$CrossProject_ID[2])
  expect_true(ret_dt$CrossProject_ID[5] == ret_dt$CrossProject_ID[6])
  expect_true(ret_dt$CrossProject_ID[6] == ret_dt$CrossProject_ID[7])
  expect_false(ret_dt$CrossProject_ID[3] == ret_dt$CrossProject_ID[7])

  ret_dt <- mutate_sensitive(dt, scramble_ids = FALSE)
  expect_equal(dt$CrossProject_ID, ret_dt$CrossProject_ID)
})


test_that("deselect_sensitive works", {

  dt <- data.frame(
    CrossProject_ID = factor(c(rep(10, 2), 11, 13, rep(14, 3))),
    Age = c(21:22, 55, 33, 44:46),
    Comment = c("Something", "something else", "wat",rep("", 4)),
    Test_Comment = c("Something", "something else", "wat",rep("", 4)),
    Note = c("Something", "something else", "wat",rep("", 4)),
    PSQI_Desc = c("Something", "something else", "wat",rep("", 4)),
    CVLT_Desc = c("Something", "something else", "wat",rep("", 4)),
    PSQI_Date = NA,
    CVLT_Date = NA,
    NationalId = 11002229933,
    Medical_1 = c("Something", "something else", "wat",rep("", 4)),
    Something_Medical = c("Something", "something else", "wat",rep("", 4)),
    stringsAsFactors = FALSE
    )

  ret_dt <- deselect_sensitive(dt)

  expect_equal(nrow(ret_dt), nrow(dt))
  expect_equal(ncol(ret_dt), 2)
  expect_equal(names(ret_dt), c("CrossProject_ID", "Age"))

})


test_that("select_sensitive works", {

  dt <- data.frame(
    CrossProject_ID = factor(c(rep(10, 2), 11, 13, rep(14, 3))),
    Age = c(21:22, 55, 33, 44:46),
    Comment = c("Something", "something else", "wat",rep("", 4)),
    Test_Comment = c("Something", "something else", "wat",rep("", 4)),
    Note = c("Something", "something else", "wat",rep("", 4)),
    PSQI_Desc = c("Something", "something else", "wat",rep("", 4)),
    CVLT_Desc = c("Something", "something else", "wat",rep("", 4)),
    PSQI_Date = NA,
    CVLT_Date = NA,
    NationalId = 11002229933,
    Medical_1 = c("Something", "something else", "wat",rep("", 4)),
    Something_Medical = c("Something", "something else", "wat",rep("", 4)),
    stringsAsFactors = FALSE
  )

  ret_dt <- select_sensitive(dt)

  expect_equal(nrow(ret_dt), nrow(dt))
  expect_equal(ncol(ret_dt), 11)
  expect_equal(names(ret_dt),
               c("CrossProject_ID", "Comment", "Test_Comment", "Note", "PSQI_Desc",
                 "CVLT_Desc", "PSQI_Date", "CVLT_Date", "NationalId", "Medical_1",
                 "Something_Medical"))


})


test_that("anonymize_moas works", {


  dt <- data.frame(
    CrossProject_ID = factor(c(rep(10, 2), 11, 13, rep(14, 3))),
    Age = c(21:22, 55, 33, 44:46),
    Comment = c("Something", "something else", "wat",rep("", 4)),
    Test_Comment = c("Something", "something else", "wat",rep("", 4)),
    Note = c("Something", "something else", "wat",rep("", 4)),
    PSQI_Desc = c("Something", "something else", "wat",rep("", 4)),
    CVLT_Desc = c("Something", "something else", "wat",rep("", 4)),
    PSQI_Date = NA,
    CVLT_Date = NA,
    NationalId = 11002229933,
    Medical_1 = c("Something", "something else", "wat",rep("", 4)),
    Something_Medical = c("Something", "something else", "wat",rep("", 4)),
    stringsAsFactors = FALSE
  )

  ret_dt <- anonymize_moas(dt)

  expect_equal(nrow(ret_dt), nrow(dt))
  expect_equal(ncol(ret_dt), 2)
  expect_equal(names(ret_dt), c("CrossProject_ID", "Age"))

  expect_true(ret_dt$CrossProject_ID[6] == ret_dt$CrossProject_ID[7])
  expect_false(ret_dt$CrossProject_ID[3] == ret_dt$CrossProject_ID[7])
  expect_equal(ret_dt$Age,
               c(21, 22, 55, 33, 44, 45, 46))
})
