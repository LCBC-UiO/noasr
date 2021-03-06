library(dplyr)

test_that("fs_lmm works", {
  tmp <- fs_lmm(noas_example,  ~ visit_age,
                site_var = site_name,
                folder_var = mri_info_folder)

  expect_equal(nrow(tmp), 10)
  expect_equal(ncol(tmp), 5)
  expect_equal(names(tmp), c("fsid", "fsid-base", "visit_age", "intercept", "visit_age_z"))
  expect_equal(tmp$visit_age_z, scale(tmp$visit_age)[,1])

  expect_error(fs_lmm(select(noas_example, -visit_age),
                      ~ cog,
                      site_var = site_name,
                      folder_var = mri_info_folder),
               "'visit_age' must be in the data")

  expect_warning(
    fs_lmm(mutate(noas_example, visit_age = NA),
           ~ cog,
           site_var = site_name,
           folder_var = mri_info_folder),
    "`NA` values in the 'visit_age'"
  )

  concat_list <- c("1000000_1", "1000000_8")
  tmp <- expect_warning(
    fs_lmm(noas_example, ~ visit_age,
           site_var = site_name,
           folder_var = mri_info_folder,
           concat_list = concat_list
    ), "1000000_8")
  expect_equal(tmp$fsid, "1000000_1")
  expect_equal(tmp$visit_age_z, NaN)


  concat_list <- c("1000000_1", "1000000_3", "1000000_5")
  tmp <- fs_lmm(noas_example, ~ visit_age,
                site_var = site_name,
                folder_var = mri_info_folder,
                concat_list = concat_list
  )
  expect_equal(tmp$fsid, concat_list)
  expect_equal(tmp$visit_age_z,
               c(-1.09108945117996, 0.218217890235992, 0.87287156094397))


  tmp <- expect_warning(
    fs_lmm(noas_example, ~ visit_age * cog,
           site_var = site_name,
           folder_var = mri_info_folder
    ),
    "Some data have been removed."
  )
  expect_equal(nrow(tmp), 8)

  file <- file.path(test_path(), "fs_lmm-files/test_doppel.csv")
  tmp <- fs_lmm(noas_example, ~ visit_age,
                site_var = site_name,
                folder_var = mri_info_folder,
                concat_list = concat_list,
                file = file
  )
  expect_true(file.exists(file))
  expect_equal(readLines(file),
               readLines(file.path(test_path(), "fs_lmm-files/test.csv")))
  expect_true(file.remove(file))
})

test_that("value replacing works", {

  values <- c(10, NA, 30)

  expect_equal(replace_na_mean(values),
               c(10, 20, 30))

  expect_equal(replace_all_mean(values),
               rep(20, 3))

  expect_equal(replace_all_first(values),
               rep(10, 3))

})

test_that("fix_numeric works", {
  tmp <- fix_numeric(noas_example, "delete")
  expect_equal(nrow(tmp), 8)
  expect_equal(names(tmp), names(noas_example))
  expect_true(all(!is.na(tmp$cog)))

  tmp <- fix_numeric(noas_example, "mean_na")
  expect_equal(nrow(tmp), 10)
  expect_equal(names(tmp), names(noas_example))
  expect_true(all(!is.na(tmp$cog)))
  expect_equal(tmp$cog,
               c(16, 14, 16, 15.2, 15, 15, 14, 13, 12.3, 10),
               tolerance = .1)
})
