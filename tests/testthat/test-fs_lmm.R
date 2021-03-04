

test_that("fs_lmm works", {
  fs_lmm(noas_example,  ~ age * sex * cog,
         site_var = site_name,
         folder_var = folder)
})
