# test_that("docs_project are created", {
#
#   tmpdir <- "tmp/"
#
#   docs_project_page(project_acronym = "MemP", 11, path = tmpdir)
#   expect_true(file.exists(paste0(tmpdir, "11-MemP.Rmd")))
#
#   docs_project_page(project_acronym = "NCP", 12, path = tmpdir)
#   expect_true(file.exists(paste0(tmpdir, "12-NCP.Rmd")))
#
#   unlink(tmpdir, recursive = TRUE)
# })
