wrk_dir <- paste0(here(), "/tests/testthat/tmp_docs")

test_that("docs_paradigm_page works", {
  docs_paradigm_page("ginger", "fmri", path = wrk_dir)

})

unlink(wrk_dir,recursive = TRUE)
