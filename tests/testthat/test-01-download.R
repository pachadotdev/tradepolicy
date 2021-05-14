context("Download")

olddir <- Sys.getenv("TRADEPOLICY_DB_DIR")
Sys.setenv(TRADEPOLICY_DB_DIR = normalizePath(file.path(getwd(), "tradepolicy"),
  mustWork = FALSE
))

test_that("Download succeeds", {
  skip_on_cran()
  tradepolicy_db_download()
  expect_true(tradepolicy_status())
})

Sys.setenv(TRADEPOLICY_DB_DIR = olddir)
