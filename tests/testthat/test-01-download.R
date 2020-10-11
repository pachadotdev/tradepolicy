context("Download")

olddir <- Sys.getenv("YOTOV_DB_DIR")
Sys.setenv(YOTOV_DB_DIR = normalizePath(file.path(getwd(), "localdb"),
  mustWork = FALSE
))

test_that("Download succeeds", {
  skip_on_cran()
  yotov_db_download()
  expect_true(yotov_status())
})

Sys.setenv(YOTOV_DB_DIR = olddir)
