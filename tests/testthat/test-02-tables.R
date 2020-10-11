olddir <- Sys.getenv("YOTOV_DB_DIR")
Sys.setenv(YOTOV_DB_DIR = normalizePath(file.path(getwd(), "localdb"),
  mustWork = FALSE
))

context("Tables")

test_that("Tables have expected types", {
  skip_on_cran()
  skip_if_not(yotov_status())
  expect_is(yotov_db(), "duckdb_connection")
  for (t in yotov_db_tables()) {
    expect_is(yotov_data(t), "tbl_df")
  }
})

Sys.setenv(YOTOV_DB_DIR = olddir)
