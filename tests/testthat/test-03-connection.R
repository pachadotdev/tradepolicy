olddir <- Sys.getenv("YOTOV_DB_DIR")
Sys.setenv(YOTOV_DB_DIR = normalizePath(file.path(getwd(), "yotover"),
                                        mustWork = FALSE
))

context("Connection")

test_that("Disconnetion works", {
  skip_on_cran()
  skip_if_not(yotov_status())

  expect_silent(yotov_db_disconnect())
})

test_that("Database is deleted", {
  skip_on_cran()
  skip_if_not(yotov_status())

  expect_error(yotov_db_delete(), NA)
  expect_equal(DBI::dbListTables(yotov_db()), character(0))
  expect_false(yotov_status())
})

test_that("Tables fail when database is deleted", {
  skip_on_cran()

  for (t in yotov_db_tables()) {
    expect_error(yotov_data(t))
  }
})

unlink(Sys.getenv("YOTOV_DB_DIR"), recursive = TRUE)
Sys.setenv(YOTOV_DB_DIR = olddir)
