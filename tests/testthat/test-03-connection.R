olddir <- Sys.getenv("TRADEPOLICY_DB_DIR")
Sys.setenv(TRADEPOLICY_DB_DIR = normalizePath(file.path(getwd(), "tradepolicy"),
                                        mustWork = FALSE
))

context("Connection")

test_that("Disconnetion works", {
  skip_on_cran()
  skip_if_not(tradepolicy_status())

  expect_silent(tradepolicy_db_disconnect())
})

test_that("Database is deleted", {
  skip_on_cran()
  skip_if_not(tradepolicy_status())

  expect_error(tradepolicy_db_delete(), NA)
  expect_equal(DBI::dbListTables(tradepolicy_db()), character(0))
  expect_false(tradepolicy_status())
})

test_that("Tables fail when database is deleted", {
  skip_on_cran()

  for (t in tradepolicy_db_tables()) {
    expect_error(tradepolicy_data(t))
  }
})

try(unlink(Sys.getenv("TRADEPOLICY_DB_DIR"), recursive = TRUE))
Sys.setenv(TRADEPOLICY_DB_DIR = olddir)
