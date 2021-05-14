tradepolicy_path <- function() {
  duckdb_version <- utils::packageVersion("duckdb")
  sys_tradepolicy_path <- Sys.getenv("TRADEPOLICY_DB_DIR")
  sys_tradepolicy_path <- gsub("\\\\", "/", sys_tradepolicy_path)
  if (sys_tradepolicy_path == "") {
    return(gsub("\\\\", "/", paste0(
      tools::R_user_dir("tradepolicy"),
      "/duckdb-", duckdb_version
    )))
  } else {
    return(gsub("\\\\", "/", paste0(sys_tradepolicy_path, "/duckdb-", duckdb_version)))
  }
}

tradepolicy_check_status <- function() {
  if (!tradepolicy_status(FALSE)) {
    stop("Local yotov database empty or corrupt. Download with tradepolicy_db_download()")
  }
}

#' The local Yotov database
#'
#' Returns a connection to the local yotov database. This is a DBI-compliant
#' duckdb database connection. When using **dplyr**-based
#' workflows, one typically accesses tables with [tradepolicy_data()], but this
#' function lets the user interact with the database directly via SQL.
#'
#' @param dbdir The location of the database on disk. Defaults to
#' `yotovdb` under [rappdirs::user_data_dir()], or the environment variable `TRADEPOLICY_DB_DIR`.
#'
#' @export
#'
#' @examples
#' if (tradepolicy_status()) {
#'   DBI::dbListTables(tradepolicy_db())
#'
#'   ch1_application1 <- DBI::dbReadTable(tradepolicy_db(), "ch1_application1")
#'
#'   DBI::dbGetQuery(
#'     tradepolicy_db(),
#'     "SELECT * FROM ch1_application1"
#'   )
#' }
tradepolicy_db <- function(dbdir = tradepolicy_path()) {
  db <- mget("tradepolicy_db", envir = tradepolicy_cache, ifnotfound = NA)[[1]]
  if (inherits(db, "DBIConnection")) {
    if (DBI::dbIsValid(db)) {
      return(db)
    }
  }

  try(dir.create(dbdir, showWarnings = FALSE, recursive = TRUE))

  tryCatch(
    {
      db <- DBI::dbConnect(
        duckdb::duckdb(),
        paste0(dbdir, "/tradepolicy_db.duckdb")
      )
    },
    error = function(e) {
      if (grepl("(Database lock|bad rolemask)", e)) {
        stop(paste(
          "Local yotov database is locked by another R session.\n",
          "Try closing or running tradepolicy_db_disconnect() in that session."
        ),
        call. = FALSE
        )
      } else {
        stop(e)
      }
    },
    finally = NULL
  )

  assign("tradepolicy_db", db, envir = tradepolicy_cache)
  db
}


#' Yotov applications data
#'
#' Returns a remote database table with the data required to replicate the
#' exercises from the book.
#'
#' @param table A string indicating the table to extract
#' @return A **dplyr** tibble ([dplyr::tbl()])
#' @export
#'
#' @examples
#' if (tradepolicy_status()) {
#'   tradepolicy_data("ch1_application1")
#' }
#' @importFrom dplyr tbl
tradepolicy_data <- function(table) {
  tradepolicy_check_status()
  df <- dplyr::as_tibble(DBI::dbReadTable(tradepolicy_db(), table))
  tradepolicy_db_disconnect()
  return(df)
}


#' Disconnect from the Yotov database
#'
#' A utility function for disconnecting from the database.
#'
#' @examples
#' tradepolicy_db_disconnect()
#' @export
#'
tradepolicy_db_disconnect <- function() {
  tradepolicy_db_disconnect_()
}
tradepolicy_db_disconnect_ <- function(environment = tradepolicy_cache) {
  db <- mget("tradepolicy_db", envir = tradepolicy_cache, ifnotfound = NA)[[1]]
  if (inherits(db, "DBIConnection")) {
    DBI::dbDisconnect(db, shutdown = TRUE)
  }
  observer <- getOption("connectionObserver")
  if (!is.null(observer)) {
    observer$connectionClosed("YotovDB", "yotovdb")
  }
}


#' Get the status of the current local Yotov database
#'
#' Get the status of the current local Yotov database. It displays informative message
#' about how to create the local database if it can't be found or it is corrupt.
#'
#' @param verbose Whether to print a status message
#'
#' @return TRUE if the database exists, FALSE if it is not detected. (invisible)
#' @export
#' @importFrom DBI dbExistsTable
#' @importFrom tools toTitleCase
#' @examples
#' tradepolicy_status()
tradepolicy_status <- function(verbose = TRUE) {
  expected_tables <- sort(tradepolicy_db_tables())
  existing_tables <- sort(DBI::dbListTables(tradepolicy_db()))

  if (isTRUE(all.equal(expected_tables, existing_tables))) {
    status_msg <- paste(crayon::green(cli::symbol$tick, "Local Yotov database is OK."))
    out <- TRUE
  } else {
    status_msg <- paste(crayon::red(cli::symbol$cross, "Local Yotov database empty or corrupt. Download with tradepolicy_db_download()"))
    out <- FALSE
  }
  if (verbose) {
    msg(cli::rule("Database status"))
    msg(status_msg)
  }
  invisible(out)
}

#' Yotov available tables
#' @export
tradepolicy_db_tables <- function() {
  sort(c(
    "ch1_application1", "ch1_application2",
    "ch1_application3", "ch1_exercise1", "ch1_exercise2", "ch2_application1",
    "ch2_application2", "ch2_exercise1", "ch2_exercise2",
    "ch2_removing_specific_border_results_full_cons_part_a",
    "ch2_removing_specific_border_results_full_cons_part_bc",
    "ch2_removing_specific_border_results_full_cons_part_d",
    "ch2_removing_specific_border_results_full_prod_part_a",
    "ch2_removing_specific_border_results_full_prod_part_bc",
    "ch2_removing_specific_border_results_full_prod_part_d",
    "ch2_removing_specific_border_results_fullge_part_a",
    "ch2_removing_specific_border_results_fullge_part_bc",
    "ch2_removing_specific_border_results_fullge_part_d",
    "ch2_removing_specific_border", "ch2_rt_as_effects_full_cons",
    "ch2_rt_as_effects_full_prod", "ch2_rt_as_effects",
    "ch2_rta_impacts_results_full_cons", "ch2_rta_impacts_results_full_prod",
    "ch2_rta_impacts_results_fullge", "ch2_rta_impacts",
    "ch2_trade_without_border_full_cons", "ch2_trade_without_border_full_prod",
    "ch2_trade_without_border_fullge", "ch2_trade_without_border"
  ))
}

tradepolicy_cache <- new.env()
reg.finalizer(tradepolicy_cache, tradepolicy_db_disconnect_, onexit = TRUE)
