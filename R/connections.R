tp_path <- function() {
  duckdb_version <- utils::packageVersion("duckdb")
  sys_tp_path <- Sys.getenv("TRADEPOLICY_DIR")
  sys_tp_path <- gsub("\\\\", "/", sys_tp_path)
  if (sys_tp_path == "") {
    return(gsub("\\\\", "/", paste0(
      tools::R_user_dir("tradepolicy"),
      "/duckdb-", duckdb_version
    )))
  } else {
    return(gsub("\\\\", "/", paste0(sys_tp_path, "/duckdb-", duckdb_version)))
  }
}

#' The local AGTPA database
#'
#' Returns a connection to the local tradepolicy database. This is a DBI-compliant
#' duckdb database connection. When using **dplyr**-based
#' workflows, one typically accesses tables with [tp_data()], but this
#' function lets the user interact with the database directly via SQL.
#'
#' @param dir The location of the database on disk. Defaults to
#' `tradepolicy` under `tools::R_user_dir("tradepolicy")`, or the path specified
#' in the environment variable `TRADEPOLICY_DIR`.
#'
#' @export
#'
#' @examples
#' if (tp_status()) {
#'   DBI::dbListTables(tp_database())
#'
#'   ch1_application1 <- DBI::dbReadTable(tp_database(), "ch1_application1")
#'
#'   DBI::dbGetQuery(
#'     tp_database(),
#'     "SELECT * FROM ch1_application1"
#'   )
#' }
tp_database <- function(dir = tp_path()) {
  duckdb_version <- utils::packageVersion("duckdb")
  db_file <- paste0(dir, "/tradepolicy_duckdb_v", gsub("\\.", "", duckdb_version), ".duckdb")

  db <- mget("tp_database", envir = tp_cache, ifnotfound = NA)[[1]]

  if (inherits(db, "DBIConnection")) {
    if (DBI::dbIsValid(db)) {
      return(db)
    }
  }

  try(dir.create(dir, showWarnings = FALSE, recursive = TRUE))

  tryCatch({
    db <- DBI::dbConnect(
      duckdb::duckdb(),
      db_file
    )
  },
  error = function(e) {
    if (grepl("Failed to open database", e)) {
      stop(paste(
        "Local AGTPA database is locked by another R session.\n",
        "Try closing or running tp_disconnect() in that session."
      ),
      call. = FALSE
      )
    } else {
      stop(e)
    }
  },
  finally = NULL
  )

  assign("tp_database", db, envir = tp_cache)
  return(db)
}


#' AGTPA applications data
#'
#' Returns a remote database table with the data required to replicate the
#' exercises from the book.
#'
#' @param table A string indicating the table to extract
#' @return A **dplyr** tibble ([dplyr::tbl()])
#' @export
#'
#' @examples
#' if (tp_status()) {
#'   tp_data("ch1_application1")
#' }
#' @importFrom dplyr tbl
tp_data <- function(table) {
  df <- tryCatch(
    tibble::as_tibble(DBI::dbReadTable(tp_database(), table)),
    error = function(e) { read_table_error(e) }
  )
  return(df)
}


#' Disconnect from the AGTPA database
#'
#' A utility function for disconnecting from the database.
#'
#' @examples
#' tp_disconnect()
#' @export
#'
tp_disconnect <- function() {
  tp_disconnect_()
}
tp_disconnect_ <- function(environment = tp_cache) {
  db <- mget("tp_database", envir = tp_cache, ifnotfound = NA)[[1]]
  if (inherits(db, "DBIConnection")) {
    DBI::dbDisconnect(db, shutdown = TRUE)
  }
  observer <- getOption("connectionObserver")
  if (!is.null(observer)) {
    observer$connectionClosed("TradePolicyDB", "tradepolicydb")
  }
}


#' Get the status of the current local AGTPA database
#'
#' Get the status of the current local AGTPA database. It displays informative message
#' about how to create the local database if it can't be found or it is corrupt.
#'
#' @param verbose Whether to print a status message
#'
#' @return TRUE if the database exists, FALSE if it is not detected. (invisible)
#' @export
#' @importFrom DBI dbExistsTable
#' @importFrom tools toTitleCase
#' @examples
#' tp_status()
tp_status <- function(verbose = TRUE) {
  expected_tables <- sort(tp_tables())
  existing_tables <- sort(DBI::dbListTables(tp_database()))

  if (isTRUE(all.equal(expected_tables, existing_tables))) {
    status_msg <- paste(crayon::green(cli::symbol$tick, "Local AGTPA database is OK."))
    out <- TRUE
  } else {
    status_msg <- paste(crayon::red(cli::symbol$cross, "Local AGTPA database empty or corrupt. Download with tp_download()"))
    out <- FALSE
  }
  if (verbose) {
    msg(cli::rule("Database status"))
    msg(status_msg)
  }
  invisible(out)
}

#' AGTPA available tables
#' @export
tp_tables <- function() {
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
    "ch2_trade_without_border_fullge", "ch2_trade_without_border",
    "metadata"
  ))
}

tp_cache <- new.env()
reg.finalizer(tp_cache, tp_disconnect_, onexit = TRUE)
