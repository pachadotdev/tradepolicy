tp_path <- function() {
  sys_tp_path <- Sys.getenv("TRADEPOLICY_DIR")
  sys_tp_path <- gsub("\\\\", "/", sys_tp_path)
  if (sys_tp_path == "") {
    return(gsub("\\\\", "/", tools::R_user_dir("tradepolicy")))
  } else {
    return(gsub("\\\\", "/", sys_tp_path))
  }
}


tp_check_status <- function() {
  if (!tp_status(FALSE)) {
    stop("Local AGTPA database empty or corrupt. Download with tp_download().")
  }
}


#' The local AGTPA database
#'
#' Returns a local database connection, which is a DuckDB DBI-compatible connection.
#' Unlike [tradepolicy::tp_table()], this function is more flexible and can be used
#' with dbplyr to read exactly what you need or with DBI to use SQL commands.
#'
#' @param dir The location of the database on disk. Defaults to
#' `tradepolicy` under `tools::R_user_dir("tradepolicy")`, or the path specified
#' in the environment variable `TRADEPOLICY_DIR`.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'  DBI::dbListTables(tp_database())
#'
#'  DBI::dbGetQuery(
#'   tp_database(),
#'   "SELECT * FROM ch1_application1 WHERE exporter = 'CAN'"
#'  )
#' }
tp_database <- function(dir = tp_path()) {
  duckdb_version <- utils::packageVersion("duckdb")
  db_file <- paste0(dir, "/tp_duckdb_v", gsub("\\.", "", duckdb_version), ".sql")

  db <- mget("tp_database", envir = tp_cache, ifnotfound = NA)[[1]]

  if (inherits(db, "DBIConnection")) {
    if (DBI::dbIsValid(db)) {
      return(db)
    }
  }

  try(dir.create(dir, showWarnings = FALSE, recursive = TRUE))

  drv <- duckdb::duckdb(db_file, read_only = FALSE)

  tryCatch({
    con <- DBI::dbConnect(drv)
  },
  error = function(e) {
    if (grepl("Failed to open database", e)) {
      stop(
        paste(
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

  assign("tp_database", con, envir = tp_cache)
  con
}


#' AGTPA applications data
#'
#' Returns a remote database table with the data required to replicate the
#' exercises from the book. For pre-filtered data please use
#' [tradepolicy::tp_database()].
#'
#' @param table A string indicating the table to extract
#' @return A tibble
#' @export
#'
#' @examples
#' \dontrun{ tp_table("ch1_application1") }
tp_table <- function(tabla) {
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
#' @return NULL
#' @export
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
#' @param msg Whether to print a status message
#'
#' @return TRUE if the database exists, FALSE if it is not detected. (invisible)
#' @export
#' @importFrom DBI dbExistsTable
#' @importFrom tools toTitleCase
#' @examples
#' \dontrun{ tp_status() }
tp_status <- function(msg = TRUE) {
  expected_tables <- sort(tp_tables())
  existing_tables <- sort(DBI::dbListTables(tp_database()))

  if (isTRUE(all.equal(expected_tables, existing_tables))) {
    status_msg <- crayon::green(paste(cli::symbol$tick,
    "Local AGTPA database is OK."))
    out <- TRUE
  } else {
    status_msg <- crayon::red(paste(cli::symbol$cross,
    "Local AGTPA database empty or corrupt. Download with tp_download()."))
    out <- FALSE
  }
  if (msg) msg(status_msg)
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
