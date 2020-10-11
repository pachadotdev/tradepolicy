#' @importFrom rappdirs user_data_dir
yotov_path <- function() {
  sys_yotover_path <- Sys.getenv("yotover_DB_DIR")
  sys_yotover_path <- gsub("\\\\", "/", sys_yotover_path)
  if (sys_yotover_path == "") {
    return(gsub("\\\\", "/", paste0(rappdirs::user_data_dir(), "/yotover")))
  } else {
    return(gsub("\\\\", "/", sys_yotover_path))
  }
}

yotov_check_status <- function() {
  if (!yotov_status(FALSE)) {
    stop("Local yotov database empty or corrupt. Download with yotov_db_download()")
  }
}

#' The local Yotov database
#'
#' Returns a connection to the local yotov database. This is a DBI-compliant
#' duckdb database connection. When using **dplyr**-based
#' workflows, one typically accesses tables with [yotov_data()], but this
#' function lets the user interact with the database directly via SQL.
#'
#' @param dbdir The location of the database on disk. Defaults to
#' `yotovdb` under [rappdirs::user_data_dir()], or the environment variable `yotov_DB_DIR`.
#'
#' @export
#'
#' @examples
#' if (yotov_status()) {
#'  library(DBI)
#'
#'  dbListTables(yotov_db())
#'
#'  ch1_application1 <- dbReadTable(yotov_db(), "ch1_application1")
#'
#'  dbGetQuery(
#'   yotov_db(),
#'   'SELECT * FROM ch1_application1'
#'  )
#' }
yotov_db <- function(dbdir = yotov_path()) {
  db <- mget("yotov_db", envir = yotover_cache, ifnotfound = NA)[[1]]
  if (inherits(db, "DBIConnection")) {
    if (DBI::dbIsValid(db)) {
      return(db)
    }
  }

  try(dir.create(dbdir, FALSE))

  tryCatch({
    db <- DBI::dbConnect(
      duckdb::duckdb(),
      paste0(dbdir, "/yotov_db.duckdb")
    )
  },
  error = function(e) {
    if (grepl("(Database lock|bad rolemask)", e)) {
      stop(paste(
        "Local yotov database is locked by another R session.\n",
        "Try closing or running yotov_db_disconnect() in that session."
      ),
      call. = FALSE
      )
    } else {
      stop(e)
    }
  },
  finally = NULL
  )

  assign("yotov_db", db, envir = yotover_cache)
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
#' if (yotov_status()) {
#'   # See the number of yotov shipment records per year
#'   yotov_data("ch1_application1") %>%
#'     filter(exporter == "ARG")
#' }
#' @importFrom dplyr tbl
yotov_data <- function(table) {
  yotov_check_status()
  dplyr::as_tibble(DBI::dbReadTable(yotov_db(), table))
}


#' Disconnect from the Yotov database
#'
#' A utility function for disconnecting from the database.
#'
#' @examples
#' yotov_db_disconnect()
#' @export
#'
yotov_db_disconnect <- function() {
  yotov_db_disconnect_()
}
yotov_db_disconnect_ <- function(environment = yotover_cache) {
  db <- mget("yotov_db", envir = yotover_cache, ifnotfound = NA)[[1]]
  if (inherits(db, "DBIConnection")) {
    DBI::dbDisconnect(db, shutdown = TRUE)
    duckdb::duckdb_shutdown(duckdb::duckdb())
  }
  observer <- getOption("connectionObserver")
  if (!is.null(observer)) {
    observer$connectionClosed("YotovDB", "yotovdb")
  }
}


#' Get the status of the current local Yotov database
#'
#' @param verbose Whether to print a status message
#'
#' @return TRUE if the database exists, FALSE if it is not detected. (invisible)
#' @export
#' @importFrom DBI dbExistsTable
#' @importFrom tools toTitleCase
#' @examples
#' yotov_status()
yotov_status <- function(verbose = TRUE) {
  expected_tables <- sort(yotov_db_tables())
  existing_tables <- sort(DBI::dbListTables(yotov_db()))

  if (isTRUE(all.equal(expected_tables, existing_tables))) {
    status_msg <- paste(crayon::green(cli::symbol$tick), "Local Yotov database is OK.")
    out <- TRUE
  } else {
    status_msg <- paste(crayon::red(cli::symbol$cross), "Local Yotov database empty or corrupt. Download with yotov_db_download()")
    out <- FALSE
  }
  if (verbose) msg(cli::rule(status_msg))
  invisible(out)
}

#' Yotov available tables
#' @export
yotov_db_tables <- function() {
  sort(c('ch1_application1', 'ch1_application2',
    'ch1_application3', 'ch1_exercise1', 'ch1_exercise2', 'ch2_application1',
    'ch2_application2', 'ch2_exercise1', 'ch2_exercise2',
    'ch2_removing_specific_border_results_full_cons_part_a',
    'ch2_removing_specific_border_results_full_cons_part_bc',
    'ch2_removing_specific_border_results_full_cons_part_d',
    'ch2_removing_specific_border_results_full_prod_part_a',
    'ch2_removing_specific_border_results_full_prod_part_bc',
    'ch2_removing_specific_border_results_full_prod_part_d',
    'ch2_removing_specific_border_results_fullge_part_a',
    'ch2_removing_specific_border_results_fullge_part_bc',
    'ch2_removing_specific_border_results_fullge_part_d',
    'ch2_removing_specific_border', 'ch2_rt_as_effects_full_cons',
    'ch2_rt_as_effects_full_prod', 'ch2_rt_as_effects',
    'ch2_rta_impacts_results_full_cons', 'ch2_rta_impacts_results_full_prod',
    'ch2_rta_impacts_results_fullge', 'ch2_rta_impacts',
    'ch2_trade_without_border_full_cons', 'ch2_trade_without_border_full_prod',
    'ch2_trade_without_border_fullge', 'ch2_trade_without_border'))
}

yotover_cache <- new.env()
reg.finalizer(yotover_cache, yotov_db_disconnect_, onexit = TRUE)
