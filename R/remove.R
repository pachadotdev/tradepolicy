#' Remove the local Yotov database
#'
#' Deletes all tables from the local database.
#'
#' @return NULL
#' @export
#' @importFrom DBI dbListTables dbRemoveTable
#'
#' @examples
#' \dontrun{ tp_delete() }
tp_delete <- function() {
  suppressWarnings(tp_disconnect())
  try(unlink(gsub("/duckdb.*", "", tp_path()), recursive = TRUE))
  update_tp_pane()
}
