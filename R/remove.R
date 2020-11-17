#' Remove the local Yotov database
#'
#' Deletes all tables from the local database.
#'
#' @return NULL
#' @export
#' @importFrom DBI dbListTables dbRemoveTable
#'
#' @examples
#' \donttest{
#' \dontrun{
#' yotov_db_delete()
#' }
#' }
yotov_db_delete <- function() {
  yotov_db_disconnect()
  try(unlink(yotov_path(), recursive = TRUE))
  update_yotov_pane()
}
