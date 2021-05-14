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
#' tradepolicy_db_delete()
#' }
#' }
tradepolicy_db_delete <- function() {
  suppressWarnings(tradepolicy_db_disconnect())
  try(unlink(tradepolicy_path(), recursive = TRUE))
  update_tradepolicy_pane()
}
