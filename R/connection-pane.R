sql_action <- function() {
  if (requireNamespace("rstudioapi", quietly = TRUE) &&
    exists("documentNew", asNamespace("rstudioapi"))) {
    contents <- paste(
      "-- !preview conn=tradepolicy::tradepolicy_db()",
      "",
      "SELECT * FROM ch1_application1 LIMIT 100",
      "",
      sep = "\n"
    )

    rstudioapi::documentNew(
      text = contents, type = "sql",
      position = rstudioapi::document_position(2, 40),
      execute = FALSE
    )
  }
}

#' Open Yotov database connection pane in RStudio
#'
#' This function launches the RStudio "Connection" pane to interactively
#' explore the database.
#'
#' @return NULL
#' @export
#'
#' @examples
#' if (!is.null(getOption("connectionObserver"))) tradepolicy_pane()
tradepolicy_pane <- function() {
  observer <- getOption("connectionObserver")
  if (!is.null(observer) && interactive()) {
    observer$connectionOpened(
      type = "YotovDB",
      host = "yotovdb",
      displayName = "Datasets from 'An Advanced Guide to Trade Policy Analysis'",
      icon = system.file("img", "un-logo.png", package = "tradepolicy"),
      connectCode = "tradepolicy::tradepolicy_pane()",
      disconnect = tradepolicy::tradepolicy_db_disconnect,
      listObjectTypes = function() {
        list(
          table = list(contains = "data")
        )
      },
      listObjects = function(type = "datasets") {
        tbls <- DBI::dbListTables(tradepolicy_db())
        data.frame(
          name = tbls,
          type = rep("table", length(tbls)),
          stringsAsFactors = FALSE
        )
      },
      listColumns = function(table) {
        res <- DBI::dbGetQuery(
          tradepolicy_db(),
          paste("SELECT * FROM", table, "LIMIT 1")
        )
        data.frame(
          name = names(res), type = vapply(res, function(x) class(x)[1], character(1)),
          stringsAsFactors = FALSE
        )
      },
      previewObject = function(rowLimit, table) {
        DBI::dbGetQuery(
          tradepolicy_db(),
          paste("SELECT * FROM", table, "LIMIT", rowLimit)
        )
      },
      actions = list(
        Status = list(
          icon = system.file("img", "un-logo.png", package = "tradepolicy"),
          callback = tradepolicy_status
        ),
        SQL = list(
          icon = system.file("img", "edit-sql.png", package = "tradepolicy"),
          callback = sql_action
        )
      ),
      connectionObject = tradepolicy_db()
    )
  }
}

update_tradepolicy_pane <- function() {
  observer <- getOption("connectionObserver")
  if (!is.null(observer)) {
    observer$connectionUpdated("YotovDB", "yotovdb", "")
  }
}
