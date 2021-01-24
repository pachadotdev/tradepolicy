sql_action <- function() {
  if (requireNamespace("rstudioapi", quietly = TRUE) &&
    exists("documentNew", asNamespace("rstudioapi"))) {
    contents <- paste(
      "-- !preview conn=yotover::yotov_db()",
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
#' if (!is.null(getOption("connectionObserver"))) yotov_pane()
yotov_pane <- function() {
  observer <- getOption("connectionObserver")
  if (!is.null(observer) && interactive()) {
    observer$connectionOpened(
      type = "YotovDB",
      host = "yotovdb",
      displayName = "Yotov Application Tables",
      icon = system.file("img", "un-logo.png", package = "yotover"),
      connectCode = "yotover::yotov_pane()",
      disconnect = yotover::yotov_db_disconnect,
      listObjectTypes = function() {
        list(
          table = list(contains = "data")
        )
      },
      listObjects = function(type = "datasets") {
        tbls <- DBI::dbListTables(yotov_db())
        data.frame(
          name = tbls,
          type = rep("table", length(tbls)),
          stringsAsFactors = FALSE
        )
      },
      listColumns = function(table) {
        res <- DBI::dbGetQuery(
          yotov_db(),
          paste("SELECT * FROM", table, "LIMIT 1")
        )
        data.frame(
          name = names(res), type = vapply(res, function(x) class(x)[1], character(1)),
          stringsAsFactors = FALSE
        )
      },
      previewObject = function(rowLimit, table) {
        DBI::dbGetQuery(
          yotov_db(),
          paste("SELECT * FROM", table, "LIMIT", rowLimit)
        )
      },
      actions = list(
        Status = list(
          icon = system.file("img", "un-logo.png", package = "yotover"),
          callback = yotov_status
        ),
        SQL = list(
          icon = system.file("img", "edit-sql.png", package = "yotover"),
          callback = sql_action
        )
      ),
      connectionObject = yotov_db()
    )
  }
}

update_yotov_pane <- function() {
  observer <- getOption("connectionObserver")
  if (!is.null(observer)) {
    observer$connectionUpdated("YotovDB", "yotovdb", "")
  }
}
