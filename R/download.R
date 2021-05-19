globalVariables(c("term"))

#' Download the Yotov database to your local computer
#'
#' This command downloads the Yotov trade database and populates a local
#' database. The download is 31.4 MB, and the database uses 3 GB on disk.
#' During import over 3.5 GB of disk space may be used temporarily.
#'
#' The database is stored by default under `tools::R_user_dir("tradepolicy")`, or
#' its location can be set with the environment variable `TRADEPOLICY_DIR`.
#'
#' @param tag What release tag of data to download. Defaults to the most recent.
#' Releases are expected to come twice per year. See all releases at
#' <https://github.com/pachamaltese/tradepolicy/releases>.
#' @param destdir Where to download the compressed file.
#' @param cleanup Whether to delete the compressed file after loading into the database.
#' @param verbose Whether to display messages and download progress
#'
#' @return NULL
#' @export
#' @importFrom DBI dbRemoveTable dbExistsTable dbCreateTable dbExecute
#'   dbWriteTable dbListTables
#'
#' @examples
#' \dontrun{ tp_download() }
tp_download <- function(ver = NULL, verbose = interactive()) {
  if (verbose) msg("\nDecompressing and building local database...\n")

  destdir <- tempdir()
  dir <- tp_path()

  suppressWarnings(try(dir.create(dir, recursive = TRUE)))

  zfile <- get_gh_release_file("pachamaltese/tradepolicy",
                               tag_name = ver,
                               dir = destdir
  )
  ver <- attr(zfile, "ver")

  suppressWarnings(try(tp_disconnect()))

  duckdb_version <- utils::packageVersion("duckdb")
  db_pattern <- paste0("v", gsub("\\.", "", duckdb_version), ".duckdb")

  existing_files <- list.files(tp_path())

  if (!any(grepl(db_pattern, existing_files))) {
    try(tp_delete())
  }

  utils::unzip(zfile, overwrite = TRUE, exdir = destdir)
  unlink(zfile)

  finp_tsv <- list.files(destdir, full.names = TRUE, pattern = "tsv")

  invisible(create_schema())

  for (x in seq_along(finp_tsv)) {

    tout <- gsub(".*/", "", gsub("\\.tsv", "", finp_tsv[x]))

    msg(sprintf("Creating %s table...", tout))

    con <- tp_database()

    suppressMessages(
      DBI::dbExecute(
        con,
        paste0(
          "COPY ", tout, " FROM '",
          finp_tsv[x],
          "' ( DELIMITER '\t', HEADER 1, NULL 'NA' )"
        )
      )
    )

    DBI::dbDisconnect(con, shutdown = TRUE)

    unlink(finp_tsv[x])
    invisible(gc())
  }

  metadata <- data.frame(duckdb_version = utils::packageVersion("duckdb"),
                          modification_date = Sys.time())
  metadata$duckdb_version <- as.character(metadata$duckdb_version)
  metadata$modification_date <- as.character(metadata$modification_date)

  con <- tp_database()
  suppressMessages(DBI::dbWriteTable(con, "metadata", metadata, append = T, temporary = F))
  DBI::dbDisconnect(con, shutdown = TRUE)

  unlink(destdir, recursive = TRUE)

  invisible(DBI::dbListTables(tp_database()))
  tp_disconnect()

  update_tp_pane()
  tp_pane()
  tp_status()
}


#' @importFrom httr GET stop_for_status content accept write_disk progress
#' @importFrom purrr keep
get_gh_release_file <- function(repo, tag_name = NULL, dir = tempdir(),
                                overwrite = TRUE, verbose = interactive()) {
  releases <- GET(
    paste0("https://api.github.com/repos/", repo, "/releases")
  )
  stop_for_status(releases, "finding releases")

  releases <- content(releases)

  if (is.null(tag_name)) {
    release_obj <- releases[1]
  } else {
    release_obj <- purrr::keep(releases, function(x) x$tag_name == tag_name)
  }

  if (!length(release_obj)) stop("No release tagged \"", tag_name, "\"")

  if (release_obj[[1]]$prerelease) {
    message("This is pre-release/sample data! It has not been cleaned or validated.")
  }

  download_url <- release_obj[[1]]$assets[[2]]$url
  filename <- basename(release_obj[[1]]$assets[[2]]$browser_download_url)
  out_path <- normalizePath(file.path(dir, filename), mustWork = FALSE)
  response <- GET(
    download_url,
    accept("application/octet-stream"),
    write_disk(path = out_path, overwrite = overwrite),
    if (verbose) progress()
  )
  stop_for_status(response, "downloading data")

  attr(out_path, "ver") <- release_obj[[1]]$tag_name
  return(out_path)
}
