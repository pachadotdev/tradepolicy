globalVariables(c("term"))

#' Download the Yotov database to your local computer
#'
#' This command downloads the Yotov trade database and populates a local
#' database. The download is 31.4 MB, and the database uses 3 GB on disk.
#' During import over 3.5 GB of disk space may be used temporarily.
#'
#' The database is stored by default under `tools::R_user_dir("yotover")`, or
#' its location can be set with the environment variable `YOTOV_DB_DIR`.
#'
#' @param tag What release tag of data to download. Defaults to the most recent.
#' Releases are expected to come twice per year. See all releases at
#' <https://github.com/pachamaltese/yotover/releases>.
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
#' \donttest{
#' \dontrun{
#' yotov_db_download()
#' }
#' }
yotov_db_download <- function(tag = NULL, destdir = tempdir(),
                              cleanup = TRUE, verbose = interactive()) {
  if (verbose) msg("Downloading data...\n")
  zfile <- get_gh_release_file("pachamaltese/yotover",
                               tag_name = tag,
                               destdir = destdir, verbose = verbose
  )
  ver <- attr(zfile, "ver")
  if (verbose) msg("\nDecompressing and building local database...\n")
  temp_tsv <- tempfile(fileext = ".tsv")
  destdir <- paste0(destdir, "/yotov-trade-db")
  destdir <- gsub("\\\\", "/", destdir)
  try(dir.create(destdir))
  utils::unzip(zfile, overwrite = TRUE, exdir = destdir)

  yotov_db_disconnect()
  try(unlink(yotov_path(), recursive = TRUE))

  finp <- list.files(destdir, full.names = TRUE)

  for (x in seq_along(finp)) {
    tout <- gsub(".*/", "", gsub("\\.tsv", "", finp[x]))

    msg(sprintf("Creating %s ...", tout))

    d <- utils::read.delim(finp[x],
                    sep = "\t",
                    stringsAsFactors = FALSE)

    if (any("year" %in% colnames(d))) {
      l <- list("year", "exporter", "importer")
    } else {
      l <- list("exporter", "importer")
    }

    if (any("country" %in% colnames(d))) {
      l <- list("country")
    }

    dplyr::copy_to(
      yotov_db(),
      d,
      tout,
      indexes = l,
      temporary = FALSE
    )

    rm(d, tout)

    yotov_db_disconnect()
  }

  file.remove(finp)

  invisible(dbListTables(yotov_db()))
  yotov_db_disconnect()

  update_yotov_pane()
  yotov_pane()
  invisible(yotov_status)
}


#' @importFrom httr GET stop_for_status content accept write_disk progress
#' @importFrom purrr keep
get_gh_release_file <- function(repo, tag_name = NULL, destdir = tempdir(),
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
  out_path <- normalizePath(file.path(destdir, filename), mustWork = FALSE)
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
