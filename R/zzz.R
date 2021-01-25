globalVariables(c("country", "exporter", "importer", "name", "type", "value"))

.onAttach <- function(...) {
  needed <- core[!is_attached(core)]
  if (length(needed) == 0) {
    return()
  }

  msg(cli::rule(crayon::bold(paste0("yotover ", package_version("yotover")))))
  msg(crayon::green(paste(cli::symbol$star,
                          "Please, read the documentation.")))
  msg(crayon::green(paste(cli::symbol$star,
                          "Use the command citation('yotover') to cite this package in publications.")))
  msg(crayon::green(paste(cli::symbol$star,
                          "Visit https://buymeacoffee.com/pacha if you'd like to donate to help improving this software.")))
  msg(crayon::green(paste(cli::symbol$warning,
                          "This package downloads a 30 MB compressed file and creates a 6 GB database.")))
  msg(crayon::green(paste(cli::symbol$warning,
                          "If you don't want to create a database in your home directory,")))
  msg(crayon::green(paste(cli::symbol$warning,
                          "run usethis::edit_r_environ() and create the environment variable YOTOV_DB_DIR with your desired location.")))

  yotover_attach()

  if (interactive() && Sys.getenv("RSTUDIO") == "1" && !in_chk()) {
    yotov_pane()
  }
  if (interactive()) yotov_status()
}

in_chk <- function() {
  any(
    grepl(
      "check",
      sapply(sys.calls(), function(a) paste(deparse(a), collapse = "\n"))
    )
  )
}

is_attached <- function(x) {
  paste0("package:", x) %in% search()
}
