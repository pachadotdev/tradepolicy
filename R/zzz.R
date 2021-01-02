globalVariables(c("country", "exporter", "importer", "name", "type", "value"))

.onAttach <- function(...) {
  needed <- core[!is_attached(core)]
  if (length(needed) == 0) {
    return()
  }

  crayon::num_colors(TRUE)
  yotover_attach()

  if (interactive() && Sys.getenv("RSTUDIO") == "1"  && !in_chk()) {
    yotov_pane()
  }
  if (interactive()) yotov_status()
}

in_chk <- function() {
  any(
    grepl("check",
          sapply(sys.calls(), function(a) paste(deparse(a), collapse = "\n"))
    )
  )
}

is_attached <- function(x) {
  paste0("package:", x) %in% search()
}
