.onAttach <- function(...) {
  needed <- core[!is_attached(core)]
  if (length(needed) == 0) {
    return()
  }

  msg(cli::rule(crayon::bold(paste0("tradepolicy ", package_version("tradepolicy")))))
  msg(paste(cli::symbol$star,
  "Please, read the documentation."))
  msg(paste(cli::symbol$star,
  "Use the command citation('tradepolicy') to cite this package in publications.\n"))

  tp_attach()
}

is_attached <- function(x) {
  paste0("package:", x) %in% search()
}
