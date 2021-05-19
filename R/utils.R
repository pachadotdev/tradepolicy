msg <- function(..., startup = FALSE) {
  if (startup) {
    if (!isTRUE(getOption("tradepolicy.quiet"))) {
      packageStartupMessage(text_col(...))
    }
  } else {
    message(text_col(...))
  }
}

text_col <- function(x) {
  # If RStudio not available, messages already printed in black
  if (!rstudioapi::isAvailable()) {
    return(x)
  }

  if (!rstudioapi::hasFun("getThemeInfo")) {
    return(x)
  }

  theme <- rstudioapi::getThemeInfo()

  if (isTRUE(theme$dark)) crayon::white(x) else crayon::black(x)
}

read_table_error <- function(e) {
  e <- as.character(e)
  msg <- c(
    sprintf("Table %s is not available.", get("tabla", envir = 1)),
    "\nVerify that you wrote the correct table name and that you downloaded",
    "\nthe datasets with tp_download()."
  )
  stop(msg, call. = FALSE)
}
