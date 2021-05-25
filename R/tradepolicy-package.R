globalVariables(c("country", "exporter", "importer", "name", "type", "value", "term"))

#' @keywords internal
"_PACKAGE"

#' @title International Trade Data for Application Exercises in AGTPA
#' @name agtpa_applications
#' @docType data
#' @author United Nations Conference on Trade and Development (UNCTAD)
#' @format A data frame object of 99,981 x 9
#' @description Contains bilateral trade flows for different pairs of countries
#' between 1986 and 2006 and additional variables modelling. See the details
#' in Yotov, et al (2016) <isbn:978-92-870-4367-2>.
#' @keywords data
NULL

# Suppress R CMD check note
#' @importFrom ggplot2 ggplot
#' @importFrom purrr map
#' @importFrom tidyr spread
NULL
