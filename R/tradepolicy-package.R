globalVariables(c("country", "exporter", "importer", "name", "type", "value", "term"))

#' @keywords internal
"_PACKAGE"

#' @title International Trade Data for Application Exercises in AGTPA
#' @name agtpa_applications
#' @docType data
#' @author AGTPA authors, adapted from The World Development Index made by The World Bank
#' @format A data frame with 99,981 rows and 17 columns:
#' \describe{
#'   \item{exporter}{Exporter ISO country code}
#'   \item{importer}{Importer ISO country code}
#'   \item{pair_id}{Symmetric Pair ID}
#'   \item{year}{Year}
#'   \item{trade}{Nominal trade flows in current US dollars}
#'   \item{dist}{Population-weighted bilateral distance between country 'i' and 'j', in kilometers}
#'   \item{cntg}{Indicator. Equal to 1 if country 'i' and 'j' share a common border}
#'   \item{lang}{Indicator. Equal to 1 if country 'i' and 'j' speak the same official language}
#'   \item{clny}{Indicator. Equal to 1 if country 'i' and 'j' share a colonial relationship}
#'   \item{rta}{Indicator that is equal to one when country 'i' and 'j' are members of same Regional Trade Agreement}
#'   \item{rta_lag3}{3rd lag of RTA}
#'   \item{rta_lag4}{4th lag of RTA}
#'   \item{rta_lag6}{6th lag of RTA}
#'   \item{rta_lag8}{8th lag of RTA}
#'   \item{rta_lag9}{9th lag of RTA}
#'   \item{rta_lag12}{12th lag of RTA}
#'   \item{rta_lead4}{4th lead of RTA}
#' }
#' @description Contains bilateral trade flows for different pairs of countries
#' between 1986 and 2006 and additional variables modelling.
#' @details The data was drawn from the WDI for the year 2005 (earliest year
#' available), the countries with no entry cost data are mainly small probably
#' not in service trade data, and there are some considerations for the
#' countries in this dataset:
#' \itemize{
#'  \item{KOR designates RKO since 1949}
#'  \item{RUS designates SUN between 1949 and 1991}
#'  \item{CZE designates CZS between 1949 and 1992}
#'  \item{DEU designates FRG between 1949 and 1989}
#'  \item{Germany unified (DEU) has data since 1991}
#' }
#' @keywords data
NULL

# Suppress R CMD check note
#' @importFrom ggplot2 ggplot
#' @importFrom purrr map
#' @importFrom tidyr spread
NULL
