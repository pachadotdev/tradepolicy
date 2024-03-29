% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summaries.R
\name{tp_summary_app_3}
\alias{tp_summary_app_3}
\title{Application 3 (Regional Trade Agreements Effects) Reporting Style}
\usage{
tp_summary_app_3(
  formula,
  data,
  method = "ppml",
  pair = "pair_id",
  pair2 = "pair_id_2",
  etfe = "exp_year",
  itfe = "imp_year",
  dist = "log_dist",
  intr = "log_dist_intra",
  brdr = "intl_brdr"
)
}
\arguments{
\item{formula}{Formula for the model}

\item{data}{Tibble or data.frame}

\item{method}{Regression method, which can be "ols" or "ppml" (default)}

\item{pair}{Inter-national fixed effects column (defaults to "pair_id")}

\item{pair2}{Intra-national fixed effects column (defaults to "pair_id_2")}

\item{etfe}{Exporter time fixed effects column (defaults to "exp_year")}

\item{itfe}{Importer time fixed effects column (defaults to "imp_year")}

\item{dist}{Distance column (defaults to "log_dist")}

\item{intr}{Intra-national distance column (defaults to "log_dist_intra")}

\item{brdr}{Inter-national borders column (defaults to "intl_brdr")}
}
\value{
A list
}
\description{
Computes clustered standard errors, tests on coefficients with
clustered standard errors and returns total RTAs effect with its associated
standard error.
}
\examples{
# See the ebook

}
