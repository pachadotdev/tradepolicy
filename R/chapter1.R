#' Application 1 (Traditional Gravity Estimates) Reporting Style
#'
#' Computes clustered standard errors, tests on coefficients with
#' clustered standard errors and obtains RESET test p-value.
#'
#' @param formula Formula for the model
#' @param data Tibble or data.frame
#' @param method Regression method, which can be "ols" or "ppml" (default)
#' @param pair Inter-national fixed effects column (defaults to "pair_id")
#' @param etfe Exporter time fixed effects column (defaults to "exp_year")
#' @param itfe Importer time fixed effects column (defaults to "imp_year")
#' @return A list
#' @export

tp_summary_app_1 <- function(formula, data, method = "ppml", pair = "pair_id",
                            etfe = "exp_year", itfe = "imp_year") {
  stopifnot(any(method %in% c("ols", "ppml")))

  if (!all(class(data) %in% "data.frame")) {
    data <- as.data.frame(data)
  }

  if (method == "ols") {
    fit <- fixest::feols(stats::as.formula(formula),
                         data = data,
                         cluster = data[, pair])
  }

  if (method == "ppml") {
    fit <- fixest::fepois(stats::as.formula(formula),
      data = data,
      cluster = data[, pair])
  }

  is_ppml <- any(fit$method %in% "fepois")

  # regardless of OLS or PPML, the base for the RESET test
  # is adapted from http://personal.lse.ac.uk/tenreyro/reset.do
  # we can't use lmtest with fixest

  # For PPML get fitted values of the linear index, not of trade
  data$predict2 <- (stats::predict(fit, type = "link"))^2

  form_reset <- as.character(stats::update(fit$fml_all$linear, ~ predict2 + .))

  if (length(as.character(fit$fml_all$fixef)) > 0) {
    form_reset <- paste0(form_reset[2], " ~ ", form_reset[3], " | ",  as.character(fit$fml_all$fixef)[2])
  } else {
    form_reset <- paste0(form_reset[2], " ~ ", form_reset[3])
  }

  if (!is_ppml) {
    fit_reset <- fixest::feols(stats::as.formula(form_reset),
                               data = data,
                               cluster = data[, pair])
  } else {
    fit_reset <- fixest::fepois(stats::as.formula(form_reset),
                                data = data,
                                cluster = data[, pair])
  }

  res <- fit_reset$coeftable["predict2", "Pr(>|t|)"]

  r2 <- if (is_ppml) {
    # Also adapted from http://personal.lse.ac.uk/tenreyro/r2.do
    actual <- as.numeric(data$trade)
    predicted <- as.numeric(fit$fitted.values)
    (stats::cor(actual, predicted, method = "kendall"))^2 # kendall mimics stata
  } else {
    fixest::r2(fit, "r2")
  }

  return(
    list(
      tidy_coefficients = broom::tidy(fit),
      nobs = nrow(data),
      rsquared = r2,
      etfe = any(grepl(paste0("^", etfe), fit$fixef_vars)),
      itfe = any(grepl(paste0("^", itfe), fit$fixef_vars)),
      reset_pval = res
    )
  )
}

#' Application 2 (The "Distance Puzzle" Resolved) Reporting Style
#'
#' Computes clustered standard errors, tests on coefficients with
#' clustered standard errors and uses the delta method to obtain changes in
#' time-based distance estimated coefficients.
#'
#' @param formula Formula for the model
#' @param data Tibble or data.frame
#' @param method Regression method (lm or glm)
#' @param pair Inter-national fixed effects column (defaults to "pair_id")
#' @param etfe Exporter time fixed effects column (defaults to "exp_year")
#' @param itfe Importer time fixed effects column (defaults to "imp_year")
#' @param dist Distance column (defaults to "log_dist")
#' @param intr Intra-national distance column (defaults to "log_dist_intra")
#' @param csfe Country-specific fixed effects (defaults to "intra_pair")
#' @return A list
#' @export
tp_summary_app_2 <- function(formula, data, method = "ppml",
                                 pair = "pair_id", etfe = "exp_year",
                                 itfe = "imp_year", dist = "log_dist",
                                 intr = "log_dist_intra", csfe = "intra_pair") {
  stopifnot(any(method %in% c("ols", "ppml")))

  if (!all(class(data) %in% "data.frame")) {
    data <- as.data.frame(data)
  }

  if (method == "ols") {
    fit <- fixest::feols(stats::as.formula(formula), data = data)
  }

  if (method == "ppml") {
    fit <- fixest::fepois(stats::as.formula(formula),
      data = data,
      cluster = data[, pair]
    )
  }

  beta_log_dist <- grep(intr,
    grep(dist, names(fit$coefficients), value = TRUE),
    value = TRUE, invert = TRUE
  )
  beta_log_dist <- c(min(beta_log_dist), max(beta_log_dist))

  # change = 100 * (beta2 - beta1) / beta1
  beta1 <- fit$coefficients[min(beta_log_dist)]
  beta2 <- fit$coefficients[max(beta_log_dist)]

  beta_pct_chg <- as.numeric(100 * (beta2 - beta1) / beta1)

  beta_vcov_cluster <- sandwich::vcovCL(
    fit,
    cluster = data[, pair]
  )
  beta_vcov_cluster <- beta_vcov_cluster[
    which(grepl(paste(beta_log_dist, collapse = "|"), rownames(beta_vcov_cluster))),
    which(grepl(paste(beta_log_dist, collapse = "|"), rownames(beta_vcov_cluster)))
  ]

  beta_std_err <- msm::deltamethod(
    ~ 100 * (x2 - x1) / x1,
    c(beta1, beta2), beta_vcov_cluster
  )

  beta_tstat <- beta_pct_chg / beta_std_err
  beta_pval <- stats::pnorm(-abs(beta_tstat)) + (1 - stats::pnorm(abs(beta_tstat)))

  return(
    list(
      tidy_coefficients = broom::tidy(fit),
      nobs = nrow(data),
      pct_chg_log_dist = beta_pct_chg,
      pcld_std_err = beta_std_err,
      pcld_std_err_pval = beta_pval,
      intr = any(grepl(paste0("^", intr, "|^", csfe), names(fit$coefficients))),
      csfe = any(grepl(paste0("^", csfe), names(fit$coefficients)))
    )
  )
}

#' Application 3 (Regional Trade Agreements Effects) Reporting Style
#'
#' Computes clustered standard errors, tests on coefficients with
#' clustered standard errors and returns total RTAs effect with its associated
#' standard error.
#'
#' @param formula Formula for the model
#' @param data Tibble or data.frame
#' @param method Regression method, which can be "ols" or "ppml" (default)
#' @param pair Inter-national fixed effects column (defaults to "pair_id")
#' @param pair2 Intra-national fixed effects column (defaults to "pair_id_2")
#' @param etfe Exporter time fixed effects column (defaults to "exp_year")
#' @param itfe Importer time fixed effects column (defaults to "imp_year")
#' @param dist Distance column (defaults to "log_dist")
#' @param intr Intra-national distance column (defaults to "log_dist_intra")
#' @param brdr Inter-national borders column (defaults to "intl_brdr")
#' @return A list
#' @export
tp_summary_app_3 <- function(formula, data, method = "ppml",
                                 pair = "pair_id", pair2 = "pair_id_2",
                                 etfe = "exp_year", itfe = "imp_year",
                                 dist = "log_dist", intr = "log_dist_intra",
                                 brdr = "intl_brdr") {
  stopifnot(any(method %in% c("ols", "ppml")))

  if (!all(class(data) %in% "data.frame")) {
    data <- as.data.frame(data)
  }

  if (method == "ols") {
    fit <- fixest::feols(
      stats::as.formula(formula),
      data = data,
      cluster = data[, pair])
  }

  if (method == "ppml") {
    fit <- fixest::fepois(stats::as.formula(formula),
      data = data,
      cluster = data[, pair])
  }

  contains_intr <- any(grepl(paste0("^", intr, "|^", brdr, "|^", pair2), names(fit$coefficients)))

  beta_rta <- fit$coefficients[grepl("^rta", names(fit$coefficients))]

  if (length(beta_rta) > 0) {
    beta_vcov_cluster <- sandwich::vcovCL(
      fit,
      cluster = data[, pair]
    )

    beta_vcov_cluster <- beta_vcov_cluster[
      which(grepl(paste(names(beta_rta), collapse = "|"), rownames(beta_vcov_cluster))),
      which(grepl(paste(names(beta_rta), collapse = "|"), rownames(beta_vcov_cluster)))
    ]

    beta_sum <- sum(beta_rta)

    beta_form <- paste(paste0("x", seq_along(beta_rta)), collapse = "+")
    beta_form <- paste0("~", beta_form)

    beta_std_err <- msm::deltamethod(stats::as.formula(beta_form), beta_rta, beta_vcov_cluster)

    beta_tstat <- beta_sum / beta_std_err
    beta_pval <- stats::pnorm(-abs(beta_tstat)) + (1 - stats::pnorm(abs(beta_tstat)))

    return(
      list(
        tidy_coefficients = broom::tidy(fit),
        nobs = nrow(data),
        total_rta_effect = beta_sum,
        trta_std_err = beta_std_err,
        trta_std_err_pval = beta_pval,
        intr = contains_intr
      )
    )
  } else {
    return(
      list(
        tidy_coefficients = broom::tidy(fit),
        nobs = nrow(data),
        intr = contains_intr
      )
    )
  }
}
