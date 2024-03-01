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
#'
#' @examples
#' # See the ebook
#'
#' @return A list
#' @export
tp_summary_app_1 <- function(formula, data, method = "ppml", pair = "pair_id",
                             etfe = "exp_year", itfe = "imp_year") {
  stopifnot(any(method %in% c("ols", "ppml")))

  formula <- as.formula(formula)

  if (!all(class(data) %in% "data.frame")) {
    data <- as.data.frame(data)
  }

  if (method == "ols") {
    fit <- feols(formula,
      data = data,
      cluster = data[, pair]
    )
  }

  if (method == "ppml") {
    fit <- fepois(formula,
      data = data,
      cluster = data[, pair]
    )
  }

  is_ppml <- any(fit$method %in% "fepois")

  # regardless of OLS or PPML, the base for the RESET test
  # is adapted from http://personal.lse.ac.uk/tenreyro/reset.do
  # we can't use lmtest with fixest

  # For PPML get fitted values of the linear index, not of trade
  data$predict2 <- (predict(fit, type = "link"))^2

  # Assuming form_reset is constructed up to this point as a character string
  form_reset <- paste0(formula[2], " ~ ", formula[3], " + predict2")

  # For fixed effects, if applicable
  if (length(as.character(fit$fml_all$fixef)) > 0) {
    form_reset <- paste0(form_reset, " | ", as.character(fit$fml_all$fixef)[2])
  }

  # Convert the character string back into a formula
  form_reset <- as.formula(form_reset)

  if (!is_ppml) {
    fit_reset <- feols(form_reset,
      data = data,
      cluster = data[, pair]
    )
  } else {
    fit_reset <- fepois(form_reset,
      data = data,
      cluster = data[, pair]
    )
  }

  res <- ifelse(method == "ols", fit_reset$coeftable["predict2", "Pr(>|t|)"],
    fit_reset$coeftable["predict2", "Pr(>|z|)"]
  )

  rsq <- if (is_ppml) {
    # Also adapted from http://personal.lse.ac.uk/tenreyro/r2.do
    actual <- as.numeric(data$trade)
    predicted <- as.numeric(fit$fitted.values)
    (cor(actual, predicted, method = "kendall"))^2 # kendall mimics stata
  } else {
    r2(fit, "r2")
  }

  structure(
    list(
      tidy_coefficients = tidy(fit) %>% mutate_if(is.numeric, function(x) round(x, 3)),
      nobs = nrow(data),
      rsquared = round(rsq, 3),
      etfe = any(grepl(paste0("^", etfe), fit$fixef_vars)),
      itfe = any(grepl(paste0("^", itfe), fit$fixef_vars)),
      reset_pval = round(res, 3)
    ),
    class = "tp_summary_app_1"
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
#'
#' @examples
#' # See the ebook
#'
#' @return A list
#' @export
tp_summary_app_2 <- function(formula, data, method = "ppml",
                             pair = "pair_id", etfe = "exp_year",
                             itfe = "imp_year", dist = "log_dist",
                             intr = "log_dist_intra", csfe = "intra_pair") {
  stopifnot(any(method %in% c("ols", "ppml")))

  formula <- as.Formula(formula)

  if (!all(class(data) %in% "data.frame")) {
    data <- as.data.frame(data)
  }

  if (method == "ols") {
    fit <- feols(formula,
      data = data,
      cluster = data[, pair]
    )
  }

  if (method == "ppml") {
    fit <- fepois(formula,
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

  beta_vcov_cluster <- vcovCL(fit, cluster = data[, pair])
  beta_vcov_cluster <- beta_vcov_cluster[
    which(grepl(paste(beta_log_dist, collapse = "|"), rownames(beta_vcov_cluster))),
    which(grepl(paste(beta_log_dist, collapse = "|"), rownames(beta_vcov_cluster)))
  ]

  beta_std_err <- deltamethod(
    ~ 100 * (x2 - x1) / x1,
    c(beta1, beta2), beta_vcov_cluster
  )

  beta_tstat <- beta_pct_chg / beta_std_err
  beta_pval <- pnorm(-abs(beta_tstat)) + (1 - pnorm(abs(beta_tstat)))

  structure(
    list(
      tidy_coefficients = tidy(fit) %>% mutate_if(is.numeric, function(x) round(x, 3)),
      nobs = nrow(data),
      pct_chg_log_dist = round(beta_pct_chg, 3),
      pcld_std_err = round(beta_std_err, 3),
      pcld_std_err_pval = round(beta_pval, 3),
      intr = any(grepl(paste0("^", intr, "|^", csfe), names(fit$coefficients))),
      csfe = any(grepl(paste0("^", csfe), names(fit$coefficients)))
    ),
    class = "tp_summary_app_2"
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
#'
#' @examples
#' # See the ebook
#'
#' @return A list
#' @export
tp_summary_app_3 <- function(formula, data, method = "ppml",
                             pair = "pair_id", pair2 = "pair_id_2",
                             etfe = "exp_year", itfe = "imp_year",
                             dist = "log_dist", intr = "log_dist_intra",
                             brdr = "intl_brdr") {
  stopifnot(any(method %in% c("ols", "ppml")))

  formula <- as.Formula(formula)

  if (!all(class(data) %in% "data.frame")) {
    data <- as.data.frame(data)
  }

  if (method == "ols") {
    fit <- feols(
      formula,
      data = data,
      cluster = data[, pair]
    )
  }

  if (method == "ppml") {
    fit <- fepois(formula,
      data = data,
      cluster = data[, pair]
    )
  }

  contains_intr <- any(grepl(paste0("^", intr, "|^", brdr, "|^", pair2), names(fit$coefficients)))

  beta_rta <- fit$coefficients[grepl("^rta", names(fit$coefficients))]

  if (length(beta_rta) > 0) {
    beta_vcov_cluster <- vcovCL(fit, cluster = data[, pair])

    beta_vcov_cluster <- beta_vcov_cluster[
      which(grepl(paste(names(beta_rta), collapse = "|"), rownames(beta_vcov_cluster))),
      which(grepl(paste(names(beta_rta), collapse = "|"), rownames(beta_vcov_cluster)))
    ]

    beta_sum <- sum(beta_rta)

    beta_form <- paste(paste0("x", seq_along(beta_rta)), collapse = "+")
    beta_form <- paste0("~", beta_form)

    beta_std_err <- deltamethod(as.formula(beta_form), beta_rta, beta_vcov_cluster)

    beta_tstat <- beta_sum / beta_std_err
    beta_pval <- pnorm(-abs(beta_tstat)) + (1 - pnorm(abs(beta_tstat)))

    out <- list(
      tidy_coefficients = tidy(fit) %>% mutate_if(is.numeric, function(x) round(x, 3)),
      nobs = nrow(data),
      total_rta_effect = round(beta_sum, 3),
      trta_std_err = round(beta_std_err, 3),
      trta_std_err_pval = round(beta_pval, 3),
      intr = contains_intr
    )
  } else {
    out <- list(
      tidy_coefficients = tidy(fit) %>% mutate_if(is.numeric, function(x) round(x, 3)),
      nobs = nrow(data),
      intr = contains_intr
    )
  }

  structure(out, class = "tp_summary_app_3")
}

# this method uses kable to present everything in a single table
#' @export
#' @noRd
print.tp_summary_app_1 <- function(x, ...) {
  d1 <- kable(x$tidy_coefficients)

  d2 <- kable(tibble(
    nobs = x$nobs,
    rsquared = x$rsquared,
    etfe = x$etfe,
    itfe = x$itfe,
    reset_pval = x$reset_pval
  ))

  cat(d1, "\n", d2, sep = "\n")
}

#' @export
#' @noRd
print.tp_summary_app_2 <- function(x, ...) {
  d1 <- kable(
    x$tidy_coefficients %>%
      # filter obs in term that start with intra_pair
      filter(!grepl("^intra_pair", term))
  )

  d2 <- kable(tibble(
    nobs = x$nobs,
    pct_chg_log_dist = x$pct_chg_log_dist,
    pcld_std_err = x$pcld_std_err,
    pcld_std_err_pval = x$pcld_std_err_pval,
    intr = x$intr,
    csfe = x$csfe
  ))

  cat(d1, "\n", d2, sep = "\n")
}

#' @export
#' @noRd
print.tp_summary_app_3 <- function(x, ...) {
  d1 <- kable(x$tidy_coefficients)

  d2 <- kable(tibble(
    nobs = x$nobs,
    total_rta_effect = x$total_rta_effect,
    trta_std_err = x$trta_std_err,
    trta_std_err_pval = x$trta_std_err_pval,
    intr = x$intr
  ))

  cat(d1, "\n", d2, sep = "\n")
}
