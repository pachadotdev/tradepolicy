#' Application 1 (Traditional Gravity Estimates) Reporting Style
#'
#' Computes clustered standard errors, tests on coefficients with
#' clustered standard errors and obtains RESET test p-value.
#'
#' @param formula Formula for the model
#' @param data Tibble or data.frame
#' @param method Regression method, which can be "lm" (default) or "glm"
#' @param pair Inter-national fixed effects column (defaults to "pair_id")
#' @param etfe Exporter time fixed effects column (defaults to "exp_year")
#' @param itfe Importer time fixed effects column (defaults to "imp_year")
#' @return A list
#' @export

tp_summary_app1 <- function(formula, data, method = "lm", pair = "pair_id",
                                etfe = "exp_year", itfe = "imp_year") {
  stopifnot(any(method %in% c("lm", "glm")))

  if (!all(class(data) %in% "data.frame")) {
    data <- as.data.frame(data)
  }

  if (method == "lm") {
    fit <- stats::lm(stats::as.formula(formula), data = data)
  }
  if (method == "glm") {
    fit <- stats::glm(stats::as.formula(formula),
      family = stats::quasipoisson(link = "log"),
      data = data,
      y = FALSE,
      model = FALSE
    )
  }

  is_ppml <- any(class(fit) %in% "glm")

  contains_etfe <- any(grepl(paste0("^", etfe), names(fit$coefficients)))
  contains_itfe <- any(grepl(paste0("^", itfe), names(fit$coefficients)))

  vcov_cluster <- sandwich::vcovCL(
    fit,
    cluster = data[, pair]
  )

  coef_test <- lmtest::coeftest(
    fit,
    vcov_cluster[
      which(!grepl(paste0("^", etfe, "|^", itfe), rownames(vcov_cluster))),
      which(!grepl(paste0("^", etfe, "|^", itfe), rownames(vcov_cluster)))
    ]
  )

  coef_test <- broom::tidy(coef_test)

  if (is_ppml) {
    # reset test: http://personal.lse.ac.uk/tenreyro/reset.do
    data$predict2 <- (stats::predict(fit))^2 # Get fitted values of the linear index, not of trade
    form_reset <- stats::update(fit$formula, ~ predict2 + .)
    fit_reset <- stats::glm(form_reset,
      family = stats::quasipoisson(link = "log"),
      data = data,
      y = FALSE,
      model = FALSE
    )
    vcov_cluster_reset <- sandwich::vcovCL(
      fit_reset,
      cluster = data[, pair]
    )
    res <- lmtest::coeftest(fit_reset, vcov_cluster_reset)
    res <- res[2, 4]

    # r2: http://personal.lse.ac.uk/tenreyro/r2.do
    actual <- as.numeric(data$trade)
    predicted <- as.numeric(fit$fitted.values)
    r2 <- (stats::cor(actual, predicted, method = "kendall"))^2 # kendall mimics stata
  } else {
    res <- lmtest::resettest(fit, power = 2)$p.value
  }

  return(
    list(
      tidy_coefficients = coef_test,
      nobs = nrow(data),
      rsquared = ifelse(is_ppml, r2, summary(fit)$r.squared),
      etfe = contains_etfe,
      itfe = contains_itfe,
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
tp_summary_app2 <- function(formula, data, method = "lm",
                                 pair = "pair_id", etfe = "exp_year",
                                 itfe = "imp_year", dist = "log_dist",
                                 intr = "log_dist_intra", csfe = "intra_pair") {
  stopifnot(any(method %in% c("lm", "glm")))

  if (!all(class(data) %in% "data.frame")) {
    data <- as.data.frame(data)
  }

  if (method == "lm") {
    fit <- stats::lm(stats::as.formula(formula), data = data)
  }
  if (method == "glm") {
    fit <- stats::glm(stats::as.formula(formula),
      family = stats::quasipoisson(link = "log"),
      data = data,
      y = FALSE,
      model = FALSE
    )
  }

  contains_intr <- any(grepl(paste0("^", intr, "|^", csfe), names(fit$coefficients)))
  contains_csfe <- any(grepl(paste0("^", csfe), names(fit$coefficients)))

  vcov_cluster <- sandwich::vcovCL(
    fit,
    cluster = data[, pair]
  )

  coef_test <- lmtest::coeftest(
    fit,
    vcov_cluster[
      which(!grepl(paste0("^", etfe, "|^", itfe, "|^", csfe), rownames(vcov_cluster))),
      which(!grepl(paste0("^", etfe, "|^", itfe, "|^", csfe), rownames(vcov_cluster)))
    ]
  )

  coef_test <- broom::tidy(coef_test)

  beta_log_dist <- grep(intr,
    grep(dist, coef_test$term, value = TRUE),
    value = TRUE, invert = TRUE
  )
  beta_log_dist <- c(min(beta_log_dist), max(beta_log_dist))

  # change = 100 * (beta2 - beta1) / beta1
  beta1 <- fit$coefficients[min(beta_log_dist)]
  beta2 <- fit$coefficients[max(beta_log_dist)]

  beta_vcov_cluster <- vcov_cluster[
    which(grepl(paste(beta_log_dist, collapse = "|"), rownames(vcov_cluster))),
    which(grepl(paste(beta_log_dist, collapse = "|"), rownames(vcov_cluster)))
  ]

  beta_pct_chg <- as.numeric(100 * (beta2 - beta1) / beta1)

  beta_std_err <- msm::deltamethod(
    ~ 100 * (x2 - x1) / x1,
    c(beta1, beta2), beta_vcov_cluster
  )

  beta_tstat <- beta_pct_chg / beta_std_err
  beta_pval <- stats::pnorm(-abs(beta_tstat)) + (1 - stats::pnorm(abs(beta_tstat)))

  return(
    list(
      tidy_coefficients = coef_test,
      nobs = nrow(data),
      pct_chg_log_dist = beta_pct_chg,
      pcld_std_err = beta_std_err,
      pcld_std_err_pval = beta_pval,
      intr = contains_intr,
      csfe = contains_csfe
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
#' @param method Regression method, which can be "lm" (default) or "glm"
#' @param pair Inter-national fixed effects column (defaults to "pair_id")
#' @param pair2 Intra-national fixed effects column (defaults to "pair_id_2")
#' @param etfe Exporter time fixed effects column (defaults to "exp_year")
#' @param itfe Importer time fixed effects column (defaults to "imp_year")
#' @param dist Distance column (defaults to "log_dist")
#' @param intr Intra-national distance column (defaults to "log_dist_intra")
#' @param brdr Inter-national borders column (defaults to "intl_brdr")
#' @return A list
#' @export
tp_summary_app3 <- function(formula, data, method = "lm",
                                 pair = "pair_id", pair2 = "pair_id_2",
                                 etfe = "exp_year", itfe = "imp_year",
                                 dist = "log_dist", intr = "log_dist_intra",
                                 brdr = "intl_brdr") {
  stopifnot(any(method %in% c("lm", "glm")))

  if (!all(class(data) %in% "data.frame")) {
    data <- as.data.frame(data)
  }

  if (method == "lm") {
    fit <- stats::lm(stats::as.formula(formula), data = data)
  }
  if (method == "glm") {
    fit <- stats::glm(stats::as.formula(formula),
      family = stats::quasipoisson(link = "log"),
      data = data,
      y = FALSE,
      model = FALSE
    )
  }

  contains_intr <- any(grepl(
    paste0("^", intr, "|^", brdr, "|^", pair2),
    names(fit$coefficients)
  ))

  vcov_cluster <- sandwich::vcovCL(
    fit,
    cluster = data[, pair]
  )

  vcov_cluster_reduced <- vcov_cluster[
    which(!grepl(
      paste0("^", etfe, "|^", itfe, "|^", brdr, "|^", pair2),
      rownames(vcov_cluster)
    )),
    which(!grepl(
      paste0("^", etfe, "|^", itfe, "|^", brdr, "|^", pair2),
      rownames(vcov_cluster)
    ))
  ]

  if (!is.null(dim(vcov_cluster_reduced))) {
    coef_test <- lmtest::coeftest(
      fit,
      vcov_cluster_reduced
    )

    coef_test <- broom::tidy(coef_test)
  } else {
    coef_test <- broom::tidy(fit) %>%
      dplyr::filter(
        !grepl(
          paste0("^", etfe, "|^", itfe, "|^", brdr, "|^", pair2),
          term
        )
      )
  }

  beta_rta <- fit$coefficients[grepl("^rta", names(fit$coefficients))]

  if (length(beta_rta) > 0) {
    beta_vcov_cluster <- vcov_cluster[
      which(grepl(paste(names(beta_rta), collapse = "|"), rownames(vcov_cluster))),
      which(grepl(paste(names(beta_rta), collapse = "|"), rownames(vcov_cluster)))
    ]

    beta_sum <- sum(beta_rta)

    beta_form <- paste(paste0("x", seq_along(beta_rta)), collapse = "+")
    beta_form <- paste0("~", beta_form)

    beta_std_err <- msm::deltamethod(stats::as.formula(beta_form), beta_rta, beta_vcov_cluster)

    beta_tstat <- beta_sum / beta_std_err
    beta_pval <- stats::pnorm(-abs(beta_tstat)) + (1 - stats::pnorm(abs(beta_tstat)))

    return(
      list(
        tidy_coefficients = coef_test,
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
        tidy_coefficients = coef_test,
        nobs = nrow(data),
        intr = contains_intr
      )
    )
  }
}
