#' GLM Regression With Robust Clustered Standard Errors
#'
#' Fits a regression with robust clustered standard errors. This uses a quasi-poisson
#' family and returns the estimated coefficients after computing a clustered
#' variance-covariance matrix.
#'
#' @param formula A formula for the model
#' @param data A tibble or data.frame
#' @export

yotov_robust_glm <- function(formula, data) {
  pair <- "pair_id" # linking variable

  fit <- stats::glm(stats::as.formula(formula), family = stats::quasipoisson(link = "log"),
                    data = data)

  vcov_cluster <- sandwich::vcovCL(
    fit,
    cluster = data[, pair],
    df_correction = TRUE
  )

  vcov_cluster_reduced <- vcov_cluster[
    which(!grepl("^exporter|^importer", rownames(vcov_cluster))),
    which(!grepl("^exporter|^importer", rownames(vcov_cluster)))
  ]

  coef_test <- lmtest::coeftest(
    fit,
    vcov_cluster_reduced
  )

  return(coef_test)
}

#' Extract fixed effects from regression object
#'
#' Takes an lm/glm object and extracts the fixed effects estimated coefficients.
#' This function was created to be used with `left_join()` and `predict()` as it
#' pastes the effects and allows to create a column with the predicted output.
#'
#' @importFrom dplyr %>%
#' @param fit A regression object
#' @export

yotov_fixed_effects <- function(fit) {
  panel_coef <- any(grepl("^exp_year|^imp_year|^pair_id_2", names(fit$coefficients)))

  if (panel_coef) {
    exp_coef <- fit$coefficients[grepl("^exp_", names(fit$coefficients))]
    exp_coef <- tibble::enframe(exp_coef) %>%
      dplyr::mutate(name = gsub("exp_year", "", name)) %>%
      dplyr::rename(exp_year = name, fe_exp_year = value)

    imp_coef <- fit$coefficients[grepl("^imp_", names(fit$coefficients))]
    imp_coef <- tibble::enframe(imp_coef) %>%
      dplyr::mutate(name = gsub("imp_year", "", name)) %>%
      dplyr::rename(imp_year = name, fe_imp_year = value)

    pair_coef <- fit$coefficients[grepl("^pair_id_2", names(fit$coefficients))]
    pair_coef <- tibble::enframe(pair_coef) %>%
      dplyr::mutate(name = gsub("pair_id_2", "", name)) %>%
      dplyr::rename(pair_id_2 = name, fe_pair_id_2 = value)

    d <- fit$data[, c("exp_year", "imp_year", "pair_id_2")]

    suppressMessages(
      d <- d %>%
        dplyr::left_join(exp_coef) %>%
        dplyr::left_join(imp_coef) %>%
        dplyr::left_join(pair_coef)
    )
  } else {
    exp_coef <- fit$coefficients[grepl("^exporter", names(fit$coefficients))]
    exp_coef <- tibble::enframe(exp_coef) %>%
      dplyr::mutate(name = gsub("exporter", "", name)) %>%
      dplyr::rename(exporter = name, fe_exporter = value)

    imp_coef <- fit$coefficients[grepl("^importer", names(fit$coefficients))]
    imp_coef <- tibble::enframe(imp_coef) %>%
      dplyr::mutate(name = gsub("importer", "", name)) %>%
      dplyr::rename(importer = name, fe_importer = value)

    d <- fit$data[, c("exporter", "importer")]

    suppressMessages(
      d <- d %>%
        dplyr::left_join(exp_coef) %>%
        dplyr::left_join(imp_coef)
    )
  }

  d <- dplyr::mutate_if(d, is.numeric, function(x) { ifelse(is.na(x), 0, x) })

  return(d)
}
