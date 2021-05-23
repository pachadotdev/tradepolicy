#' GLM Regression With Clustered Standard Errors
#'
#' Fits a regression with robust clustered standard errors. This uses a quasi-poisson
#' family and returns the estimated coefficients after computing a clustered
#' variance-covariance matrix.
#'
#' @param formula Formula for the model
#' @param data Tibble or data.frame
#' @param pair Inter-national fixed effects column (defaults to "pair_id")
#' @param fe_pattern Pattern for the fixed effects variable, allows character
#' or regex (defaults to "^exporter|^importer")
#' @return A coeftest
#' @export
tp_clustered_glm <- function(formula, data, pair = "pair_id", fe_pattern = "^exporter|^importer") {
  fit <- stats::glm(stats::as.formula(formula),
    family = stats::quasipoisson(link = "log"),
    data = data
  )

  vcov_cluster <- sandwich::vcovCL(
    fit,
    cluster = data[, pair]
  )

  vcov_cluster_reduced <- vcov_cluster[
    which(!grepl(fe_pattern, rownames(vcov_cluster))),
    which(!grepl(fe_pattern, rownames(vcov_cluster)))
  ]

  coef_test <- lmtest::coeftest(
    fit,
    vcov_cluster_reduced
  )

  return(coef_test)
}

#' Extract Fixed Effects From Regression Objects
#'
#' Takes an lm/glm object and extracts the fixed effects estimated coefficients.
#' This function was created to be used with `left_join()` and `predict()` as it
#' pastes the effects and allows to create a column with the predicted output.
#'
#' @param fit Regression object
#' @param fe_exp_pattern Pattern for the fixed effects exporter variable, allows character
#' or regex (defaults to \code{"^exporter|^exp_year"})
#' @param fe_imp_pattern Pattern for the fixed effects exporter variable, allows character
#' or regex (defaults to \code{"^importer|^imp_year"})
#' @param fe_pair_pattern Pattern for the fixed effects exporter variable, allows character
#' or regex (defaults to \code{"^pair_id_2"})
#' @param fe_time_pattern Pattern for the time-depending fixed effects, allows character
#' or regex (defaults to \code{"year"})
#' @return A tibble
#' @export
tp_fixed_effects <- function(fit, fe_exp_pattern = "^exporter|^exp_year",
                             fe_imp_pattern = "^importer|^imp_year",
                             fe_pair_pattern = "^pair_id_2",
                             fe_time_pattern = "year") {
  # exporter FE

  fe_exp_pattern_2 <- grep(fe_exp_pattern, names(fit$model), value = TRUE)
  if (length(fe_exp_pattern_2) > 1) {
    fe_exp_pattern_2 <- grep(fe_time_pattern, fe_exp_pattern_2, value = TRUE)
  }

  exp_coef <- tibble::enframe(
    fit$coefficients[grepl(fe_exp_pattern, names(fit$coefficients))]) %>%
    dplyr::mutate(name = gsub(fe_exp_pattern_2, "", name))

  colnames(exp_coef) <- c(fe_exp_pattern_2, paste0("fe_", fe_exp_pattern_2))

  # importer FE

  fe_imp_pattern_2 <- grep(fe_imp_pattern, names(fit$model), value = TRUE)
  if (length(fe_imp_pattern_2) > 1) {
    fe_imp_pattern_2 <- grep(fe_time_pattern, fe_imp_pattern_2, value = TRUE)
  }

  imp_coef <- tibble::enframe(
    fit$coefficients[grepl(fe_imp_pattern, names(fit$coefficients))]) %>%
    dplyr::mutate(name = gsub(fe_imp_pattern_2, "", name))

  colnames(imp_coef) <- c(fe_imp_pattern_2, paste0("fe_", fe_imp_pattern_2))

  # pair FE

  fe_pair_pattern_2 <- grep(fe_pair_pattern, names(fit$model), value = TRUE)

  if (length(fe_pair_pattern_2) > 0) {
    pair_coef <- tibble::enframe(fit$coefficients[grepl(fe_pair_pattern, names(fit$coefficients))]) %>%
      dplyr::mutate(name = gsub(fe_pair_pattern_2, "", name))

    colnames(pair_coef) <- c(fe_pair_pattern_2, paste0("fe_", fe_pair_pattern_2))
  }

  # combine FE

  if (length(fe_pair_pattern_2) > 0) {
    suppressMessages(
      d <- fit$data[, c(fe_exp_pattern_2, fe_imp_pattern_2, fe_pair_pattern_2)] %>%
        dplyr::left_join(exp_coef) %>%
        dplyr::left_join(imp_coef) %>%
        dplyr::left_join(pair_coef)
    )
  } else {
    suppressMessages(
      d <- fit$data[, c(fe_exp_pattern_2, fe_imp_pattern_2)] %>%
        dplyr::left_join(exp_coef) %>%
        dplyr::left_join(imp_coef)
    )
  }

  d <- dplyr::mutate_if(d, is.numeric, function(x) {
    ifelse(is.na(x), 0, x)
  })

  return(d)
}
