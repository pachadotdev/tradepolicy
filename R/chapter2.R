#' GLM Regression With Robust Clustered Standard Errors
#'
#' Computes clustered standard errors, tests on coefficients with
#' clustered standard errors and delta method for percent change in log
#'
#' @param formula A formula for the model
#' @param data A tibble or data.frame
#' @export

yotov_robust_glm <- function(formula, data) {
  pair <- "pair_id" # linking variable

  fit <- stats::glm(stats::as.formula(formula), family = stats::quasipoisson(link = "log"),
                    data = data)

  vcov_cluster <- multiwayvcov::cluster.vcov(
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

#' GLM Regression With Robust Clustered Standard Errors
#'
#' Computes clustered standard errors, tests on coefficients with
#' clustered standard errors and delta method for percent change in log
#'
#' @importFrom dplyr %>%
#' @param fit A regression object
#' @export

yotov_fixed_effects <- function(fit) {
  d <- tibble::enframe(fit$coefficients) %>%
    dplyr::filter(substr(name, 1, 4) %in% c("expo", 'impo')) %>%
    dplyr::mutate(name = gsub("ter", "ter ", name)) %>%
    tidyr::separate(col = "name", into = c("type", "country"), sep = " ") %>%
    tidyr::spread(type, value)

  d_exp <- d %>% dplyr::select(exporter = country, fe_exporter = exporter)
  d_imp <- d %>% dplyr::select(importer = country, fe_importer = importer)

  d2 <- tidyr::crossing(d_exp, d_imp) %>%
    dplyr::mutate_if(is.numeric, function(x) { ifelse(is.na(x), 0, x) })

  return(d2)
}
