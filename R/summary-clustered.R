#' Stata-Like Clustered Standard Errors Summary
#'
#' Returns a list for a a general \code{lm} or \code{glm} which returns number
#' of observations, F-statistic (with degrees of freedom and p-value), R-squared
#' (and pseudo R-squared for Poisson-type generalized models), root MSE and clustered
#' standard errors for estimated coefficients.
#'
#' @param model \code{lm} or \code{glm} object
#' @param cluster Clustering variable in the model data (e.g.
#' \code{"pair_id"})
#' @examples
#' # These regressions constitute a dummy example
#' model1 <- lm(mpg ~ wt, data = mtcars)
#' tp_summary_clustered(model1, "cyl")
#'
#' model2 <- glm(mpg ~ wt, data = mtcars, family = quasipoisson)
#' tp_summary_clustered(model2, "cyl")
#' @return A list
#' @export
tp_summary_clustered <- function(model, cluster) {
  # Check ----
  stopifnot(any(class(model) %in% c("lm", "glm")))
  if (any(class(model) == "glm") & !any(grepl("poisson", model$call))) {
    warning("The input model is not of (quasi)poisson-type, the pseudo R-squared couldn't be obtained.")
  }

  # Call the data and pass it as a data.frame ----
  d <- eval(model$call$data)
  if (!all(class(d) %in% "data.frame")) {
    d <- as.data.frame(d)
  }

  # Obtain clustered variance-covariance matrix ----
  # The correction results in multiplying the matrix by a factor F
  # so that F = (M / (M-1)) * ((N-1) / (N-K))
  # where
  # M = number of unique cluster IDs
  # N = number of rows
  # K = number of parameters in the model
  vcov_cluster <- sandwich::vcovCL(
    model,
    cluster = d[, cluster]
  )

  # Pseudo-R2 for PPML models ----
  if (any(class(model) == "glm")) {
    if (any(grepl("poisson", model$call))) {
      # r2: http://personal.lse.ac.uk/tenreyro/
      # here we extract the depending variable as a vector from the model itself
      actual <- as.numeric(d[, as.character(model$call[[2]][[2]])])
      # not we convert the prediction to vector class
      predicted <- as.numeric(model$fitted.values)
      # kendall mimics stata computation
      r_squared <- (stats::cor(actual, predicted, method = "kendall"))^2
    } else {
      r_squared <- NA
    }
  } else {
    r_squared <- summary(model)$r.squared
  }
  r_squared <- round(r_squared, 4)

  # Test on model coefficients ----
  coef_test <- broom::tidy(lmtest::coeftest(model, vcov_cluster))

  if (all(class(model) == "lm")) {
    # Obtain Stata-like F-statistic from the model ----
    wald_test <- as.data.frame(
      broom::tidy(lmtest::waldtest(model, vcov = vcov_cluster, test = "F"))
    )
    fs <- round(wald_test$statistic[2], 2)
    fp <- round(wald_test$p.value[2], 4)
    df1 <- length(model$coefficients) - 1 # df1 for F-statistic
    df2 <- length(unique(d[, cluster])) - 1 #  df2 for F-statistic

    # Obtain Root MSE ----
    rss <- as.numeric(crossprod(model$residuals))
    rmse <- round(sqrt(rss / length(model$residuals)), 3)

    # Create output list for lm objects ----
    summary_output <- list(
      n_obs = nrow(d),
      f_stat = fs,
      f_df = c(df1, df2),
      prob_f = fp,
      r_sq = r_squared,
      root_mse = rmse,
      coefficients = coef_test
    )
  } else {
    summary_output <- list(
      n_obs = nrow(d),
      pseudo_r_sq = r_squared,
      coefficients = coef_test
    )
  }

  # Output ------------------------------------------------------------------
  return(summary_output)
}
