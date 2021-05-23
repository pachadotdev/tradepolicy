# context("Clustered summary")
#
# test_that("Output has the expected contents for lm-objets", {
#   model1 <- lm(mpg ~ wt, data = mtcars)
#   summary1 <- tp_summary_clustered(model1, "cyl")
#   expect_is(summary1, "list")
#   expect_is(summary1$f_stat, "numeric")
#   expect_is(summary1$f_df, "numeric")
#   expect_is(summary1$prob_f, "numeric")
#   expect_is(summary1$r_sq, "numeric")
#   expect_is(summary1$root_mse, "numeric")
#   expect_is(summary1$coefficients, c("tbl_df", "tbl", "data.frame"))
#
#   expect_equal(length(summary1$n_obs), 1)
#   expect_equal(length(summary1$f_stat), 1)
#   expect_equal(length(summary1$f_df), 2)
#   expect_equal(length(summary1$prob_f), 1)
#   expect_equal(length(summary1$r_sq), 1)
#   expect_equal(length(summary1$root_mse), 1)
# })
#
# test_that("Output has the expected contents for glm-objets", {
#   model2 <- glm(mpg ~ wt, data = mtcars, family = quasipoisson)
#   summary2 <- tp_summary_clustered(model2, "cyl")
#   expect_is(summary2, "list")
#   expect_is(summary2$pseudo_r_sq, "numeric")
#   expect_is(summary2$coefficients, c("tbl_df", "tbl", "data.frame"))
#
#   expect_equal(length(summary2$n_obs), 1)
#   expect_equal(length(summary2$pseudo_r_sq), 1)
# })
