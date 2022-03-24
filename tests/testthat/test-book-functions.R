test_that("Functions from Ch.1 work as expected, part 1", {
  skip_on_cran()

  d_test <- agtpa_applications %>%
    filter(
      exporter == "ARG",
      importer !=  "ARG",
      year == 1994,
      trade > 0
    ) %>%
    mutate(
      log_trade = log(trade),
      log_dist = log(dist)
    )

  # THESE REGRESSIONS ARE JUST FOR TESTING !!

  summary1 <- tp_summary_app_1(
    formula = "log_trade ~ log_dist + cntg + lang + clny",
    data = d_test,
    method = "ols"
  )

  summary2 <- tp_summary_app_1(
    formula = "trade ~ log_dist + cntg + lang + clny",
    data = d_test,
    method = "ppml"
  )

  expect_is(summary1, "list")
  expect_is(summary2, "list")
})


test_that("Functions from Ch.1 work as expected, part 2", {
  skip_on_cran()

  d_test <- agtpa_applications %>%
    filter(
      exporter == "ARG",
      importer != "ARG",
      year >= 2002,
      trade > 0
    ) %>%
    mutate(
      exp_year = paste0(exporter, year),
      imp_year = paste0(importer, year),
      year = paste0("log_dist_", year),
      log_trade = log(trade),
      log_dist = log(dist),
      smctry = ifelse(importer != exporter, 0, 1),
      log_dist_intra = log_dist * smctry,
      intra_pair = ifelse(exporter == importer, exporter, "inter")
    ) %>%
    pivot_wider(names_from = year, values_from = log_dist, values_fill = 0) %>%
    mutate(across(log_dist_1986:log_dist_2006, function(x) x * (1 - smctry)))

  # THESE REGRESSIONS ARE JUST FOR TESTING !!

  summary3 <- tp_summary_app_2(
    formula = "log_trade ~ log_dist_2002 + log_dist_2006 + cntg + lang + clny | exp_year + imp_year",
    data = d_test,
    method = "ols"
  )

  summary4 <- tp_summary_app_2(
    formula = "trade ~ log_dist_2002 + log_dist_2006 + cntg + lang + clny | exp_year + imp_year",
    data = d_test,
    method = "ppml"
  )

  expect_is(summary3, "list")
  expect_is(summary4, "list")
})


test_that("Functions from Ch.1 work as expected, part 3", {
  skip_on_cran()

  d_test <- agtpa_applications %>%
    filter(
      exporter == "ARG",
      importer != "ARG",
      year >= 2002,
      trade > 0
    ) %>%
    mutate(
      exp_year = paste0(exporter, year),
      imp_year = paste0(importer, year),
      year = paste0("intl_border_", year),
      log_trade = log(trade),
      log_dist = log(dist),
      intl_brdr = ifelse(exporter == importer, pair_id, "inter"),
      intl_brdr_2 = ifelse(exporter == importer, 0, 1),
      pair_id_2 = ifelse(exporter == importer, "0-intra", pair_id)
    ) %>%
    spread(year, intl_brdr_2, fill = 0)

  # THESE REGRESSIONS ARE JUST FOR TESTING !!

  summary5 <- tp_summary_app_3(
    formula = "log_trade ~ log_dist + cntg + lang + clny + rta | exp_year + imp_year",
    data = d_test,
    method = "ols"
  )

  summary6 <- tp_summary_app_3(
    formula = "trade ~ log_dist + cntg + lang + clny + rta | exp_year + imp_year",
    data = d_test,
    method = "ppml"
  )

  expect_is(summary5, "list")
  expect_is(summary6, "list")
})
