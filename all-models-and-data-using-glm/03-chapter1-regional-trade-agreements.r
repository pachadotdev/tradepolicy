library(tradepolicy)

# data ----

ch1_application3 <- agtpa_applications %>%
  filter(year %in% seq(1986, 2006, 4)) %>%
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

ch1_application3 <- ch1_application3 %>%
  group_by(pair_id) %>%
  mutate(sum_trade = sum(trade)) %>%
  ungroup()

# ols ----

ch1_app3_ols <- tp_summary_app3(
  formula = "log_trade ~ 0 + log_dist + cntg + lang + clny +
    rta + exp_year + imp_year",
  data = filter(ch1_application3, trade > 0, importer != exporter),
  method = "lm"
)

# ppml ----

ch1_app3_ppml <- tp_summary_app3(
  formula = "trade ~ 0 + log_dist + cntg + lang + clny +
    rta + exp_year + imp_year",
  data = filter(ch1_application3, importer != exporter),
  method = "glm"
)

# trade diversion ----

ch1_app3_intra <- tp_summary_app3(
  formula = "trade ~ 0 + log_dist + cntg + lang + clny +
    rta + exp_year + imp_year + intl_brdr",
  data = ch1_application3,
  method = "glm"
)

# endogeneity ----

ch1_app3_endg <- tp_summary_app3(
  formula = "trade ~ 0 + rta + exp_year + imp_year + pair_id_2",
  data = filter(ch1_application3, sum_trade > 0),
  method = "glm"
)

# reverse causality ----

ch1_app3_lead <- tp_summary_app3(
  formula = "trade ~ 0 + rta + rta_lead4 + exp_year + imp_year + pair_id_2",
  data = filter(ch1_application3, sum_trade > 0),
  method = "glm"
)

# non-linear/phasing effects ----

ch1_app3_phsng <- tp_summary_app3(
  formula = "trade ~ 0 + rta + rta_lag4 + rta_lag8 + rta_lag12 +
    exp_year + imp_year + pair_id_2",
  data = filter(ch1_application3, sum_trade > 0),
  method = "glm"
)

# globalization ----

ch1_app3_glbzn <- tp_summary_app3(
  formula = "trade ~ 0 + rta + rta_lag4 + rta_lag8 + rta_lag12 +
    intl_border_1986 + intl_border_1990 + intl_border_1994 +
    intl_border_1998 + intl_border_2002 +
    exp_year + imp_year + pair_id_2",
  data = filter(ch1_application3, sum_trade > 0),
  method = "glm"
)

save.image("all-models-and-data/03-chapter1-regional-trade-agreements.RData", compress = "xz")
