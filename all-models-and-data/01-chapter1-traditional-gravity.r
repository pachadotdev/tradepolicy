library(tradepolicy)

# data ----

ch1_application1 <-  agtpa_applications %>%
  select(exporter, importer, pair_id, year, trade, dist, cntg, lang, clny) %>%
  filter(year %in% seq(1986, 2006, 4))

ch1_application1 <- ch1_application1 %>%
  mutate(
    log_trade = log(trade),
    log_dist = log(dist)
  )

ch1_application1 <- ch1_application1 %>%
  # Create Yit
  group_by(exporter, year) %>%
  mutate(
    y = sum(trade),
    log_y = log(y)
  ) %>%

  # Create Eit
  group_by(importer, year) %>%
  mutate(
    e = sum(trade),
    log_e = log(e)
  )

ch1_application1 <- ch1_application1 %>%
  # Replicate total_e
  group_by(exporter, year) %>%
  mutate(total_e = sum(e)) %>%
  group_by(year) %>%
  mutate(total_e = max(total_e)) %>%

  # Replicate rem_exp
  group_by(exporter, year) %>%
  mutate(
    remoteness_exp = sum(dist *  total_e / e),
    log_remoteness_exp = log(remoteness_exp)
  ) %>%

  # Replicate total_y
  group_by(importer, year) %>%
  mutate(total_y = sum(y)) %>%
  group_by(year) %>%
  mutate(total_y = max(total_y)) %>%

  # Replicate rem_imp
  group_by(importer, year) %>%
  mutate(
    remoteness_imp = sum(dist / (y / total_y)),
    log_remoteness_imp = log(remoteness_imp)
  )

ch1_application1 <- ch1_application1 %>%
  # This merges the columns exporter/importer with year
  mutate(
    exp_year = paste0(exporter, year),
    imp_year = paste0(importer, year)
  )

ch1_application1 <- ch1_application1 %>%
  filter(exporter != importer)

# ols ----

fit_ols <- lm(
  log_trade ~ log_dist + cntg + lang + clny + log_y + log_e,
  data = ch1_application1 %>%
    filter(trade > 0)
)

summary(fit_ols)

ch1_app1_ols <- tp_summary_app1(
  formula = "log_trade ~ log_dist + cntg + lang + clny + log_y + log_e",
  data = filter(ch1_application1, trade > 0),
  method = "lm"
)

# ols remoteness ----

ch1_app1_ols_remoteness <- tp_summary_app1(
  formula = "log_trade ~ log_dist + cntg + lang + clny + log_y + log_e +
    log_remoteness_exp + log_remoteness_imp",
  data = filter(ch1_application1, trade > 0),
  method = "lm"
)

# fe ----

ch1_app1_fe <- tp_summary_app1(
  formula = "log_trade ~ log_dist + cntg + lang + clny + exp_year + imp_year",
  data = filter(ch1_application1, trade > 0),
  method = "lm"
)

# ppml ----

# not used in the book
# fit_ppml <- glm(trade ~ log_dist + cntg + lang + clny + exp_year + imp_year,
#   family = quasipoisson(link = "log"),
#   data = ch1_application1,
#   y = FALSE,
#   model = FALSE
# )

ch1_app1_ppml <- tp_summary_app1(
  formula = "trade ~ log_dist + cntg + lang + clny + exp_year + imp_year",
  data = ch1_application1,
  method = "glm"
)

save.image("all-models-and-data/01-chapter1-traditional-gravity.RData", compress = "xz")
