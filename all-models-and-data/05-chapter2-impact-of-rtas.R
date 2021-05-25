library(tradepolicy)

# data ----

ch2_application2 <- agtpa_applications %>%
  select(exporter, importer, pair_id, year, trade, dist, cntg, lang, clny, rta) %>%
  filter(year %in% seq(1986, 2006, 4)) %>%
  mutate(
    log_dist = log(dist),
    intl = ifelse(exporter != importer, 1, 0),
    exporter = ifelse(exporter == "DEU", "0-DEU", exporter),
    importer = ifelse(importer == "DEU", "0-DEU", importer)
  ) %>%

  # Create Yit
  group_by(exporter, year) %>%
  mutate(y = sum(trade)) %>%

  # Create Eit
  group_by(importer, year) %>%
  mutate(e = sum(trade)) %>%

  # Create Er
  group_by(year) %>%
  mutate(e_r = max(ifelse(importer == "0-DEU", e, NA), na.rm = T))

ch2_application2 <- ch2_application2 %>%
  mutate(
    exp_year = paste0(exporter, year),
    imp_year = paste0(importer, year),
    pair_id_2 = ifelse(exporter == importer, "0-intra", pair_id)
  )

ch2_application2 <- ch2_application2 %>%
  group_by(pair_id) %>%
  mutate(sum_trade = sum(trade)) %>%
  ungroup()

# Step 1: Solve the baseline gravity model ----

fit_baseline_app2 <- glm(
  trade ~ 0 + rta + exp_year + imp_year + pair_id_2,
  family = quasipoisson(link = "log"),
  data = filter(ch2_application2, sum_trade > 0)
)

ch2_application2 <- ch2_application2 %>%
  left_join(
    tp_fixed_effects(fit_baseline_app2),
    by = c("exp_year", "imp_year", "pair_id_2")
  )

ch2_application2 <- ch2_application2 %>%
  mutate(
    tij_bar = exp(fe_pair_id_2),
    tij_bln = exp(fe_pair_id_2 + fit_baseline_app2$coefficients["rta"] * rta)
  )

ch2_application2_1994 <- ch2_application2 %>%
  filter(year == 1994, exporter != importer)

fit_costs_app2 <- glm(
  tij_bar ~ 0 + log_dist + cntg + lang + clny + exporter + importer,
  family = quasipoisson(link = "log"),
  data = ch2_application2_1994
)

ch2_application2_1994 <- ch2_application2_1994 %>%
  mutate(tij_no_rta = predict(fit_costs_app2, ch2_application2_1994, "response")) %>%
  select(exporter, importer, tij_no_rta)

ch2_application2 <- ch2_application2 %>%
  filter(year == 1994) %>%
  left_join(ch2_application2_1994, by = c("exporter", "importer")) %>%
  mutate(
    tij_bar = ifelse(is.na(tij_bar), tij_no_rta, tij_bar),
    tij_bln = ifelse(is.na(tij_bln), tij_bar * exp(fit_baseline_app2$coefficients["rta"] * rta), tij_bln)
  ) %>%
  select(-tij_no_rta) %>%
  mutate(log_tij_bln = log(tij_bln))

fit_constrained_app2 <- glm(
  trade ~ 0 + exporter + importer + offset(log_tij_bln),
  family = quasipoisson(link = "log"),
  data = ch2_application2
)

ch2_application2 <- ch2_application2 %>%
  mutate(tradehat_bln = predict(fit_constrained_app2, ch2_application2, "response")) %>%
  group_by(exporter) %>%
  mutate(xi_bln = sum(tradehat_bln * (exporter != importer))) %>%
  ungroup()

ch2_application2 <- ch2_application2 %>%
  left_join(tp_fixed_effects(fit_constrained_app2), by = c("exporter","importer"))

ch2_application2 <- ch2_application2 %>%
  mutate(
    omr_bln = y * e_r/ exp(fe_exporter),
    imr_bln = e / (exp(fe_importer) * e_r)
  )

# Step II: Define a counterfactual scenario ----

nafta <- c("MEX", "USA", "CAN")

ch2_application2 <- ch2_application2 %>%
  mutate(
    rta_no_nafta = ifelse(exporter %in% nafta & importer %in% nafta, 0, rta),
    tij_cfl = tij_bar * exp(fit_baseline_app2$coefficients["rta"] * rta_no_nafta),
    log_tij_cfl = log(tij_cfl)
  )

# Step III: Solve the counterfactual model ----

fit_counterfactual_app2 <- glm(
  trade ~ 0 + exporter + importer + offset(log_tij_cfl),
  family = quasipoisson(link = "log"),
  data = ch2_application2
)

ch2_application2 <- ch2_application2 %>%
  left_join(tp_fixed_effects(fit_counterfactual_app2), by = c("exporter","importer")) %>%
  rename(
    fe_exporter_bln = fe_exporter.x,
    fe_exporter_cfl = fe_exporter.y,
    fe_importer_bln = fe_importer.x,
    fe_importer_cfl = fe_importer.y
  )

ch2_application2 <- ch2_application2 %>%
  mutate(
    omr_cfl = y * e_r / exp(fe_exporter_cfl),
    imr_cfl = e / (exp(fe_importer_cfl) * e_r)
  )

ch2_application2 <- ch2_application2 %>%
  mutate(tradehat_cfl = predict(fit_counterfactual_app2, ch2_application2, "response")) %>%
  group_by(exporter) %>%
  mutate(xi_cfl = sum(tradehat_cfl * (exporter != importer))) %>%
  ungroup()

# set the criteria of convergence
# taken from the literature (see the Stata code)
sigma <- 7

ch2_application2 <- ch2_application2 %>%
  mutate(
    change_tij = tij_cfl / tij_bln,
    phi = ifelse(importer == exporter, e / y, 0)
  ) %>%
  group_by(exporter) %>%
  mutate(phi = max(phi)) %>%
  ungroup()

ch2_application2 <- ch2_application2 %>%
  group_by(exporter) %>%
  mutate(change_p_i = ((exp(fe_exporter_cfl) / e_r) / (exp(fe_exporter_bln) / e_r))^(1 /(1 - sigma))) %>%
  ungroup() %>%

  group_by(importer) %>%
  mutate(
    change_p_j = ifelse(importer == exporter, change_p_i, 0),
    change_p_j = max(change_p_j)
  ) %>%
  ungroup()

ch2_application2 <- ch2_application2 %>%
  mutate(trade_cfl = tradehat_cfl * change_p_i * change_p_j)

ch2_application2 <- ch2_application2 %>%
  mutate(
    omr_cfl_0 = omr_cfl,
    imr_cfl_0 = imr_cfl,
    change_imr_full_0 = 1,
    change_omr_full_0 = 1,
    change_p_i_0 = change_p_i,
    change_p_j_0 = change_p_j,
    fe_exporter_cfl_0 = fe_exporter_cfl,
    fe_importer_cfl_0 = fe_importer_cfl,
    tradehat_0 = tradehat_cfl,
    e_r_cfl_0 = e_r
  )

# set parameters
max_dif <- 1
sd_dif <- 1
change_price_i_old <- 0

i2 <- 1
while(sd_dif > 1e-3 | max_dif > 1e-3) {
  ch2_application2 <- ch2_application2 %>%
    mutate(trade_1 = tradehat_0 * change_p_i_0 * change_p_j_0 / (change_omr_full_0 * change_imr_full_0))

  # repeat the counterfactual model
  fit_counterfactual_app2_2 <- glm(
    trade_1 ~ 0 + exporter + importer + offset(log_tij_cfl),
    family = quasipoisson(link = "log"),
    data = ch2_application2
  )

  ch2_application2 <- ch2_application2 %>%
    left_join(
      tp_fixed_effects(fit_counterfactual_app2_2),
      by = c("exporter", "importer")
    )

  # compute the conditional general equilibrium effects of trade
  ch2_application2 <- ch2_application2 %>%
    mutate(tradehat_1 = predict(fit_counterfactual_app2_2, ch2_application2, "response")) %>%
    group_by(exporter) %>%
    mutate(y_cfl_1 = sum(tradehat_1)) %>%
    ungroup() %>%

    mutate(e_cfl_1 = ifelse(importer == exporter, phi * y_cfl_1, 0)) %>%
    group_by(importer) %>%
    mutate(e_cfl_1 = max(e_cfl_1)) %>%
    ungroup() %>%

    mutate(
      e_r_cfl_1 = ifelse(importer == "0-DEU", e_cfl_1, 0),
      e_r_cfl_1 = max(e_r_cfl_1)
    )

  # compute the change in prices for exporters and importers
  ch2_application2 <- ch2_application2 %>%
    mutate(change_p_i_1 = ((exp(fe_exporter) / e_r_cfl_1) /
      (exp(fe_exporter_cfl_0) / e_r_cfl_0))^(1 / (1 - sigma)))

  # compute the change in prices for exporters and importers
  ch2_application2 <- ch2_application2 %>%
    group_by(importer) %>%
    mutate(
      change_p_j_1 = ifelse(importer == exporter, change_p_i_1, 0),
      change_p_j_1 = max(change_p_j_1)
    ) %>%
    ungroup()

  # compute both outward and inward multilateral resistance
  ch2_application2 <- ch2_application2 %>%
    mutate(
      omr_cfl_1 = (y_cfl_1 * e_r_cfl_1) / exp(fe_exporter),
      imr_cfl_1 = e_cfl_1 / (exp(fe_importer) * e_r_cfl_1)
    )

  # update the differences
  max_dif <- abs(max(ch2_application2$change_p_i_0 - change_price_i_old))
  sd_dif <- sd(ch2_application2$change_p_i_0 - change_price_i_old)
  change_price_i_old <- ch2_application2$change_p_i_0

  # compute changes in outward and inward multilateral resistance
  ch2_application2 <- ch2_application2 %>%
    mutate(
      change_omr_full_1 = omr_cfl_1 / omr_cfl_0,
      change_imr_full_1 = imr_cfl_1 / imr_cfl_0,
      omr_cfl_0 = omr_cfl_1,
      imr_cfl_0 = imr_cfl_1,
      change_omr_full_0 = change_omr_full_1,
      change_imr_full_0 = change_imr_full_1,
      change_p_i_0 = change_p_i_1,
      change_p_j_0 = change_p_j_1,
      fe_exporter_cfl_0 = fe_exporter,
      fe_importer_cfl_0 = fe_importer,
      tradehat_0 = tradehat_1,
      e_r_cfl_0 = e_r_cfl_1
    ) %>%
    select(-fe_exporter, -fe_importer)

  i2 <- i2 + 1
}

ch2_application2 <- ch2_application2 %>%
  mutate(
    change_p_i_full = ((exp(fe_exporter_cfl_0) / e_r_cfl_0) /
                         (exp(fe_exporter_bln) / e_r))^(1 / (1 - sigma)),
    change_p_j_full = change_p_i_full * (exporter == importer)
  ) %>%
  group_by(importer) %>%
  mutate(change_p_j_full = max(change_p_j_full)) %>%
  ungroup() %>%
  mutate(y_full = change_p_i_full * y)

ch2_application2 <- ch2_application2 %>%
  mutate(e_full = change_p_j_full * e * (exporter == importer)) %>%
  group_by(importer) %>%
  mutate(e_full = max(e_full, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(
    e_full_r = e_full * (importer == "0-DEU"),
    e_full_r = max(e_full_r)
  )

ch2_application2 <- ch2_application2 %>%
  mutate(
    omr_full = y_full * e_r_cfl_0 / exp(fe_exporter_cfl_0),
    imr_full = e_cfl_1 / (exp(fe_importer_cfl_0) * e_r_cfl_0)
  )

ch2_application2 <- ch2_application2 %>%
  mutate(x_full = (y_full * e_full * tij_cfl) / (imr_full * omr_full)) %>%
  group_by(exporter) %>%
  mutate(xi_full = sum(x_full * (importer != exporter))) %>%
  ungroup()

# Step IV: Collect, construct, and report indexes of interest ----

exporter_indexes <- ch2_application2 %>%
  select(
    exporter, starts_with("omr_"), change_p_i_full,
    starts_with("xi_"), y, y_full
  ) %>%
  distinct() %>%
  mutate(exporter = ifelse(exporter == "0-DEU", "DEU", exporter)) %>%
  arrange(exporter) %>%
  mutate(
    change_p_i_full = (1 - change_p_i_full) * 100,
    change_omr_cfl = ((omr_bln / omr_cfl)^(1 / (1-sigma)) - 1) * 100,
    change_omr_full = ((omr_bln / omr_full)^(1 / (1-sigma)) - 1) * 100,
    change_xi_cfl = (xi_bln / xi_cfl - 1) * 100,
    change_xi_full = (xi_bln / xi_full - 1) * 100
  ) %>%
 select(exporter, starts_with("change"), starts_with("y"))

importer_indexes <- ch2_application2 %>%
  select(importer, imr_bln, imr_cfl, imr_full) %>%
  ungroup()  %>%
  distinct() %>%
  mutate(importer = ifelse(importer == "0-DEU", "DEU", importer)) %>%
  arrange(importer) %>%
  mutate(
    change_imr_cfl = ((imr_bln / imr_cfl)^(1 / (1 - sigma)) - 1) * 100,
    change_imr_full = ((imr_bln / imr_full)^(1 / (1 - sigma)) - 1) * 100
  )

indexes_final <- exporter_indexes %>%
  left_join(importer_indexes, by = c("exporter" = "importer")) %>%
  mutate(
    rgdp_bln = y / (imr_bln^(1 / (1 - sigma))),
    rgdp_full = y_full / (imr_full^(1 / (1 - sigma))),
    change_rgdp_full = (rgdp_bln / rgdp_full - 1) * 100
  ) %>%
  select(exporter, change_xi_cfl, change_xi_full,
         change_rgdp_full, change_imr_full, change_omr_full, change_p_i_full)

indexes_final <- indexes_final %>%
  mutate_if(is.numeric, function(x) round(x, 2))

save.image("all-models-and-data/05-chapter2-impact-of-rtas.RData", compress = "xz")
