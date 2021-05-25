library(tradepolicy)

# Data ----

ch2_application1 <- agtpa_applications %>%
  select(exporter, importer, pair_id, year, trade, dist, cntg, lang, clny) %>%
  filter(year == 2006) %>%
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
  ungroup() %>%
  mutate(e_r = max(ifelse(importer == "0-DEU", e, NA), na.rm = T))

# Step I: Solve the baseline model ----

fit_baseline_app1 <- glm(
  trade ~ 0 + log_dist + cntg + intl + exporter + importer,
  family = quasipoisson(link = "log"),
  data = ch2_application1
)

ch2_app1_baseline_2 <- tp_clustered_glm(fit_baseline_app1$formula, ch2_application1)

ch2_application1 <- ch2_application1 %>%
  left_join(
    tp_fixed_effects(fit_baseline_app1),
    c("exporter", "importer")
  )

ch2_application1 <- ch2_application1 %>%
  mutate(
    tij_bln = exp(fit_baseline_app1$coefficients["log_dist"] * log_dist +
                    fit_baseline_app1$coefficients["cntg"] * cntg +
                    fit_baseline_app1$coefficients["intl"] * intl),

    # outward multilateral resistance (omr)
    omr_bln = y * (e_r / exp(fe_exporter)),

    # inward multilateral resistance (imr)
    imr_bln = e / (exp(fe_importer) * e_r)
  )

ch2_application1 <- ch2_application1 %>%
  mutate(tradehat_bln = predict(fit_baseline_app1, ch2_application1, "response")) %>%
  group_by(exporter) %>%
  mutate(xi_bln = sum(tradehat_bln * (exporter != importer))) %>%
  ungroup()

# Step II: Define a counterfactual scenario ----

ch2_application1 <- ch2_application1 %>%
  mutate(
    tij_cfl = exp(fit_baseline_app1$coefficients["log_dist"] * log_dist +
                    fit_baseline_app1$coefficients["cntg"] * cntg),
    log_tij_cfl = log(tij_cfl)
  )

ch2_application1 <- ch2_application1 %>%
  mutate(
    intl_cfl = 0,
    tij_bln = exp(fit_baseline_app1$coefficients["log_dist"] * log_dist +
                    fit_baseline_app1$coefficients["cntg"] * cntg +
                    fit_baseline_app1$coefficients["intl"] * intl_cfl),
    log_tij_cfl = log(tij_cfl)
  )

# Step III: Solve the counterfactual model ----

fit_counterfactual_app1 <- glm(
  trade ~ 0 + exporter + importer + offset(log_tij_cfl),
  family = quasipoisson(link = "log"),
  data = ch2_application1
)

ch2_application1 <- ch2_application1 %>%
  left_join(
    tp_fixed_effects(fit_counterfactual_app1),
    by = c("exporter", "importer")
  ) %>%
  rename(
    fe_exporter_bln = fe_exporter.x,
    fe_exporter_cfl = fe_exporter.y,
    fe_importer_bln = fe_importer.x,
    fe_importer_cfl = fe_importer.y
  )

ch2_application1 <- ch2_application1 %>%
  mutate(
    # outward multilateral resistance (omr)
    omr_cfl = y * (e_r / exp(fe_exporter_cfl)),

    # inward multilateral resistance (imr)
    imr_cfl = e / (exp(fe_importer_cfl) * e_r)
  )

ch2_application1 <- ch2_application1 %>%
  mutate(tradehat_cfl = predict(fit_counterfactual_app1, ch2_application1, "response")) %>%
  group_by(exporter) %>%
  mutate(xi_cfl = sum(tradehat_cfl * (exporter != importer))) %>%
  ungroup()

# set the criteria of convergence
# taken from the literature (see the Stata code)
sigma <- 7

ch2_application1 <- ch2_application1 %>%
  mutate(
    change_tij = tij_cfl / tij_bln,
    phi = ifelse(importer == exporter, e / y, 0)
  ) %>%
  group_by(exporter) %>%
  mutate(phi = max(phi)) %>%
  ungroup()

ch2_application1 <- ch2_application1 %>%
  group_by(exporter) %>%
  mutate(change_p_i = ((exp(fe_exporter_cfl) / e_r) / (exp(fe_exporter_bln) / e_r))^(1 /(1 - sigma))) %>%
  ungroup() %>%

  group_by(importer) %>%
  mutate(
    change_p_j = ifelse(importer == exporter, change_p_i, 0),
    change_p_j = max(change_p_j)
  ) %>%
  ungroup()

ch2_application1 <- ch2_application1 %>%
  mutate(trade_cfl = tradehat_cfl * change_p_i * change_p_j)

ch2_application1 <- ch2_application1 %>%
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

i <- 1
while(sd_dif > 1e-5 | max_dif > 1e-5) {
  ch2_application1 <- ch2_application1 %>%
    mutate(trade_1 = tradehat_0 * change_p_i_0 * change_p_j_0 / (change_omr_full_0 * change_imr_full_0))

  # repeat the counterfactual model
  fit_counterfactual_app1_2 <- glm(
    trade_1 ~ 0 + exporter + importer + offset(log_tij_cfl),
    family = quasipoisson(link = "log"),
    data = ch2_application1
  )

  ch2_application1 <- ch2_application1 %>%
    left_join(
      tp_fixed_effects(fit_counterfactual_app1_2),
      by = c("exporter", "importer")
    )

  # compute the conditional general equilibrium effects of trade
  ch2_application1 <- ch2_application1 %>%
    mutate(tradehat_1 = predict(fit_counterfactual_app1_2, ch2_application1, "response")) %>%
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
  ch2_application1 <- ch2_application1 %>%
    mutate(change_p_i_1 = ((exp(fe_exporter) / e_r_cfl_1) /
      (exp(fe_exporter_cfl_0) / e_r_cfl_0))^(1 / (1 - sigma)))

  # compute the change in prices for exporters and importers
  ch2_application1 <- ch2_application1 %>%
    group_by(importer) %>%
    mutate(
      change_p_j_1 = ifelse(importer == exporter, change_p_i_1, 0),
      change_p_j_1 = max(change_p_j_1)
    ) %>%
    ungroup()

  # compute both outward and inward multilateral resistance
  ch2_application1 <- ch2_application1 %>%
    mutate(
      omr_cfl_1 = (y_cfl_1 * e_r_cfl_1) / exp(fe_exporter),
      imr_cfl_1 = e_cfl_1 / (exp(fe_importer) * e_r_cfl_1)
    )

  # update the differences
  max_dif <- abs(max(ch2_application1$change_p_i_0 - change_price_i_old))
  sd_dif <- sd(ch2_application1$change_p_i_0 - change_price_i_old)
  change_price_i_old <- ch2_application1$change_p_i_0

  # compute changes in outward and inward multilateral resistance
  ch2_application1 <- ch2_application1 %>%
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

  i <- i + 1
}

ch2_application1 <- ch2_application1 %>%
  mutate(
    change_p_i_full = ((exp(fe_exporter_cfl_0) / e_r_cfl_0) /
                         (exp(fe_exporter_bln) / e_r))^(1 / (1 - sigma)),
    change_p_j_full = change_p_i_full * (exporter == importer)
  ) %>%
  group_by(importer) %>%
  mutate(change_p_j_full = max(change_p_j_full)) %>%
  ungroup() %>%
  mutate(y_full = change_p_i_full * y)

ch2_application1 <- ch2_application1 %>%
  mutate(e_full = change_p_j_full * e * (exporter == importer)) %>%
  group_by(importer) %>%
  mutate(e_full = max(e_full, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(
    e_full_r = e_full * (importer == "0-DEU"),
    e_full_r = max(e_full_r)
  )

ch2_application1 <- ch2_application1 %>%
  mutate(
    omr_full = y_full * e_r_cfl_0 / exp(fe_exporter_cfl_0),
    imr_full = e_full / (exp(fe_importer_cfl_0) * e_full_r)
  )

ch2_application1 <- ch2_application1 %>%
  mutate(x_full = (y_full * e_full * tij_cfl) / (imr_full * omr_full)) %>%
  group_by(exporter) %>%
  mutate(xi_full = sum(x_full * (importer != exporter))) %>%
  ungroup()

# Step IV: Collect, construct, and report indexes of interest ----

ch2_application1 <- ch2_application1 %>%
  mutate(
    change_price_full = (change_p_i_full - 1) * 100,
    change_omr_cfl = (omr_cfl^(1 / (1 - sigma)) / omr_bln^(1 / (1 - sigma)) - 1) * 100,
    change_omr_full = (omr_full^(1 / (1 - sigma)) / omr_bln^(1 / (1 - sigma)) - 1) * 100,
    change_xi_cfl = (xi_cfl / xi_bln  - 1) * 100,
    change_xi_full = (xi_full / xi_bln - 1) * 100
  )

ch2_application1 <- ch2_application1 %>%
  mutate(
    change_imr_full = -(imr_full^(1 / (1 - sigma)) / imr_bln^(1 / (1 - sigma)) - 1) * 100,
    rgdp = ((y_full / imr_full^(1 / (1 - sigma))) / (y / imr_bln^(1 / (1 - sigma))) - 1) * 100
  )

# Figures replication ----

ch2_application1 <- ch2_application1 %>%
  filter(exporter == importer) %>%
  select(exporter, importer, y, change_xi_cfl, change_xi_full, rgdp,
         change_price_full, change_imr_full) %>%
  mutate(log_y = log(y))

ch2_app1_figures_2 <- ggplot(data = ch2_application1 %>%
         filter(exporter != "HKG")) +
  geom_point(aes(x = log_y, y = change_xi_cfl, color = "1")) +
  geom_point(aes(x = log_y, y = change_xi_full, color = "2")) +
  labs(
    x = "Log value of output",
    y = "Percent change of exports",
    title = "Figure 6: Effects of abolishing international borders on exports",
    caption = "Source: Authors' calculations",
    color = ""
  ) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_color_manual(
    labels = c(
      "Conditional general equilibrium",
      "Full endowment general equilibrium"
    ),
    values = c("#b6b8dd","#232958")
  )

save.image("all-models-and-data/04-chapter2-trade-without-borders.RData", compress = "xz")
