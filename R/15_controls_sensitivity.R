# =============================================================================
# 15 - Sensitivity of the main coefficient to the controls (Section 4.1)
#
# Table 1, column 1 (formal employment, t = 2021, RTW_CH only), estimated
#   (a) with all controls (paper),
#   (b) without the share of workers under 30,
#   (c) without any controls (state fixed effects kept).
# Output: output/extension/controls_sensitivity.csv
# =============================================================================

suppressPackageStartupMessages({ library(sandwich); library(lmtest) })

df <- load_proc("analysis_dataset") %>% filter(year == 2021)

specs <- c(
  "All controls (paper)"   = "fem + hs_comp + young + GDP + share_agric + factor(uf)",
  "Without share under 30" = "fem + hs_comp + GDP + share_agric + factor(uf)",
  "No controls (state FE)" = "factor(uf)"
)

controls_sens <- bind_rows(lapply(names(specs), function(s) {
  fit <- lm(as.formula(paste("d_employ ~ RTR_TW_CH_Total +", specs[[s]])), data = df)
  G   <- length(unique(df$mesoreg))
  ct  <- coeftest(fit, vcov. = vcovCL(fit, cluster = ~ mesoreg, type = "HC1"), df = G - 1)
  tibble(specification = s, estimate = ct["RTR_TW_CH_Total", 1],
         se = ct["RTR_TW_CH_Total", 2], p = ct["RTR_TW_CH_Total", 4], N = nobs(fit))
}))

write_csv(controls_sens, file.path(EXT, "controls_sensitivity.csv"))
print(as.data.frame(controls_sens %>% mutate(across(c(estimate, se, p), ~ round(.x, 3)))))
