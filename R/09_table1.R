# =============================================================================
# 09 - Table 1: Equation (5), estimated separately for each year t
# Stata: "16 -Tab 1 and Fig C5 C6 - Results Employment.do"
#
#   y_{r,t} - y_{r,2016} = alpha_{s} + beta_t^{CH} RTW_r^{CH} + gamma_t X_r + e_{r,t}
#
# One cross-sectional OLS regression per year (558 microregions), with state
# dummies and 2016 controls; SEs clustered by mesoregion (137 clusters).
#
# Standard errors: Stata's  reg ..., cluster(mesoreg)  uses the small-sample
# correction  G/(G-1) * (N-1)/(N-K)  and t-tests with G-1 degrees of freedom.
# sandwich::vcovCL(type = "HC1") gives exactly the same correction, and we use
# G-1 degrees of freedom for p-values, so SEs and stars match Stata's.
# =============================================================================

suppressPackageStartupMessages({ library(sandwich); library(lmtest) })

delta    <- load_proc("Delta_employment")
rtw      <- load_proc("RTW")
gdp      <- load_proc("GDP_2016")
controls <- read_dta(file.path(RAIS, "controls_2016.dta"))   %>% zap_labels()
agric    <- read_dta(file.path(RAIS, "share_agric_2016.dta")) %>% zap_labels()

df <- delta %>%
  inner_join(rtw, by = "microreg") %>%
  left_join(controls, by = "microreg") %>%
  left_join(gdp,      by = "microreg") %>%
  left_join(agric,    by = "microreg")

save_proc(df, "analysis_dataset")

ctrl <- "fem + hs_comp + young + GDP + share_agric + factor(uf)"

# Estimate one specification for one year and return the coefficients of interest
estimate <- function(outcome, regressors, y, keep) {
  d   <- filter(df, year == y)
  fml <- as.formula(paste(outcome, "~", paste(regressors, collapse = " + "), "+", ctrl))
  fit <- lm(fml, data = d)
  vc  <- vcovCL(fit, cluster = ~ mesoreg, type = "HC1")
  G   <- length(unique(d$mesoreg[as.numeric(rownames(model.frame(fit)))]))
  ct  <- coeftest(fit, vcov. = vc, df = G - 1)
  data.frame(year = y, term = keep, estimate = ct[keep, 1], se = ct[keep, 2],
             p = ct[keep, 4], N = nobs(fit), clusters = G, row.names = NULL)
}

years <- c(2021, 2020, 2019, 2018, 2017, 2015, 2014, 2013, 2012)

panels <- list(
  A1 = list(outcome = "d_employ",  x = "RTR_TW_CH_Total",                        keep = "RTR_TW_CH_Total"),
  A2 = list(outcome = "d_employ",  x = c("RTR_TW_CH_Total", "RTR_TW_US_Total"),  keep = c("RTR_TW_CH_Total", "RTR_TW_US_Total")),
  A3 = list(outcome = "d_employ",  x = c("RTR_TW_CH_Total", "RTR_MFN_CH_Total"), keep = "RTR_TW_CH_Total"),
  B1 = list(outcome = "d_remdezr", x = "RTR_TW_CH_Total",                        keep = "RTR_TW_CH_Total"),
  B2 = list(outcome = "d_remdezr", x = c("RTR_TW_CH_Total", "RTR_TW_US_Total"),  keep = c("RTR_TW_CH_Total", "RTR_TW_US_Total")),
  B3 = list(outcome = "d_remdezr", x = c("RTR_TW_CH_Total", "RTR_MFN_CH_Total"), keep = "RTR_TW_CH_Total")
)

results <- bind_rows(lapply(names(panels), function(pan) {
  s <- panels[[pan]]
  out <- bind_rows(lapply(years, function(y) estimate(s$outcome, s$x, y, s$keep)))
  out$panel <- pan
  out
}))

write_csv(results, file.path(TABLES, "table1_long.csv"))

# ---- Formatted table, same layout as the paper ------------------------------------
stars <- function(p) ifelse(p < 0.01, "**", ifelse(p < 0.05, "*", ""))
fmt   <- function(x) formatC(x, format = "f", digits = 3)

wide <- results %>%
  mutate(coef = paste0(fmt(estimate), stars(p)),
         se   = paste0("(", fmt(se), ")")) %>%
  select(panel, term, year, coef, se) %>%
  pivot_longer(c(coef, se), names_to = "stat") %>%
  pivot_wider(id_cols = c(panel, term, stat), names_from = year,
              values_from = value, names_prefix = "t=") %>%
  arrange(panel, term != "RTR_TW_CH_Total", stat) %>%
  mutate(term = ifelse(stat == "se", "",
                       recode(term, RTR_TW_CH_Total = "RTW_CH", RTR_TW_US_Total = "RTW_US")),
         panel = ifelse(stat == "se", "", panel)) %>%
  select(-stat)

write_csv(wide, file.path(TABLES, "table1_replication.csv"))
print(as.data.frame(wide), row.names = FALSE)
