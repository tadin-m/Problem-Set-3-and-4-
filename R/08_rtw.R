# =============================================================================
# 08 - Regional Trade War tariff changes (RTW), Equations (3) and (4)
# Stata: "11 - RTW.do"
#
#   GMI_i^{C1}  = ( M_{i,2016}^{C1<-C2} / M_{i,2016}^W ) * ln(1 + tau^{TW}_{C1,i,2019})   (3)
#   RTW_r^{C1}  = sum_i lambda_{r,i} * GMI_i^{C1}                                        (4)
#   lambda_{r,i} = 2016 share of region r's tradable formal employment in industry i
# =============================================================================

cnae_group  <- load_proc("CNAE_Group")
ind_tariffs <- load_proc("tariffCNAE20_weighted_by_imports")
share_trade <- load_proc("share_trade_affected")

# 2016 formal employment by microregion x CNAE class (authors' RAIS aggregate)
w <- read_dta(file.path(RAIS, "Rais_2016 - weights.dta")) %>% zap_labels()
names(w)[names(w) == "cnae20classe"] <- "CNAE20"
names(w)[grepl("ativo", names(w))]    <- "ativos"   # "vínculoativo3112"

# Apply the industry grouping; CNAE classes not in the grouping file are
# non-tradable and get code 999 (Stata: merge m:1, drop _merge==2, _merge==1 -> 999)
w <- w %>%
  left_join(cnae_group %>% mutate(.matched = TRUE), by = "CNAE20") %>%
  mutate(CNAE20 = ifelse(is.na(.matched), 999, activity)) %>%
  group_by(microreg, CNAE20) %>%
  summarise(ativos = sum0(ativos), .groups = "drop")

# Keep non-tradable employment aside, then drop it
w <- w %>%
  group_by(microreg) %>%
  mutate(non_tradable = if (any(CNAE20 == 999)) max(ativos[CNAE20 == 999]) else NA_real_) %>%
  ungroup() %>%
  filter(CNAE20 != 999)

# Merge industry tariffs and affected trade shares (inner joins, as in Stata)
rtw_ind <- w %>%
  inner_join(ind_tariffs, by = "CNAE20") %>%
  inner_join(share_trade, by = "CNAE20")

# Employment shares over tradable industries (sum to one in each region)
rtw_ind <- rtw_ind %>%
  group_by(microreg) %>%
  mutate(lambda = ativos / sum0(ativos)) %>%
  ungroup()

# ---- RTW measures (region x industry terms) ---------------------------------------
rtw_ind <- rtw_ind %>%
  mutate(
    # 1) Main measure, Eq. (3)-(4)
    RTR_TW_CH_Total = lambda * share_imp_CH_from_US  * log(1 + CH_TW_tariff_ind2019),
    RTR_TW_US_Total = lambda * share_imp_USA_from_CH * log(1 + US_TW_tariff_ind2019),
    # 2) MFN tariff changes 2016-2019 (control in Panels A.3 / B.3, Appendix F)
    RTR_MFN_CH_Total = lambda * share_imp_CH  * (log(1 + CH_MFN_tariff_ind2019) - log(1 + CH_MFN_tariff_ind2016)),
    RTR_MFN_US_Total = lambda * share_imp_USA * (log(1 + US_MFN_tariff_ind2019) - log(1 + US_MFN_tariff_ind2016)),
    # 3) Trade war + 2016 MFN level (Appendix G.1)
    RTR_TW_16_MFN_CH_Total = lambda * share_imp_CH_from_US  * (log(1 + CH_TW_MFN16_tariff_ind2019) - log(1 + CH_TW_MFN16_tariff_ind2016)),
    RTR_TW_16_MFN_US_Total = lambda * share_imp_USA_from_CH * (log(1 + US_TW_MFN16_tariff_ind2019) - log(1 + US_TW_MFN16_tariff_ind2016)),
    # 4) Squared trade shares (Appendix G.2)
    RTR_TW_CH_squared_Total = lambda * share_imp_CH_from_US^2  * log(1 + CH_TW_tariff_ind2019),
    RTR_TW_US_squared_Total = lambda * share_imp_USA_from_CH^2 * log(1 + US_TW_tariff_ind2019),
    # 5) Square-root trade shares (Appendix G.2).
    #    NOTE: the authors' code uses sqrt(share^2) for the US measure, which is
    #    just the share itself. We copy their line to replicate their numbers.
    RTR_TW_CH_sroot_Total         = lambda * sqrt(share_imp_CH_from_US)    * log(1 + CH_TW_tariff_ind2019),
    RTR_TW_US_sroot_squared_Total = lambda * sqrt(share_imp_USA_from_CH^2) * log(1 + US_TW_tariff_ind2019),
    # 6) Only tariffs in place by end of 2018 (Appendix G.3)
    RTR_TW_16_MFN_CH_Total_2018 = lambda * share_imp_CH_from_US  * log(1 + CH_TW_tariff_ind2018),
    RTR_TW_16_MFN_US_Total_2018 = lambda * share_imp_USA_from_CH * log(1 + US_TW_tariff_ind2018)
  )

# Sum over industries (missing terms count as zero, like Stata's collapse)
rtw_raw <- rtw_ind %>%
  group_by(microreg) %>%
  summarise(across(starts_with("RTR"), sum0), .groups = "drop")
save_proc(rtw_raw, "RTW_unstandardized")   # kept for the maps / descriptive use

# Standardise: mean 0, SD 1 across microregions
standardise <- function(x) (x - mean(x)) / sd(x)
rtw <- rtw_raw %>% mutate(across(starts_with("RTR"), standardise))

save_proc(rtw, "RTW")

# Data behind Figure 3 (the map is not produced by the authors' code)
write_csv(select(rtw, microreg, RTR_TW_CH_Total, RTR_TW_US_Total),
          file.path(PROC, "Data_fig3_RTW_Map.csv"))
