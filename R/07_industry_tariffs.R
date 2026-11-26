# =============================================================================
# 07 - Industry (CNAE 2.0) level tariffs and affected trade flows
# Stata: "10 - Tariff_Cnae20.do"
#
#   tau_{C1,t,i} = sum_{p in i} [ M^{C1}_{p,2016} / sum_{p in i} M^{C1}_{p,2016} ] * tau_{C1,t,p}
#   (weights = product's share in country C1's 2016 imports of industry i)
# =============================================================================

trade  <- load_proc("imports_world_and_bilateral_China_USA_2016")
us_mfn <- load_proc("UStariffs_mfn_6-digit-HS4")
ch_mfn <- load_proc("CHtariffs_mfn_6-digit-HS4")
ch_tw  <- load_proc("CHtariffs_onlytw_HS4")
us_tw  <- load_proc("UStariffs_onlytw_HS4")
corr   <- load_proc("Correspondence_HS4_to_CNAE20_tradable")

# Product-level dataset: keep products present in all tariff files
# (each Stata merge drops _merge == 1 and _merge == 2 -> inner joins)
prod <- trade %>%
  mutate(hs6 = pad_hs6(commoditycode)) %>%
  select(-commoditycode) %>%
  inner_join(us_mfn, by = "hs6") %>%
  inner_join(ch_mfn, by = "hs6") %>%
  inner_join(ch_tw,  by = "hs6") %>%
  inner_join(us_tw,  by = "hs6") %>%
  mutate(hs6 = as.numeric(hs6))

# Attach industries (a product can belong to several industries).
# Products without a correspondence are put in a residual category 99999.
prod <- prod %>%
  left_join(corr %>% filter(!is.na(hs6)) %>% mutate(.matched = TRUE),
            by = "hs6", relationship = "one-to-many") %>%
  mutate(CNAE20 = ifelse(is.na(.matched), 99999, CNAE20)) %>%
  select(-.matched)

# Import values in millions of US$
prod <- prod %>%
  mutate(across(c(total_imports, imports_China_from_USA, import_China,
                  imports_USA_from_China, import_USA), ~ .x / 1e6))

# ---- 1) Share of world trade directly affected (used in Eq. 3) ------------------
share_trade <- prod %>%
  group_by(CNAE20) %>%
  summarise(across(c(total_imports, import_USA, import_China,
                     imports_China_from_USA, imports_USA_from_China), sum0),
            .groups = "drop") %>%
  transmute(CNAE20,
            share_imp_USA_from_CH = imports_USA_from_China / total_imports,
            share_imp_CH_from_US  = imports_China_from_USA / total_imports,
            share_imp_USA         = import_USA / total_imports,
            share_imp_CH          = import_China / total_imports)
save_proc(share_trade, "share_trade_affected")

# ---- 2) Import-weighted industry tariffs ------------------------------------------
prod <- prod %>%
  group_by(CNAE20) %>%
  mutate(total_imports_industry_US = sum0(import_USA),
         total_imports_industry_CH = sum0(import_China)) %>%
  ungroup() %>%
  mutate(sharevalue_US = safe_div(import_USA,   total_imports_industry_US),
         sharevalue_CH = safe_div(import_China, total_imports_industry_CH))

years <- c(2016, 2018, 2019)
for (y in years) {
  prod[[paste0("US_TW_tariff_ind",  y)]] <- prod[[paste0("US_TW_tariff",  y)]] * prod$sharevalue_US
  prod[[paste0("US_MFN_tariff_ind", y)]] <- prod[[paste0("US_mfn_tariff", y)]] * prod$sharevalue_US
  prod[[paste0("CH_TW_tariff_ind",  y)]] <- prod[[paste0("CH_TW_tariff",  y)]] * prod$sharevalue_CH
  prod[[paste0("CH_MFN_tariff_ind", y)]] <- prod[[paste0("CH_mfn_tariff", y)]] * prod$sharevalue_CH
}
# Trade war change + 2016 MFN level (robustness measure, Appendix G.1)
for (y in years) {
  prod[[paste0("US_TW_MFN16_tariff_ind", y)]] <- prod[[paste0("US_TW_tariff_ind", y)]] + prod$US_MFN_tariff_ind2016
  prod[[paste0("CH_TW_MFN16_tariff_ind", y)]] <- prod[[paste0("CH_TW_tariff_ind", y)]] + prod$CH_MFN_tariff_ind2016
}

ind_tariffs <- prod %>%
  group_by(CNAE20) %>%
  summarise(across(c(matches("_tariff_ind\\d{4}$"), starts_with("total_imports_industry")), sum0),
            .groups = "drop")

# Industries with zero imports get a missing tariff
us_cols <- grep("^US_.*_ind\\d{4}$", names(ind_tariffs), value = TRUE)
ch_cols <- grep("^CH_.*_ind\\d{4}$", names(ind_tariffs), value = TRUE)
ind_tariffs[ind_tariffs$total_imports_industry_US == 0, us_cols] <- NA
ind_tariffs[ind_tariffs$total_imports_industry_CH == 0, ch_cols] <- NA

save_proc(ind_tariffs, "tariffCNAE20_weighted_by_imports")
