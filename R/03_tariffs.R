# =============================================================================
# 03 - Trade-war tariff changes (Li 2021) and MFN tariffs (WTO), at the
#      HS 2012 6-digit level.
# Stata: "2 - Trade War Tariff Changes.do" and
#        "3 - Constructing TW_Tariffs at 2012 HS code.do"
# =============================================================================

hs17_to_12 <- read_dta(file.path(CORR, "HS2017toHS2012ConversionAndCorrelationTables.dta")) %>%
  mutate(across(everything(), as.character))

# ---- 1) Trade-war tariff changes (Li 2021, HS 2017 codes) ----------------------
h6 <- read_excel(file.path(RAW, "Tariffs", "h6_tariff.xls")) %>%
  mutate(hs6 = pad_hs6(hs6)) %>%
  transmute(
    hs6,
    # names = implementation dates of each tariff wave (do-file 2)
    UStariff23032018 = us_sa_tariff_increase,   # steel & aluminium
    UStariff06072018 = uscn34_tariff_increase,
    UStariff23082018 = uscn16_tariff_increase,
    UStariff24092018 = us200b1_tariff_increase,
    UStariff09052019 = us200b2_tariff_increase,
    UStariff01092019 = us300b1_tariff_increase,
    UStariff15122019 = us300b2_tariff_increase,
    CHtariff02042018 = chn3_tariff_increase,
    CHtariff06072018 = chn34_tariff_increase,
    CHtariff23082018 = chn16_tariff_increase,
    CHtariff24092018 = chn60b1_tariff_increase,
    CHtariff13052019 = chn60b2_tariff_increase,
    CHtariff01092019 = chn75b1_tariff_increase,
    CHtariff15122019 = chn75b2_tariff_increase
  )
save_proc(h6, "h6_tariff")   # also used for Figures 1 and 2

# Cumulative tariff change by end of 2018 and end of 2019, then convert
# HS 2017 -> HS 2012 and take the simple mean within each HS 2012 code.
tw <- h6 %>%
  mutate(
    CH_TW_tariff2018 = CHtariff02042018 + CHtariff06072018 + CHtariff23082018 + CHtariff24092018,
    CH_TW_tariff2019 = CH_TW_tariff2018 + CHtariff13052019 + CHtariff01092019 + CHtariff15122019,
    US_TW_tariff2018 = UStariff23032018 + UStariff06072018 + UStariff23082018 + UStariff24092018,
    US_TW_tariff2019 = US_TW_tariff2018 + UStariff09052019 + UStariff01092019 + UStariff15122019
  ) %>%
  select(FromHS2017 = hs6, CH_TW_tariff2018, CH_TW_tariff2019, US_TW_tariff2018, US_TW_tariff2019) %>%
  inner_join(hs17_to_12, by = "FromHS2017") %>%           # drop unmatched (_merge 1 and 2)
  group_by(hs6 = ToHS2012) %>%
  summarise(across(c(CH_TW_tariff2018, CH_TW_tariff2019, US_TW_tariff2018, US_TW_tariff2019), mean_na),
            .groups = "drop") %>%
  # before the trade war the change is zero by construction
  mutate(CH_TW_tariff2016 = 0, US_TW_tariff2016 = 0)

save_proc(select(tw, hs6, starts_with("CH_")), "CHtariffs_onlytw_HS4")
save_proc(select(tw, hs6, starts_with("US_")), "UStariffs_onlytw_HS4")

# ---- 2) MFN tariffs (WTO Tariff Analysis Online), 2012-2019 ---------------------
read_mfn_year <- function(country_file, y) {
  path <- file.path(RAW, "Tariffs", sprintf("%s-%d_DutyDetails_TL.txt", country_file, y))
  d <- read_tsv(path, col_types = cols(.default = "c"), trim_ws = FALSE, quote = "",
                show_col_types = FALSE, name_repair = "minimal")
  d <- d[, c("TL", "Duty Type/Code", "AV Duty Rate", "Duty Nature", "TLS")]
  names(d) <- c("tl", "dutytypecode", "avdutyrate", "dutynature", "tls")

  d <- d %>%
    filter(dutytypecode == "'02'") %>%                          # MFN applied rates only
    mutate(avdutyrate = suppressWarnings(as.numeric(avdutyrate)),
           avdutyrate = ifelse(dutynature != "A", 0, avdutyrate)) %>%  # non ad valorem -> 0
    group_by(tl) %>%
    # if a tariff line has an ad valorem duty, drop its non-ad-valorem duties
    filter(!(dutynature != "A" & any(dutynature == "A"))) %>%
    ungroup() %>%
    filter(trimws(tls) %in% c("", "'00'")) %>%                  # keep the main tariff line
    mutate(hs6 = substr(tl, 2, 7),                              # "'01012100'" -> "010121"
           mfn = avdutyrate / 100)

  if (y >= 2017) {                                              # HS 2017 -> HS 2012
    d <- d %>%
      inner_join(hs17_to_12, by = c("hs6" = "FromHS2017")) %>%
      mutate(hs6 = ToHS2012)
  }
  d %>% group_by(hs6) %>% summarise(mfn = mean_na(mfn), .groups = "drop") %>% mutate(year = y)
}

build_mfn <- function(country_file, prefix) {
  bind_rows(lapply(2012:2019, function(y) read_mfn_year(country_file, y))) %>%
    pivot_wider(names_from = year, values_from = mfn, names_prefix = prefix)
}

ch_mfn <- build_mfn("Chinese", "CH_mfn_tariff")
us_mfn <- build_mfn("US", "US_mfn_tariff")

save_proc(ch_mfn, "CHtariffs_mfn_6-digit-HS4")
save_proc(us_mfn, "UStariffs_mfn_6-digit-HS4")
