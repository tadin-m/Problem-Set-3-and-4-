# =============================================================================
# 14 - Extension and robustness (Table 2)
#
# All output goes to output/extension/.
#
# Re-estimates the main column (t = 2021, Table 1 Panels A.2 / B.2) with
# three versions of the Chinese exposure measure RTW_CH (Table 2 columns 1-3):
#   (1) Baseline (authors' measure)
#   (4) Without soybeans & oilseeds (CNAE 0115-6, 0116-4)
#   (5) Soybeans & oilseeds only
# (The labels (1), (4), (5) are kept so that the report notebook can read them.)
# Also saves each industry's share of total exposure (soybeans: 24%).
# =============================================================================

suppressPackageStartupMessages({ library(sandwich); library(lmtest) })

h6     <- load_proc("h6_tariff")
hs1712 <- read_dta(file.path(CORR, "HS2017toHS2012ConversionAndCorrelationTables.dta")) %>%
  mutate(across(everything(), as.character))
trade  <- load_proc("imports_world_and_bilateral_China_USA_2016")
us_mfn <- load_proc("UStariffs_mfn_6-digit-HS4")
ch_mfn <- load_proc("CHtariffs_mfn_6-digit-HS4")
corr   <- load_proc("Correspondence_HS4_to_CNAE20_tradable")
cnae_group <- load_proc("CNAE_Group")
share_trade <- load_proc("share_trade_affected")
df     <- load_proc("analysis_dataset")

# ---- 1) Industry-level 2019 trade-war tariffs (to rebuild the exposure measure) ---------
# Same steps as 03_tariffs.R (HS 2017 -> 2012, simple mean) and 07_industry_tariffs.R
# (import-weighted average within industry, same product sample).
industry_tw <- function(ch_waves, us_waves) {
  tw <- h6 %>%
    transmute(FromHS2017 = hs6,
              CH = rowSums(across(all_of(ch_waves))),
              US = rowSums(across(all_of(us_waves)))) %>%
    inner_join(hs1712, by = "FromHS2017") %>%
    group_by(hs6 = ToHS2012) %>%
    summarise(CH = mean(CH), US = mean(US), .groups = "drop")

  prod <- trade %>%
    mutate(hs6 = pad_hs6(commoditycode)) %>%
    inner_join(select(us_mfn, hs6), by = "hs6") %>%   # same product sample as 07
    inner_join(select(ch_mfn, hs6), by = "hs6") %>%
    inner_join(tw, by = "hs6") %>%
    mutate(hs6 = as.numeric(hs6)) %>%
    left_join(corr %>% filter(!is.na(hs6)) %>% mutate(.m = TRUE), by = "hs6", relationship = "one-to-many") %>%
    mutate(CNAE20 = ifelse(is.na(.m), 99999, CNAE20)) %>%
    group_by(CNAE20) %>%
    mutate(tot_US = sum0(import_USA), tot_CH = sum0(import_China)) %>%
    ungroup() %>%
    mutate(sh_US = safe_div(import_USA, tot_US),            # vectors of equal length
           sh_CH = safe_div(import_China, tot_CH))

  prod %>%
    group_by(CNAE20) %>%
    summarise(CH_TW_tariff_ind2019 = sum0(CH * sh_CH), US_TW_tariff_ind2019 = sum0(US * sh_US),
              tot_CH = first(tot_CH), tot_US = first(tot_US), .groups = "drop") %>%
    mutate(CH_TW_tariff_ind2019 = ifelse(tot_CH == 0, NA, CH_TW_tariff_ind2019),
           US_TW_tariff_ind2019 = ifelse(tot_US == 0, NA, US_TW_tariff_ind2019)) %>%
    select(-tot_CH, -tot_US)
}

all_ch <- grep("^CHtariff", names(h6), value = TRUE)
all_us <- grep("^UStariff", names(h6), value = TRUE)
tw_base <- industry_tw(all_ch, all_us)

# ---- 2) Region x industry terms of Eq. (4), for a given set of industry tariffs ---------
w <- read_dta(file.path(RAIS, "Rais_2016 - weights.dta")) %>% zap_labels()
names(w)[names(w) == "cnae20classe"] <- "CNAE20"
names(w)[grepl("ativo", names(w))]    <- "ativos"
w <- w %>%
  left_join(cnae_group %>% mutate(.m = TRUE), by = "CNAE20") %>%
  mutate(CNAE20 = ifelse(is.na(.m), 999, activity)) %>%
  group_by(microreg, CNAE20) %>% summarise(ativos = sum0(ativos), .groups = "drop") %>%
  filter(CNAE20 != 999)

rtw_terms <- function(tw) {
  w %>%
    inner_join(tw, by = "CNAE20") %>%
    inner_join(share_trade, by = "CNAE20") %>%
    group_by(microreg) %>% mutate(lambda = ativos / sum0(ativos)) %>% ungroup() %>%
    mutate(CH = lambda * share_imp_CH_from_US  * log(1 + CH_TW_tariff_ind2019),
           US = lambda * share_imp_USA_from_CH * log(1 + US_TW_tariff_ind2019))
}
standardise <- function(x) (x - mean(x)) / sd(x)
make_rtw <- function(terms, keep = function(i) TRUE) {
  terms %>% filter(keep(CNAE20)) %>%
    group_by(microreg) %>% summarise(CH = sum0(CH), US = sum0(US), .groups = "drop") %>%
    right_join(distinct(w, microreg), by = "microreg") %>%
    mutate(across(c(CH, US), ~ standardise(replace_na(.x, 0))))
}

terms_base <- rtw_terms(tw_base)

# Sanity check: the baseline rebuilt here equals the measure from 08_rtw.R
check <- make_rtw(terms_base) %>% inner_join(load_proc("RTW"), by = "microreg")
if (max(abs(check$CH - check$RTR_TW_CH_Total)) > 1e-8 || max(abs(check$US - check$RTR_TW_US_Total)) > 1e-8) {
  stop("Rebuilt baseline exposure does not match 08_rtw.R")
}

SOY <- 146   # grouped industry: CNAE 0115-6 (soybeans) + 0116-4 (other oilseeds)
stopifnot(all(cnae_group$CNAE20[cnae_group$activity == SOY] %in% c(1156, 1164)))

us_base <- make_rtw(terms_base) %>% select(microreg, US)
versions <- list(
  "(1) Baseline"            = make_rtw(terms_base),
  "(4) Without soybeans"    = make_rtw(terms_base, function(i) i != SOY)    %>% select(-US) %>% left_join(us_base, by = "microreg"),
  "(5) Soybeans only"       = make_rtw(terms_base, function(i) i == SOY)    %>% select(-US) %>% left_join(us_base, by = "microreg")
)

# ---- 3) Estimate the main column with each version ----------------------------------------
ctrl <- "fem + hs_comp + young + GDP + share_agric + factor(uf)"
estimate_version <- function(rtw, outcome, y = 2021) {
  d   <- df %>% filter(year == y) %>% select(-RTR_TW_CH_Total, -RTR_TW_US_Total) %>%
    inner_join(rtw, by = "microreg")
  fit <- lm(as.formula(paste(outcome, "~ CH + US +", ctrl)), data = d)
  G   <- length(unique(d$mesoreg))
  ct  <- coeftest(fit, vcov. = vcovCL(fit, cluster = ~ mesoreg, type = "HC1"), df = G - 1)
  tibble(term = c("CH", "US"), estimate = ct[c("CH", "US"), 1], se = ct[c("CH", "US"), 2],
         p = ct[c("CH", "US"), 4], N = nobs(fit))
}

table2 <- bind_rows(lapply(names(versions), function(v) bind_rows(
  estimate_version(versions[[v]], "d_employ")  %>% mutate(outcome = "Employment"),
  estimate_version(versions[[v]], "d_remdezr") %>% mutate(outcome = "Wage bill")) %>%
  mutate(version = v)))
write_csv(table2, file.path(EXT, "table2_extension_long.csv"))

# ---- 4) Share of each industry in total exposure (soybeans: 24%, Section 5.1) ---------------
contrib <- terms_base %>% group_by(CNAE20) %>% summarise(total = sum0(CH), .groups = "drop") %>%
  mutate(pct_of_exposure = 100 * total / sum(total)) %>% arrange(desc(total))
write_csv(contrib, file.path(EXT, "exposure_share_by_industry.csv"))

# ---- 5) LaTeX table (simple version used in the report) -------------------------------------
# Columns: all industries (paper) | without soybeans | soybeans only
# Panel A: after the trade war (2016 -> 2021). Panel B: before (placebo years).
stars <- function(p) ifelse(p < 0.01, "^{**}", ifelse(p < 0.05, "^{*}", ""))
cell  <- function(b, p) sprintf("$%.3f%s$", b, stars(p))
se    <- function(s) sprintf("(%.3f)", s)
cols  <- c("(1) Baseline", "(4) Without soybeans", "(5) Soybeans only")

tex_row <- function(label, outcome, y) {
  r <- bind_rows(lapply(cols, function(v)
    estimate_version(versions[[v]], outcome, y = y) %>% filter(term == "CH")))
  c(paste0(label, " & ", paste(cell(r$estimate, r$p), collapse = " & "), " \\\\"),
    paste0(" & ", paste(se(r$se), collapse = " & "), " \\\\[2pt]"))
}

tex <- c(
  "\\begin{tabular}{l*{3}{c}}",
  "\\toprule",
  "Exposure measure $RTW_r^{CH}$ built from: & All industries & All except & Soybeans \\\\",
  " & (paper) & soybeans & only \\\\",
  " & (1) & (2) & (3) \\\\",
  "\\midrule",
  "\\multicolumn{4}{l}{\\textbf{Panel A. After the trade war: change 2016 $\\rightarrow$ 2021}} \\\\",
  tex_row("Formal employment", "d_employ", 2021),
  tex_row("Formal wage bill", "d_remdezr", 2021),
  "\\midrule",
  "\\multicolumn{4}{l}{\\textbf{Panel B. Before the trade war (placebo): formal employment}} \\\\",
  tex_row("$t = 2015$", "d_employ", 2015),
  tex_row("$t = 2014$", "d_employ", 2014),
  tex_row("$t = 2013$", "d_employ", 2013),
  "\\midrule",
  "Controls, $RTW_r^{US}$, state FE & Yes & Yes & Yes \\\\",
  "Observations & 558 & 558 & 558 \\\\",
  "\\bottomrule",
  "\\end{tabular}"
)
writeLines(tex, file.path(EXT, "table2_extension.tex"))
