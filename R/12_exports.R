# =============================================================================
# 12 - Monthly Brazilian exports by HS6 product, 2017-2021, to China, the US
#      and the world (input for Figure 2)
# Stata: "Clean Data/13 - clean Brazil Exports.do"
#
# Raw data: Data/Raw Data/Brazilian Exports/EXP_COMPLETA.csv (SECEX/Comex Stat,
# all Brazilian exports 1997-2025, several GB). The file is read in chunks of
# one million rows and only 2017-2021 is kept, so it fits in memory.
#
# If EXP_COMPLETA.csv is not in the Data folder, the script falls back to the
# authors' processed file (Processed Data/Brazilian Exports/exp_clean.dta).
# =============================================================================

raw_file  <- file.path(RAW, "Brazilian Exports", "EXP_COMPLETA.csv")
us_cpi    <- load_proc("US_CPI")

if (file.exists(raw_file)) {

  # Detect the delimiter from the header line (SECEX uses ";")
  header <- readLines(raw_file, n = 1)
  delim  <- if (grepl(";", header)) ";" else ","

  # Aggregate one chunk to year x month x HS6
  aggregate_chunk <- function(x, pos) {
    names(x) <- tolower(names(x))
    x %>%
      mutate(across(c(co_ano, co_mes, co_ncm, co_pais, kg_liquido, vl_fob), as.numeric)) %>%
      filter(co_ano >= 2017, co_ano <= 2021) %>%
      mutate(hs6 = co_ncm %/% 100,                       # 8-digit NCM -> 6-digit HS
             vl_fob_CH     = ifelse(co_pais == 160, vl_fob, 0),      # 160 = China
             kg_liquido_CH = ifelse(co_pais == 160, kg_liquido, 0),
             vl_fob_US     = ifelse(co_pais == 249, vl_fob, 0),      # 249 = USA
             kg_liquido_US = ifelse(co_pais == 249, kg_liquido, 0)) %>%
      group_by(year = co_ano, month = co_mes, hs6) %>%
      summarise(across(c(kg_liquido, vl_fob, vl_fob_CH, kg_liquido_CH, vl_fob_US, kg_liquido_US), sum0),
                .groups = "drop")
  }

  message("Reading EXP_COMPLETA.csv in chunks (this can take a few minutes)...")
  chunks <- read_delim_chunked(
    raw_file, callback = DataFrameCallback$new(aggregate_chunk),
    delim = delim, chunk_size = 1e6,
    col_types = cols(.default = col_character()), progress = FALSE
  )

  exp <- chunks %>%                                       # chunks can split a cell
    group_by(year, month, hs6) %>%
    summarise(across(everything(), sum0), .groups = "drop")

} else {
  message("NOTE: ", raw_file, " not found. Using the authors' processed export file instead.")
  exp <- read_dta(file.path(DATA, "Processed Data", "Brazilian Exports", "exp_clean.dta")) %>%
    zap_labels() %>%
    left_join(us_cpi, by = "year") %>%                    # undo their deflation, redone below
    mutate(across(c(vl_fob, vl_fob_CH, vl_fob_US), ~ .x / US_infl)) %>%
    select(year, month, hs6, kg_liquido, vl_fob, vl_fob_CH, kg_liquido_CH, vl_fob_US, kg_liquido_US)
}

# Balanced panel: every product in every month 2017-2021, zeros where not exported
# (Stata: reshape wide, mvencode _all, mv(0), reshape long)
exp <- exp %>%
  complete(hs6, year = 2017:2021, month = 1:12,
           fill = list(kg_liquido = 0, vl_fob = 0, vl_fob_CH = 0, kg_liquido_CH = 0,
                       vl_fob_US = 0, kg_liquido_US = 0)) %>%
  mutate(period = (year - 2017) * 12 + month) %>%         # 1 = Jan 2017, ..., 60 = Dec 2021
  # deflate values to 2016 US dollars with the US CPI
  left_join(us_cpi %>% select(year, US_infl), by = "year") %>%
  mutate(across(c(vl_fob, vl_fob_CH, vl_fob_US), ~ .x * US_infl)) %>%
  select(-US_infl) %>%
  arrange(hs6, period)

save_proc(exp, "exp_clean")
