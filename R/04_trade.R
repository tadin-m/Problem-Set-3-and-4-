# =============================================================================
# 04 - 2016 trade flows: world imports by product, and bilateral flows
#      China <- US and US <- China (UN Comtrade).
# Stata: "4 - clean_trade_data_2016.do" (first part; the Brazilian export part
#        is only used for Appendix Table B1 and is not needed for Table 1)
# =============================================================================

# ---- 1) World imports in 2016 (sum over all reporting countries) ------------------
big <- read_csv(file.path(RAW, "Trade", "Comtrade - thebigdataset.csv"),
                show_col_types = FALSE, name_repair = "unique_quiet")

imports_world <- big %>%
  transmute(commoditycode = as.numeric(Commodity.Code),
            reporter = Reporter,
            value = as.numeric(Trade.Value..US..)) %>%
  group_by(commoditycode) %>%
  summarise(
    total_imports = sum0(value),
    import_USA    = if (any(reporter == "USA"))   max(value[reporter == "USA"])   else 0,
    import_China  = if (any(reporter == "China")) max(value[reporter == "China"]) else 0,
    .groups = "drop"
  )

# ---- 2) Bilateral flows --------------------------------------------------------
read_bilateral <- function(file, newname) {
  read_csv(file.path(RAW, "Trade", file), show_col_types = FALSE,
           col_types = cols(.default = "c")) %>%
    transmute(commoditycode = as.numeric(`Commodity Code`),
              !!newname := as.numeric(`Trade Value (US$)`))
}
ch_from_us <- read_bilateral("HS4Imports_China_from_USA_2016.csv", "imports_China_from_USA")
us_from_ch <- read_bilateral("HS4Imports_USA_from_China_2016.csv", "imports_USA_from_China")

# Stata: merge 1:1 ... ; replace x = 0 if _merge == 1  (keeps using-only rows too)
merge_keep_all <- function(master, using, var) {
  out <- full_join(master %>% mutate(.in_master = TRUE), using, by = "commoditycode")
  out[[var]] <- ifelse(!is.na(out$.in_master) & is.na(out[[var]]), 0, out[[var]])
  select(out, -.in_master)
}

trade <- imports_world %>%
  merge_keep_all(ch_from_us, "imports_China_from_USA") %>%
  merge_keep_all(us_from_ch, "imports_USA_from_China")

save_proc(trade, "imports_world_and_bilateral_China_USA_2016")
