# =============================================================================
# 06 - 2016 GDP by microregion (control variable)
# Stata: "9 - Clean GDP.do"
# =============================================================================

gdp_file <- find_file(file.path(RAW, "GDP"), "^PIB.*\\.xls$")
pib <- read_excel_safe(gdp_file, sheet = 1)

col_micro <- grep("^Código da Microrregião", names(pib), value = TRUE)
col_gdp   <- grep("^Produto Interno Bruto,", names(pib), value = TRUE)  # not "per capita"

gdp <- pib %>%
  filter(Ano == 2016) %>%
  transmute(microreg = as.numeric(.data[[col_micro]]),
            GDP = as.numeric(.data[[col_gdp]])) %>%
  group_by(microreg) %>%
  summarise(GDP = sum0(GDP) / 1e6, .groups = "drop")   # same scaling as the authors

save_proc(gdp, "GDP_2016")
