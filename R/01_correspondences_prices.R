# =============================================================================
# 01 - Municipality -> microregion/mesoregion/state correspondence, and price
#      indices (IPCA and US CPI) in 2016 prices.
# Stata: aux codes "0 - Correspondence - municipality to mmc.do" and
#        "0 - Prices Index.do"
# =============================================================================

# ---- 1) Municipality correspondence ------------------------------------------
dtb <- read_excel_safe(file.path(CORR, "RELATORIO_DTB_BRASIL_MUNICIPIO.xls"),
                       sheet = "DTB_2021_Municipio", col_types = "text")

munic <- dtb %>%
  transmute(
    # microregion / mesoregion codes are "UF" + regional code (e.g. "35" + "061")
    uf       = as.numeric(UF),
    microreg = as.numeric(paste0(UF, `Microrregião Geográfica`)),
    mesoreg  = as.numeric(paste0(UF, `Mesorregião Geográfica`)),
    # drop the last digit of the 7-digit code (it is only a check digit)
    munic    = as.numeric(substr(`Código Município Completo`, 1, 6))
  )

save_proc(munic, "munic")

# ---- 2) IPCA (Brazilian CPI), December of each year, 2016 = 1 -----------------
ipca_raw <- read_excel_safe(file.path(RAW, "Prices", "IPCA.xls"), sheet = 1, col_types = "text")
names(ipca_raw)[1:2] <- c("Data", "index")

ipca <- ipca_raw %>%
  filter(Data %in% paste0(2012:2021, ".12")) %>%
  transmute(year = as.numeric(substr(Data, 1, 4)),
            index = as.numeric(index)) %>%
  mutate(IPCA = index / index[year == 2016]) %>%
  select(year, IPCA)

save_proc(ipca, "IPCA")

# ---- 3) US CPI, 2016 = 1 ------------------------------------------------------
us_cpi <- read_excel_safe(file.path(RAW, "Prices", "U.S_CPI.xlsx"), sheet = "Plan1") %>%
  rename(year = Year) %>%
  mutate(US_infl = CPI / CPI[year == 2016]) %>%
  select(year, CPI, US_infl)

save_proc(us_cpi, "US_CPI")
