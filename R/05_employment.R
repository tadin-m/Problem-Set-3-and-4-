# =============================================================================
# 05 - Microregion panel of formal employment and wage bill, 2012-2021, and
#      log changes relative to 2016.
# Stata: "8 - Clean Employment.do"
# Input: municipality-level RAIS aggregates provided by the authors.
# =============================================================================

munic <- load_proc("munic")
ipca  <- load_proc("IPCA")

rais <- bind_rows(lapply(2012:2021, function(y)
  read_dta(file.path(RAIS, sprintf("employment_RAIS_%d.dta", y))) %>% zap_labels()))

emp <- rais %>%
  rename(munic = municipio) %>%
  inner_join(munic, by = "munic") %>%     # drops municipality 999999 (no region)
  group_by(microreg, year) %>%
  summarise(employ  = sum0(empem3112),
            remdezr = sum0(remdezr),      # December wage bill
            mesoreg = first(mesoreg),
            uf      = first(uf),
            .groups = "drop") %>%
  left_join(ipca, by = "year") %>%
  mutate(remdezr = remdezr / IPCA) %>%    # deflate to 2016 prices
  select(-IPCA)

save_proc(emp %>% mutate(avr_wage = remdezr / employ), "Employment_Data_Descriptive")

# Log change relative to 2016: d_x(t) = ln(x_t) - ln(x_2016)
base <- emp %>% filter(year == 2016) %>% select(microreg, employ16 = employ, remdezr16 = remdezr)

delta <- emp %>%
  select(microreg, year, employ, remdezr) %>%
  complete(microreg, year = 2012:2021) %>%   # balanced, like Stata's reshape wide/long
  left_join(base, by = "microreg") %>%
  left_join(distinct(emp, microreg, mesoreg, uf), by = "microreg") %>%
  mutate(d_employ  = log(employ)  - log(employ16),
         d_remdezr = log(remdezr) - log(remdezr16)) %>%
  select(microreg, year, mesoreg, uf, d_employ, d_remdezr) %>%
  arrange(microreg, year)

save_proc(delta, "Delta_employment")
