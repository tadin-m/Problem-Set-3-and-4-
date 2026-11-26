# =============================================================================
# 02 - Correspondence HS 2012 (6-digit) -> CNAE 2.0 industries (tradable)
# Stata: "1 - Correspondence HS4 to CNAE20.do"
#
# Chain: HS 2012 -> CPC 2.1 -> ISIC 4.0 -> CNAE 2.0, then CNAE codes are
# grouped (industries with identical product sets / very similar industries).
# =============================================================================

# HS 2012 -> CPC 2.1
cpc_hs <- read_excel(file.path(CORR, "CPC21 X HS4.xlsx"), sheet = "Plan1", col_types = "text") %>%
  transmute(HS4 = as.numeric(HS12code), CPC21 = as.numeric(CPC21code))

# CPC 2.1 -> ISIC 4.0
cpc_isic <- read_excel(file.path(CORR, "CPC21 X ISIC40.xlsx"), sheet = "Plan1", col_types = "text") %>%
  transmute(CPC21 = as.numeric(CPC21code), ISIC40 = as.numeric(ISIC4code))

# ISIC 4.0 -> CNAE 2.0. CNAE codes look like "0111-3"; Stata's
#   moss CNAE20, match("([0-9]+)") regex ; replace CNAE20 = _match1 + _match2
# glues the first two digit groups together: "0111" + "3" -> 1113
first_two_digit_groups <- function(x) {
  vapply(regmatches(x, gregexpr("[0-9]+", x)),
         function(m) paste(head(m, 2), collapse = ""), character(1))
}
isic_cnae <- read_excel(file.path(CORR, "ISIC40 X CNAE20.xls"), sheet = "Planilha1", col_types = "text")
names(isic_cnae)[1:2] <- c("ISIC40", "CNAE20")
isic_cnae <- isic_cnae %>%
  mutate(ISIC40 = as.numeric(ISIC40),
         CNAE20 = suppressWarnings(as.numeric(first_two_digit_groups(CNAE20)))) %>%
  filter(!is.na(CNAE20))

# CNAE -> grouped industry ("activity")
cnae_group <- read_excel(file.path(CORR, "Corresp - Group - CNAE.xlsx")) %>%
  transmute(CNAE20 = as.numeric(CNAE20), activity = as.numeric(GROUP))
save_proc(cnae_group, "CNAE_Group")

# ---- Merge the chain ----------------------------------------------------------
# joinby CPC21 ... keep if _merge == 3   -> inner (many-to-many) join
hs_isic <- inner_join(cpc_hs, cpc_isic, by = "CPC21", relationship = "many-to-many") %>%
  select(-CPC21) %>%
  distinct()

# joinby ISIC40 ..., drop _merge==1, drop if missing(HS4) -> inner join
hs_cnae <- inner_join(hs_isic, isic_cnae, by = "ISIC40", relationship = "many-to-many") %>%
  select(-ISIC40)

# merge n:1 CNAE20 using CNAE_Group, keeping all rows (Stata only drops _merge)
corr_hs_cnae <- full_join(hs_cnae, cnae_group, by = "CNAE20") %>%
  select(hs6 = HS4, CNAE20 = activity) %>%
  distinct()

save_proc(corr_hs_cnae, "Correspondence_HS4_to_CNAE20_tradable")

# Check reported in the paper/code: share of products mapped to one industry only
n_ind <- corr_hs_cnae %>% filter(!is.na(hs6)) %>% count(hs6)
message(sprintf("HS6 products: %d; mapped to one industry: %d (%.1f%%)",
                nrow(n_ind), sum(n_ind$n == 1), 100 * mean(n_ind$n == 1)))
