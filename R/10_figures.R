# =============================================================================
# 10 - Figures
#   Figure C5/C6: Table 1 Panel A.2 / B.2 coefficients by year (event-study
#               style plot of Table 1; Stata: second half of do-file 16)
# =============================================================================

suppressPackageStartupMessages(library(ggplot2))

# ---- Figures C5 / C6: Table 1 coefficients over time --------------------------------
res <- read_csv(file.path(TABLES, "table1_long.csv"), show_col_types = FALSE) %>%
  filter(panel %in% c("A2", "B2"))

# 2016 is the base year: coefficient zero by construction
base_rows <- expand_grid(panel = c("A2", "B2"), term = c("RTR_TW_CH_Total", "RTR_TW_US_Total")) %>%
  mutate(year = 2016, estimate = 0, se = 0)

# 95% CI with t(G-1) critical value, as in Stata's parmest output
crit <- qt(0.975, df = unique(res$clusters) - 1)

coef_plot_data <- bind_rows(res, base_rows) %>%
  mutate(lo = estimate - crit * se, hi = estimate + crit * se,
         tariffs = recode(term, RTR_TW_CH_Total = "Chinese tariffs", RTR_TW_US_Total = "American tariffs"),
         xpos = year + ifelse(tariffs == "American tariffs", 0.2, 0),
         outcome = recode(panel, A2 = "Formal employment", B2 = "Formal wage bill"))

figC <- ggplot(coef_plot_data, aes(xpos, estimate, colour = tariffs)) +
  geom_hline(yintercept = 0, colour = "grey30") +
  geom_vline(xintercept = 2016.6, linetype = "dotted", colour = "grey50") +
  geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.12) +
  geom_point(size = 2) +
  facet_wrap(~ outcome, ncol = 1) +
  scale_colour_manual(values = c("Chinese tariffs" = "#c0392b", "American tariffs" = "#1f4e9c")) +
  scale_x_continuous(breaks = 2012:2021) +
  labs(x = "Year t (outcome measured as change from 2016 to t)",
       y = "Effect of 1 SD higher exposure (log points)", colour = NULL) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank())

ggsave(file.path(FIGS, "figureC5_C6_coefficients.pdf"), figC, width = 7, height = 6)
ggsave(file.path(FIGS, "figureC5_C6_coefficients.png"), figC, width = 7, height = 6, dpi = 200, bg = "white")
