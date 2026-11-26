# =============================================================================
# 13 - Figure 2: effect of trade-war tariffs on Brazil's exports
#      Callaway & Sant'Anna (2021) staggered difference-in-differences
# Stata: "Main Analysis/15 - Fig 2 - CS_did_exp.do" (csdid + estat event)
#
# Setting: HS6 products p, months t = 1 (Jan 2017), ..., 60 (Dec 2021).
#   Group g = month in which product p first faces a trade-war tariff.
#   Control group = products never targeted (g = 0).
#   Outcome Y_pt = log(1 + exports) to China or to the US (value or kg).
#
# Equation (1) has no covariates, so csdid's estimator reduces to a simple
# difference in mean changes:
#   t >= g :  ATT(g,t) = [Ybar_g(t) - Ybar_g(g-1)] - [Ybar_C(t) - Ybar_C(g-1)]
#   t <  g :  ATT(g,t) = [Ybar_g(t) - Ybar_g(t-1)] - [Ybar_C(t) - Ybar_C(t-1)]
#             (pre-periods use the previous month as base: csdid's default)
# Equation (2), event time k = t - g:
#   theta_k = sum_g W_g ATT(g, g+k),  W_g = group size share among the groups
#             observed at event time k
# Standard errors: influence functions, summed within HS4 clusters, including
# the estimation error in the weights W_g (as csdid does).
# =============================================================================

suppressPackageStartupMessages(library(ggplot2))

h6  <- load_proc("h6_tariff")
exp <- load_proc("exp_clean")

# ---- 1) Month of first tariff for each product ------------------------------------
# Month each wave enters the treatment indicator in do-file 15 (period 1 = Jan 2017).
# NOTE: here the 6 July 2018 waves start in July (period 19), while Figure 1's
# code puts them in June.
wave_period <- c(
  UStariff23032018 = 15, CHtariff02042018 = 16,
  UStariff06072018 = 19, CHtariff06072018 = 19,
  UStariff23082018 = 20, CHtariff23082018 = 20,
  UStariff24092018 = 21, CHtariff24092018 = 21,
  UStariff09052019 = 29, CHtariff13052019 = 29,
  UStariff01092019 = 33, CHtariff01092019 = 33,
  UStariff15122019 = 36, CHtariff15122019 = 36
)

first_month <- function(cols) {
  m <- sapply(cols, function(v) ifelse(h6[[v]] != 0, wave_period[[v]], Inf))
  out <- apply(m, 1, min)
  ifelse(is.infinite(out), 0, out)            # 0 = never treated
}

treat <- tibble(
  hs6 = as.numeric(h6$hs6),
  # Authors' treatment: a tariff by China OR by the US ("treat" in do-file 15)
  g_any = first_month(names(wave_period)),
  # Separate first-tariff months by country (used only for descriptives here)
  g_CH  = first_month(grep("^CH", names(wave_period), value = TRUE)),
  g_US  = first_month(grep("^US", names(wave_period), value = TRUE))
)

# ---- 2) Merge with exports (keep all products from both files) -----------------------
# Products with exports but no tariff data -> never treated;
# products with tariff data but never exported -> exports = 0 in every month.
panel <- full_join(
  expand_grid(hs6 = treat$hs6, period = 1:60) %>% left_join(treat, by = "hs6"),
  exp %>% select(-year, -month),
  by = c("hs6", "period")
) %>%
  mutate(across(c(g_any, g_CH, g_US), ~ replace_na(.x, 0)),
         across(c(kg_liquido, vl_fob, vl_fob_CH, kg_liquido_CH, vl_fob_US, kg_liquido_US), ~ replace_na(.x, 0)),
         hs4 = hs6 %/% 100,
         log_fob_CH = log(1 + vl_fob_CH), log_kg_CH = log(1 + kg_liquido_CH),
         log_fob_US = log(1 + vl_fob_US), log_kg_US = log(1 + kg_liquido_US)) %>%
  arrange(hs6, period)

save_proc(panel, "CSDID_dataset")

# ---- 3) Callaway & Sant'Anna estimator --------------------------------------------------
cs_event_study <- function(panel, yvar, gvar) {
  units <- panel %>% distinct(hs6, hs4, g = .data[[gvar]]) %>% arrange(hs6)
  Y <- matrix(panel[[yvar]], ncol = 60, byrow = TRUE)          # rows = products, cols = months
  stopifnot(nrow(Y) == nrow(units))
  N <- nrow(units); Tn <- ncol(Y)
  G <- units$g
  groups <- sort(unique(G[G > 0]))
  ctrl <- G == 0
  p_c  <- mean(ctrl)

  # ATT(g,t) and its influence function
  att <- list()
  for (g in groups) {
    D <- G == g; p_g <- mean(D)
    for (t in 2:Tn) {
      base <- if (t >= g) g - 1 else t - 1
      dY   <- Y[, t] - Y[, base]
      mu_g <- mean(dY[D]); mu_c <- mean(dY[ctrl])
      psi  <- D * (dY - mu_g) / p_g - ctrl * (dY - mu_c) / p_c
      att[[length(att) + 1]] <- list(g = g, t = t, e = t - g, att = mu_g - mu_c, psi = psi)
    }
  }
  info <- tibble(g = sapply(att, `[[`, "g"), t = sapply(att, `[[`, "t"),
                 e = sapply(att, `[[`, "e"), att = sapply(att, `[[`, "att"))
  PSI  <- sapply(att, `[[`, "psi")                             # N x (#ATT(g,t))
  p_grp <- sapply(groups, function(g) mean(G == g)); names(p_grp) <- groups

  # Clustered variance of an estimator with influence function psi
  clustered_se <- function(psi) sqrt(sum(rowsum(psi, units$hs4)^2)) / N

  # Aggregate to event time e (Eq. 2), with the weight-estimation term
  agg_psi <- function(idx) {
    gs  <- info$g[idx]; pg <- p_grp[as.character(gs)]; P <- sum(pg)
    w   <- pg / P
    est <- sum(w * info$att[idx])
    inS <- G %in% gs
    psi_w <- sapply(seq_along(gs), function(j) ((G == gs[j]) - pg[j]) / P - pg[j] * (inS - P) / P^2)
    psi <- PSI[, idx, drop = FALSE] %*% w + psi_w %*% info$att[idx]
    list(est = est, psi = as.vector(psi))
  }

  es <- lapply(sort(unique(info$e)), function(k) {
    a <- agg_psi(which(info$e == k)); c(e = k, est = a$est, se = clustered_se(a$psi), list(psi = a$psi))
  })
  out <- tibble(e = sapply(es, `[[`, "e"), estimate = sapply(es, `[[`, "est"), se = sapply(es, `[[`, "se"))

  # Pre- and post-period averages (csdid: estat event, Pre_avg / Post_avg)
  avg <- function(sel) {
    psi <- rowMeans(sapply(es[sel], `[[`, "psi"))
    c(estimate = mean(out$estimate[sel]), se = clustered_se(psi))
  }
  list(event = out,
       pre_avg  = avg(which(out$e < 0)),
       post_avg = avg(which(out$e >= 0)),
       n_units = N, n_control = sum(ctrl), n_clusters = length(unique(units$hs4)),
       group_sizes = table(G))
}

outcomes <- c(A1 = "log_fob_CH", A2 = "log_fob_US", B1 = "log_kg_CH", B2 = "log_kg_US")
fits <- lapply(outcomes, function(y) cs_event_study(panel, y, "g_any"))

event <- bind_rows(lapply(names(fits), function(p) mutate(fits[[p]]$event, panel = p)))
write_csv(event, file.path(TABLES, "figure2_event_study.csv"))

averages <- bind_rows(lapply(names(fits), function(p) tibble(
  panel = p, outcome = outcomes[[p]],
  pre_avg = fits[[p]]$pre_avg["estimate"],  pre_se = fits[[p]]$pre_avg["se"],
  post_avg = fits[[p]]$post_avg["estimate"], post_se = fits[[p]]$post_avg["se"])))
write_csv(averages, file.path(TABLES, "figure2_averages.csv"))
print(as.data.frame(averages))

f <- fits[[1]]
message(sprintf("Products: %d (never treated: %d), HS4 clusters: %d", f$n_units, f$n_control, f$n_clusters))

# ---- 4) Figure 2 (same layout as the paper: 2 x 2 panels) --------------------------------
panel_titles <- c(A1 = "Panel A.1 - Exports to China (value)", A2 = "Panel A.2 - Exports to US (value)",
                  B1 = "Panel B.1 - Exports to China (quantity)", B2 = "Panel B.2 - Exports to US (quantity)")

fig2_data <- event %>%
  mutate(lo = estimate - 1.96 * se, hi = estimate + 1.96 * se,        # 95% CI
         timing = factor(ifelse(e < 0, "Pre-treatment", "Post-treatment"),
                         levels = c("Pre-treatment", "Post-treatment")),
         title = factor(panel_titles[panel], levels = panel_titles))

# y-axis ranges used by the authors: [-1, 3] for value, [-2, 2] for quantity
y_ranges <- tibble(panel = names(panel_titles), title = factor(panel_titles, levels = panel_titles),
                   ymin = c(-1, -1, -2, -2), ymax = c(3, 3, 2, 2))

fig2 <- ggplot(fig2_data, aes(e, estimate, colour = timing)) +
  geom_blank(data = y_ranges, aes(x = 0, y = ymin), inherit.aes = FALSE) +
  geom_blank(data = y_ranges, aes(x = 0, y = ymax), inherit.aes = FALSE) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "black") +
  geom_linerange(aes(ymin = lo, ymax = hi), linewidth = 1.6, alpha = 0.45) +
  geom_point(size = 1.1) +
  facet_wrap(~ title, ncol = 2, scales = "free_y") +
  scale_colour_manual(values = c("Pre-treatment" = "#2e7d32", "Post-treatment" = "#e8541e")) +
  scale_x_continuous(breaks = seq(-30, 40, 10)) +
  labs(x = "Months relative to tariff increases", y = "Change in log(1 + exports)", colour = NULL) +
  theme_minimal(base_size = 10) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank(),
        strip.text = element_text(face = "bold"))

ggsave(file.path(FIGS, "figure2_exports.pdf"), fig2, width = 9, height = 6.5)
ggsave(file.path(FIGS, "figure2_exports.png"), fig2, width = 9, height = 6.5, dpi = 200, bg = "white")
