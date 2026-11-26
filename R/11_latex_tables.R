# =============================================================================
# 11 - LaTeX versions of the regression tables (for the report)
#
#   table1.tex        : full Table 1 in the paper's layout (6 panels x 9 years)
#   table1_main.tex   : standard OLS regression table for the main column
#                       (t = 2021), showing all controls, FE, N, clusters, R^2
#
# Both files contain only the tabular environment (needs \usepackage{booktabs}),
# so they can be pasted into the report template with \input{...}.
# =============================================================================

suppressPackageStartupMessages({ library(sandwich); library(lmtest) })

df  <- load_proc("analysis_dataset")
res <- read_csv(file.path(TABLES, "table1_long.csv"), show_col_types = FALSE)

stars <- function(p) ifelse(p < 0.01, "^{**}", ifelse(p < 0.05, "^{*}", ""))
num   <- function(x, d = 3) formatC(x, format = "f", digits = d)
cell  <- function(b, p, d = 3) paste0("$", num(b, d), stars(p), "$")
secell <- function(s, d = 3) paste0("(", num(s, d), ")")

# ---- 1) Full Table 1, paper layout ---------------------------------------------------
years <- c(2021, 2020, 2019, 2018, 2017, 2015, 2014, 2013, 2012)

panel_titles <- c(
  A1 = "\\textit{A.1 Main specification}",
  A2 = "\\textit{A.2 Including U.S. tariffs}",
  A3 = "\\textit{A.3 Including MFN controls}",
  B1 = "\\textit{B.1 Main specification}",
  B2 = "\\textit{B.2 Including U.S. tariffs}",
  B3 = "\\textit{B.3 Including MFN controls}"
)
term_label <- c(RTR_TW_CH_Total = "$RTW_r^{CH}$", RTR_TW_US_Total = "$RTW_r^{US}$")

rows_for_panel <- function(pan) {
  r <- res %>% filter(panel == pan)
  out <- paste0(panel_titles[[pan]], " \\\\")
  for (tm in unique(r$term)) {
    rt <- r %>% filter(term == tm) %>% arrange(match(year, years))
    out <- c(out,
             paste(term_label[[tm]], paste(cell(rt$estimate, rt$p), collapse = " & "), sep = " & "),
             paste0(paste("", paste(secell(rt$se), collapse = " & "), sep = " & "), " \\\\[2pt]"))
    out[length(out) - 1] <- paste0(out[length(out) - 1], " \\\\")
  }
  out
}

t1 <- c(
  "\\begin{tabular}{l*{9}{c}}",
  "\\toprule",
  "Dep. var.: $\\Delta_{16}\\log(Y_t)$ & \\multicolumn{5}{c}{After trade war} & \\multicolumn{4}{c}{Before trade war} \\\\",
  "\\cmidrule(lr){2-6}\\cmidrule(lr){7-10}",
  paste0(" & ", paste(sprintf("(%d)", 1:9), collapse = " & "), " \\\\"),
  paste0(" & ", paste(sprintf("$t=%d$", years), collapse = " & "), " \\\\"),
  "\\midrule",
  "\\multicolumn{10}{l}{\\textbf{Panel A. Formal employment}} \\\\",
  rows_for_panel("A1"), rows_for_panel("A2"), rows_for_panel("A3"),
  "\\midrule",
  "\\multicolumn{10}{l}{\\textbf{Panel B. Formal wage bill}} \\\\",
  rows_for_panel("B1"), rows_for_panel("B2"), rows_for_panel("B3"),
  "\\midrule",
  paste0("Observations & ", paste(rep(unique(res$N), 9), collapse = " & "), " \\\\"),
  "\\bottomrule",
  "\\end{tabular}"
)
writeLines(t1, file.path(TABLES, "table1.tex"))

# ---- 2) Main column (t = 2021) as a standard OLS regression table -----------------
ctrl_vars <- c("fem", "hs_comp", "young", "GDP", "share_agric")
d21 <- filter(df, year == 2021)

fit_one <- function(outcome, x) {
  fml <- as.formula(paste(outcome, "~", paste(c(x, ctrl_vars), collapse = " + "), "+ factor(uf)"))
  fit <- lm(fml, data = d21)
  G   <- length(unique(d21$mesoreg))
  ct  <- coeftest(fit, vcov. = vcovCL(fit, cluster = ~ mesoreg, type = "HC1"), df = G - 1)
  list(ct = ct, N = nobs(fit), G = G, r2 = summary(fit)$r.squared)
}

specs <- list(
  list(y = "d_employ",  x = "RTR_TW_CH_Total"),
  list(y = "d_employ",  x = c("RTR_TW_CH_Total", "RTR_TW_US_Total")),
  list(y = "d_remdezr", x = "RTR_TW_CH_Total"),
  list(y = "d_remdezr", x = c("RTR_TW_CH_Total", "RTR_TW_US_Total"))
)
fits <- lapply(specs, function(s) fit_one(s$y, s$x))

row_labels <- c(
  RTR_TW_CH_Total = "$RTW_r^{CH}$ (Chinese tariffs)",
  RTR_TW_US_Total = "$RTW_r^{US}$ (U.S. tariffs)",
  fem             = "Share female workers",
  hs_comp         = "Share with high school degree",
  young           = "Share under 30",
  GDP             = "GDP 2016",
  share_agric     = "Share in agriculture"
)

coef_rows <- unlist(lapply(names(row_labels), function(v) {
  dg <- if (v == "GDP") 4 else 3   # GDP coefficient is tiny: show 4 decimals
  b  <- sapply(fits, function(f) if (v %in% rownames(f$ct)) cell(f$ct[v, 1], f$ct[v, 4], dg) else "")
  se <- sapply(fits, function(f) if (v %in% rownames(f$ct)) secell(f$ct[v, 2], dg) else "")
  c(paste0(row_labels[[v]], " & ", paste(b, collapse = " & "), " \\\\"),
    paste0(" & ", paste(se, collapse = " & "), " \\\\[2pt]"))
}))

t2 <- c(
  "\\begin{tabular}{l*{4}{c}}",
  "\\toprule",
  " & \\multicolumn{2}{c}{$\\Delta_{16}\\log(\\text{Formal employment})$} & \\multicolumn{2}{c}{$\\Delta_{16}\\log(\\text{Wage bill})$} \\\\",
  "\\cmidrule(lr){2-3}\\cmidrule(lr){4-5}",
  " & (1) & (2) & (3) & (4) \\\\",
  "\\midrule",
  coef_rows,
  "\\midrule",
  "State fixed effects & Yes & Yes & Yes & Yes \\\\",
  paste0("Observations & ", paste(sapply(fits, `[[`, "N"), collapse = " & "), " \\\\"),
  paste0("Clusters (mesoregions) & ", paste(sapply(fits, `[[`, "G"), collapse = " & "), " \\\\"),
  paste0("$R^2$ & ", paste(num(sapply(fits, `[[`, "r2")), collapse = " & "), " \\\\"),
  "\\bottomrule",
  "\\end{tabular}"
)
writeLines(t2, file.path(TABLES, "table1_main.tex"))

# Also print the main table in the console
for (i in seq_along(fits)) {
  cat(sprintf("\n(%d) %s on %s: R2 = %.3f\n", i, specs[[i]]$y,
              paste(specs[[i]]$x, collapse = " + "), fits[[i]]$r2))
  print(round(fits[[i]]$ct[c(specs[[i]]$x, ctrl_vars), ], 4))
}
