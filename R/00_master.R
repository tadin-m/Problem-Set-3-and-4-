# =============================================================================
# Replication in R of:
#   Cavalcanti, Ogeda & Ornelas (2026), "The US-China trade war creates jobs
#   (elsewhere)", Journal of International Economics 161, 104249.
#
# Master script: runs the whole pipeline from the authors' raw data to Table 1.
# Translated from the authors' Stata replication package (master-Replication.do).
#
# HOW TO RUN
#   1. Copy the "Data" folder of the authors' replication package
#      (https://doi.org/10.17632/2zmtf6xggh.1) into the project root, so that
#      it contains:  Data/   R/   output/
#   2. Source this script (RStudio "Source" button or source("R/00_master.R")).
#      The working directory is set automatically.
#
# Mapping to the authors' do-files:
#   01_correspondences_prices.R  <- aux codes "0 - Correspondence - municipality to mmc.do"
#                                   and "0 - Prices Index.do"
#   02_hs_to_cnae.R              <- "1 - Correspondence HS4 to CNAE20.do"
#   03_tariffs.R                 <- "2 - Trade War Tariff Changes.do" and
#                                   "3 - Constructing TW_Tariffs at 2012 HS code.do"
#   04_trade.R                   <- "4 - clean_trade_data_2016.do" (import flows only)
#   05_employment.R              <- "8 - Clean Employment.do"
#   06_gdp.R                     <- "9 - Clean GDP.do"
#   07_industry_tariffs.R        <- "10 - Tariff_Cnae20.do"
#   08_rtw.R                     <- "11 - RTW.do"
#   09_table1.R                  <- "16 -Tab 1 and Fig C5 C6 - Results Employment.do"
#   10_figures.R                 <- Figure C5/C6 part of do-file 16
#   11_latex_tables.R            LaTeX versions of Table 1 for the report
#   12_exports.R                 <- "13 - clean Brazil Exports.do"
#   13_figure2.R                 <- "15 - Fig 2 - CS_did_exp.do" (Callaway-Sant'Anna)
#   14_extension.R               (new) Table 2: extension (soybean exposure)
#   15_controls_sensitivity.R    (new) Section 4.1: coefficient without controls
#
# Do-files 5-7 need the confidential RAIS microdata and are NOT translated:
# like the authors, we start from the microregion-level RAIS files they provide
# in Data/RAIS Data.
# =============================================================================

rm(list = ls())

# ---- Working directory ----------------------------------------------------------
# The project root is the folder that contains R/ (and Data/, output/). It is found
# from the location of this script, so no fixed path is needed. Works when the
# script is run with source("R/00_master.R") or the "Source" button in RStudio.
script_dir <- tryCatch(dirname(sys.frame(1)$ofile), error = function(e) NULL)
if (is.null(script_dir) && requireNamespace("rstudioapi", quietly = TRUE) &&
    rstudioapi::isAvailable()) {
  script_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
}
if (!is.null(script_dir) && nzchar(script_dir)) setwd(file.path(script_dir, ".."))
if (!dir.exists("Data")) stop("Data/ folder not found. Put the authors' Data folder next to R/.")

# ---- Packages -----------------------------------------------------------------
# All packages used in the project. Any that are missing are installed
# automatically the first time you run this script.
packages <- c(
  "haven",     # read Stata .dta files
  "readxl",    # read Excel files (.xls / .xlsx)
  "readr",     # read csv / txt files
  "dplyr",     # data manipulation
  "tidyr",     # reshaping (wide <-> long)
  "sandwich",  # clustered standard errors
  "lmtest",    # coefficient tests with clustered SEs
  "ggplot2",   # figures
  "scales"     # axis formatting in figures
)

to_install <- packages[!packages %in% rownames(installed.packages())]
if (length(to_install) > 0) {
  install.packages(to_install, repos = "https://cloud.r-project.org")
}

invisible(lapply(packages, library, character.only = TRUE))

source("R/functions.R")

scripts <- c(
  "01_correspondences_prices.R",
  "02_hs_to_cnae.R",
  "03_tariffs.R",
  "04_trade.R",
  "05_employment.R",
  "06_gdp.R",
  "07_industry_tariffs.R",
  "08_rtw.R",
  "09_table1.R",
  "10_figures.R",
  "11_latex_tables.R",
  "12_exports.R",
  "13_figure2.R",
  "14_extension.R",
  "15_controls_sensitivity.R"
)

for (s in scripts) {
  message("\n=== Running ", s, " ===")
  source(file.path("R", s), local = new.env())
}

message("\nDone. Replication: output/tables and output/figures. Extension: output/extension.")
