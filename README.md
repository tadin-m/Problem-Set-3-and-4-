# PS3: Replication of Cavalcanti, Ogeda & Ornelas (2026)

Elia Sanjuan, Roko Rosic, Tadin Morina. Empirical Methods, University of Zurich, HS 2026.

R replication of "The US-China trade war creates jobs (elsewhere)", *Journal of International Economics* 161, 104249 (<https://doi.org/10.1016/j.jinteco.2026.104249>). The authors' original package is written in Stata. We translated it to R and extended it.

## Data

The repository does **not** contain the data. The data are part of the authors' replication package, which is too large for GitHub (over 1 GB). We use it unchanged:

1. Download the authors' replication package from Mendeley Data: <https://doi.org/10.17632/2zmtf6xggh.1> (CC BY 4.0).
2. Copy its `Data` folder into the root of this repository, next to `R/`.


## Folder structure

```
├── README.md
├── Data/                authors' data, added by the user (see above)
├── R/                   code
└── output/              created by the code
    ├── tables/          Table 1, Figure 2 data
    ├── figures/         Figure 2 (exports), Figures C.5/C.6 (employment)
    ├── extension/       Table 2 and Section 4.1 results
    └── processed/       intermediate datasets (not on GitHub)
```

## How to run

1. Open `R/00_master.R` in RStudio and click **Source**. The working directory is set automatically, missing packages are installed, and all scripts run in order (a few minutes, mostly for reading the export data).

## Scripts

| Script | Content | Output in report | Authors' do-file |
|---|---|---|---|
| 01_correspondences_prices.R | Municipality codes, price indices | | 0 (aux) |
| 02_hs_to_cnae.R | Product-to-industry correspondence | | 1 |
| 03_tariffs.R | Trade-war and MFN tariffs | | 2, 3 |
| 04_trade.R | 2016 trade flows | | 4 |
| 05_employment.R | Employment and wage bill changes | | 8 |
| 06_gdp.R | 2016 GDP | | 9 |
| 07_industry_tariffs.R | Industry-level tariffs | | 10 |
| 08_rtw.R | Exposure measure (Eq. 3-4) | | 11 |
| 09_table1.R | Regressions of Eq. (5) | Table 1 | 16 |
| 10_figures.R | Coefficients by year | Figure 2 | 16 |
| 11_latex_tables.R | LaTeX version of Table 1 | Table 1 | - |
| 12_exports.R | Monthly exports | | 13 |
| 13_figure2.R | Export event study (Callaway-Sant'Anna) | Figure 1 | 15 |
| 14_extension.R | Exposure without soybeans / soybeans only | Table 2, Section 5.1 | - |
| 15_controls_sensitivity.R | Coefficient without controls | Section 4.1 | - |

Do-files 5-7 need the confidential RAIS microdata. Like the authors, we start from their regional RAIS aggregates in `Data/RAIS Data`.

## Software

R 4.3.3 with haven 2.5.4, readxl 1.4.3, readr 2.1.5, dplyr 1.1.4, tidyr 1.3.1, sandwich 3.1.0, lmtest 0.9.40, ggplot2 3.4.4, scales 1.3.0. No random numbers are used.

