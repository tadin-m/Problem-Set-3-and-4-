# =============================================================================
# Shared paths and helper functions
# =============================================================================

suppressPackageStartupMessages({
  library(haven)
  library(readxl)
  library(readr)
  library(dplyr)
  library(tidyr)
})

# ---- Paths (all relative to the project root) -------------------------------
DATA   <- "Data"                        # authors' Data folder (never modified)
RAW    <- file.path(DATA, "Raw Data")
CORR   <- file.path(DATA, "Correspondence")
RAIS   <- file.path(DATA, "RAIS Data")
PROC   <- file.path("output", "processed")   # our intermediate files
TABLES <- file.path("output", "tables")
FIGS   <- file.path("output", "figures")
EXT    <- file.path("output", "extension")      # our extension (Table 2 and extra checks)
for (d in c(PROC, TABLES, FIGS, EXT)) dir.create(d, recursive = TRUE, showWarnings = FALSE)

# ---- Helpers ----------------------------------------------------------------

# Save / load intermediate datasets (R's own format)
save_proc <- function(x, name) saveRDS(x, file.path(PROC, paste0(name, ".rds")))
load_proc <- function(name) readRDS(file.path(PROC, paste0(name, ".rds")))

# Stata pads 5-digit HS codes with one leading zero:
#   tostring hs6, replace
#   replace hs6 = "0" + hs6 if missing(substr(hs6,6,1))
pad_hs6 <- function(x) {
  x <- as.character(x)
  ifelse(nchar(x) == 5, paste0("0", x), x)
}

# Stata's collapse (sum): missing values count as zero, and a group with only
# missing values gets 0 (not missing).
sum0 <- function(x) sum(x, na.rm = TRUE)

# Stata's collapse (mean): ignores missing; all-missing group -> missing
mean_na <- function(x) if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)

# Stata turns x/0 into missing; R gives Inf or NaN. Make it missing.
safe_div <- function(a, b) {
  b <- rep_len(b, length(a))            # also works when b is a single number
  ifelse(is.na(b) | b == 0, NA_real_, a / b)
}

# Some file names contain accents (e.g. "Municípios"). On some systems the
# accent is stored differently, so we look the file up by a pattern instead.
find_file <- function(dir, pattern) {
  f <- list.files(dir, pattern = pattern, full.names = TRUE)
  if (length(f) != 1) stop("Could not find a unique file matching ", pattern, " in ", dir)
  f
}

# readxl cannot always open .xls files whose path contains accents.
# Copy to a temporary plain-ASCII path first (harmless if not needed).
read_excel_safe <- function(path, ...) {
  tmp <- tempfile(fileext = paste0(".", tools::file_ext(path)))
  file.copy(path, tmp)
  on.exit(unlink(tmp))
  read_excel(tmp, ...)
}
