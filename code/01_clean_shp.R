# ==============================================================================
# 01_clean_shp.R  —  Load and clean SHP long-format panel data
# ==============================================================================
# Input:  swiss-household-panel/data/SHP-Data-Longfile-STATA/
# Output: data/processed/shp_panel_clean.rds
# ==============================================================================

library(tidyverse)
library(haven)

cat("=== 01_clean_shp.R ===\n")

# --- Paths -------------------------------------------------------------------
base <- "g:/My Drive/sandbox/ai-paper"
long_p <- file.path(base, "swiss-household-panel/data/SHP-Data-Longfile-STATA/shplong_p_user.dta")
long_h <- file.path(base, "swiss-household-panel/data/SHP-Data-Longfile-STATA/shplong_h_user.dta")
out_dir <- file.path(base, "data/processed")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# --- Load person-level long file ---------------------------------------------
cat("Loading person long file...\n")
person <- read_dta(long_p, col_select = c(
  # IDs & demographics
  "idpers", "year", "idhous", "sex", "age", "ownkid",
  "educat", "isced", "edyear", "plingu", "wstat", "occupa",
  # Work hours
  "pw74", # contractual hours per week
  "pw77", # hours actually worked per week
  "pw46", # reference hours
  "pw42", # part-time percentage
  "pw610", # total hours worked all jobs
  # Job characteristics
  "pw29", # type of employment
  "pw34a", # management/supervision
  "pw604", # work stress
  "pw71b", # working-time autonomy
  "pw80a", # work at home frequency
  "pw87", # supervisory tasks
  "pw228", # job satisfaction (0-10)
  "pw92", # satisfaction: income
  "pw93", # satisfaction: work conditions
  "pw94", # satisfaction: work atmosphere
  "pw229", # satisfaction: interest in tasks
  "pw230", # satisfaction: amount of work
  "pw101", # risk of unemployment
  # Work-life conflict
  "pf50", # work interferes with private life
  "pf51", # exhausted after work
  "pf52", # difficulty disconnecting
  # Well-being
  "pc44", # life satisfaction (0-10)
  "pc02", # satisfaction with health
  "pi01", # satisfaction with financial situation
  # Leisure
  "pa05", # satisfaction with free time
  "pa06", # satisfaction with leisure activities
  # Gender attitudes
  "pd91", # job preserves independence
  "pd92", # child suffers with working mother
  # Income
  "iwyn", # yearly work income net
  "iwyg", # yearly work income gross
  "iempmn", # monthly income from employment net
  "iempmg" # monthly income from employment gross
))
cat("  Person file:", nrow(person), "rows x", ncol(person), "cols\n")

# --- Load household-level long file ------------------------------------------
cat("Loading household long file...\n")
hh <- read_dta(long_h, col_select = c(
  "idhous", "year",
  "canton", "region", "hlingu",
  "com1_", "com2_", "com3_",
  "nbkid", "nbpers",
  "ihtyg", "ihtyn", # HH income gross/net
  "idispy" # disposable HH income
))
cat("  HH file:", nrow(hh), "rows x", ncol(hh), "cols\n")

# --- Merge person + household ------------------------------------------------
cat("Merging person + HH on idhous x year...\n")
panel <- person %>%
  left_join(hh, by = c("idhous", "year"))
cat("  Merged panel:", nrow(panel), "rows\n")

# --- Recode SHP missing values -----------------------------------------------
# SHP convention: negative values = missing (see metadata)
#   -8 = other error, -7 = filter error, -6 to -5 = special,
#   -4 = working at home, -3 = inapplicable, -2 = no answer, -1 = don't know
cat("Recoding SHP missing values (negative codes -> NA)...\n")
panel <- panel %>%
  mutate(across(where(is.numeric), ~ if_else(. < 0, NA_real_, as.numeric(.))))

# --- Filter to working-age employed sample -----------------------------------
cat("Filtering to employed, ages 18-65...\n")
panel <- panel %>%
  filter(
    age >= 18, age <= 65,
    wstat %in% c(1, 2, 3) # 1=full-time, 2=part-time, 3=irregular/occasional
  )
cat("  After filter:", nrow(panel), "rows\n")
cat("  Year range:", min(panel$year, na.rm = TRUE), "-", max(panel$year, na.rm = TRUE), "\n")
cat("  Unique individuals:", n_distinct(panel$idpers), "\n")

# --- Strip Stata labels for clean R objects -----------------------------------
panel <- panel %>%
  mutate(across(where(is.labelled), ~ as.numeric(.)))

# --- Save ---------------------------------------------------------------------
out_path <- file.path(out_dir, "shp_panel_clean.rds")
saveRDS(panel, out_path)
cat("Saved:", out_path, "\n")
cat("  Final dimensions:", nrow(panel), "rows x", ncol(panel), "cols\n")
cat("=== Done ===\n")
