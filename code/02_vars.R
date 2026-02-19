# ==============================================================================
# 02_vars.R  —  Construct analysis variables
# ==============================================================================
# Input:  data/processed/shp_panel_clean.rds
# Output: data/processed/shp_analysis.rds
# ==============================================================================

library(tidyverse)

cat("=== 02_vars.R ===\n")

base <- "g:/My Drive/sandbox/ai-paper"
panel <- readRDS(file.path(base, "data/processed/shp_panel_clean.rds"))
cat("Loaded panel:", nrow(panel), "rows\n")

# --- Cultural region ----------------------------------------------------------
# plingu: 1 = German, 2 = French  (Italian and unknown dropped)
panel <- panel %>%
  filter(plingu %in% c(1, 2)) %>%
  mutate(
    is_french = if_else(plingu == 2, 1L, 0L),
    is_latin  = is_french, # alias for regression compat
    lang_cat  = if_else(plingu == 1, "German", "French")
  )

cat("  Language distribution:\n")
print(table(panel$lang_cat, useNA = "ifany"))

# --- Hours gap (key dependent variable) ---------------------------------------
panel <- panel %>%
  mutate(
    hours_gap      = pw77 - pw74, # actual - contractual
    hours_gap_ref  = pw77 - pw46, # actual - reference
    wants_less     = if_else(hours_gap > 0, 1L, 0L),
    overwork_5h    = if_else(hours_gap > 5, 1L, 0L)
  )

cat("\n  hours_gap summary:\n")
print(summary(panel$hours_gap))
cat("  wants_less distribution:\n")
print(table(panel$wants_less, useNA = "ifany"))

# --- Cohort dummies -----------------------------------------------------------
# Approximate birth year from age and survey year
panel <- panel %>%
  mutate(
    birth_year = year - age,
    cohort = case_when(
      birth_year >= 1997 ~ "Gen Z",
      birth_year >= 1981 ~ "Millennial",
      birth_year >= 1965 ~ "Gen X",
      birth_year >= 1946 ~ "Boomer",
      TRUE ~ "Silent"
    ),
    cohort = factor(cohort, levels = c("Boomer", "Gen X", "Millennial", "Gen Z", "Silent"))
  )

cat("\n  Cohort distribution:\n")
print(table(panel$cohort, useNA = "ifany"))

# --- Control variables --------------------------------------------------------
panel <- panel %>%
  mutate(
    female = if_else(sex == 2, 1L, 0L),
    age_sq = age^2,
    edu_cat = case_when(
      educat %in% c(1, 2, 3, 4) ~ "Compulsory",
      educat %in% c(5, 6, 7, 8) ~ "Secondary",
      educat %in% c(9, 10, 11) ~ "Tertiary",
      TRUE ~ NA_character_
    ),
    edu_cat = factor(edu_cat, levels = c("Compulsory", "Secondary", "Tertiary")),
    has_kids = if_else(!is.na(nbkid) & nbkid > 0, 1L, 0L),
    # Period indicators
    period = case_when(
      year <= 2019 ~ "Pre-pandemic",
      year == 2020 ~ "2020",
      year >= 2021 ~ "Post-pandemic"
    ),
    post_2020 = if_else(year >= 2021, 1L, 0L)
  )

# --- Canton coding ------------------------------------------------------------
# Map canton numbers to abbreviations for maps
canton_map <- tibble::tribble(
  ~canton, ~canton_abbr, ~canton_name,
  1, "ZH", "Zürich",
  2, "BE", "Bern",
  3, "LU", "Luzern",
  4, "UR", "Uri",
  5, "SZ", "Schwyz",
  6, "OW", "Obwalden",
  7, "NW", "Nidwalden",
  8, "GL", "Glarus",
  9, "ZG", "Zug",
  10, "FR", "Fribourg",
  11, "SO", "Solothurn",
  12, "BS", "Basel-Stadt",
  13, "BL", "Basel-Landschaft",
  14, "SH", "Schaffhausen",
  15, "AR", "Appenzell A.Rh.",
  16, "AI", "Appenzell I.Rh.",
  17, "SG", "St. Gallen",
  18, "GR", "Graubünden",
  19, "AG", "Aargau",
  20, "TG", "Thurgau",
  21, "TI", "Ticino",
  22, "VD", "Vaud",
  23, "VS", "Valais",
  24, "NE", "Neuchâtel",
  25, "GE", "Genève",
  26, "JU", "Jura"
)

panel <- panel %>%
  left_join(canton_map, by = "canton")

# --- Border cantons flag (for robustness) -------------------------------------
border_cantons <- c("BE", "FR", "VS", "GR", "SO", "JU")
panel <- panel %>%
  mutate(is_border = if_else(canton_abbr %in% border_cantons, 1L, 0L))

# --- Save ---------------------------------------------------------------------
out_path <- file.path(base, "data/processed/shp_analysis.rds")
saveRDS(panel, out_path)
cat("\nSaved:", out_path, "\n")
cat("  Final dimensions:", nrow(panel), "rows x", ncol(panel), "cols\n")

# --- Quick diagnostics --------------------------------------------------------
cat("\n=== Quick diagnostics ===\n")
cat("Observations by year:\n")
print(table(panel$year))
cat("\nMean hours_gap by region and period:\n")
panel %>%
  filter(!is.na(hours_gap)) %>%
  group_by(lang_cat, period) %>%
  summarise(
    n = n(),
    mean_gap = round(mean(hours_gap, na.rm = TRUE), 2),
    pct_wants_less = round(mean(wants_less, na.rm = TRUE) * 100, 1),
    .groups = "drop"
  ) %>%
  print(n = 20)

cat("\n=== Done ===\n")
