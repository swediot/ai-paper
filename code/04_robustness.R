# ==============================================================================
# 04_robustness.R  —  Robustness checks
# ==============================================================================

library(tidyverse)
library(fixest)
library(modelsummary)

cat("=== 04_robustness.R ===\n")

base <- "g:/My Drive/sandbox/ai-paper"
df <- readRDS(file.path(base, "data/processed/shp_analysis.rds"))

dir.create(file.path(base, "tables"), showWarnings = FALSE, recursive = TRUE)

# Rename FE labels
setFixest_dict(c(idpers = "Individual", year = "Year"))

# Shared GOF map
gof_custom <- list(
    list(raw = "nobs", clean = "Observations", fmt = 0),
    list(raw = "r.squared", clean = "R$^2$", fmt = 3),
    list(raw = "FE: idpers", clean = "FE: Individual", fmt = 0),
    list(raw = "FE: year", clean = "FE: Year", fmt = 0)
)

# ==============================================================================
# A. Alternative outcomes
# ==============================================================================
cat("\n--- A: Alternative outcomes ---\n")

reg_data <- df %>%
    filter(!is.na(hours_gap), !is.na(is_latin))

# Work stress
m_stress <- feols(
    pw604 ~ hours_gap * is_latin + age + age_sq + female + has_kids | idpers + year,
    data = reg_data %>% filter(!is.na(pw604)), cluster = ~idpers
)

# Satisfaction with work conditions
m_wcond <- feols(
    pw93 ~ hours_gap * is_latin + age + age_sq + female + has_kids | idpers + year,
    data = reg_data %>% filter(!is.na(pw93)), cluster = ~idpers
)

# Exhaustion after work
m_exhaust <- feols(
    pf51 ~ hours_gap * is_latin + age + age_sq + female + has_kids | idpers + year,
    data = reg_data %>% filter(!is.na(pf51)), cluster = ~idpers
)

msummary(
    list(
        "Work Cond. Sat." = m_wcond,
        "Post-work Exhaust." = m_exhaust
    ),
    output = file.path(base, "tables/tab_robust_alt_outcomes.tex"),
    stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
    coef_map = c(
        "hours_gap"              = "Hours Gap",
        "is_latin"               = "French Region",
        "hours_gap:is_latin"     = "Hours Gap $\\times$ French"
    ),
    gof_map = gof_custom,
    title = "Robustness: Alternative Outcome Measures\\label{tab:robust_alt}",
    notes = list("\\scriptsize * p<0.1, ** p<0.05, *** p<0.01. Individual and year fixed effects. Standard errors clustered by individual."),
    stars_note = FALSE,
    escape = FALSE
)
cat("  Saved tables/tab_robust_alt_outcomes.tex\n")

cat("\n=== 04_robustness.R complete ===\n")
