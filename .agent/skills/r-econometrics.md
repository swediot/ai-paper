---
name: r-econometrics
description: Run IV, DiD, and RDD analyses in R with proper diagnostics and data cleaning
workflow_stage: analysis
compatibility:
  - claude-code
  - cursor
  - codex
  - gemini-cli
author: Awesome Econ AI Community
version: 1.1.0
tags:
  - R
  - econometrics
  - causal-inference
  - fixest
  - regression
  - data-cleaning
---

# R Econometrics

## Purpose

This skill helps economists run rigorous econometric analyses in R, from data cleaning to high-end causal inference (IV, DiD, RDD). It generates publication-ready code using `fixest` and `modelsummary`.

## Instructions

### Step 1: Understand the Research Design

Before generating code, ask the user:
1. What is the identification strategy? (OLS, IV, DiD, RDD)
2. What is the level of observation and clustering?
3. What fixed effects are needed?
4. Is data cleaning required?

### Step 2: Generate Analysis Code

Generate R code that:
1. **Cleans data properly** using `janitor` and `tidyverse`.
2. **Uses `fixest`** for fast high-dimensional fixed effects.
3. **Includes diagnostics** (F-stats, parallel trends, balance tables).
4. **Creates publication tables** using `modelsummary`.

## Example Analysis Workflow

```r
# ============================================
# 1. Setup and Packages
# ============================================
library(tidyverse)
library(fixest)       # Estimation
library(modelsummary) # Tables
library(janitor)      # Cleaning
library(sandwich)     # Robust SEs

# ============================================
# 2. Data Cleaning
# ============================================
df <- read_csv("data.csv") %>%
  clean_names() %>%   # snake_case names
  mutate(
    treated = if_else(state %in% treat_list, 1, 0),
    post = if_else(year >= 2015, 1, 0),
    treat_post = treated * post
  )

# Check balance / summary stats
datasummary_skim(df)
datasummary_balance(~treated, data = df)

# ============================================
# 3. Main Specification (DiD example)
# ============================================

# Two-way fixed effects with clustering
did_model <- feols(
  outcome ~ treat_post + controls | state + year,
  data = df,
  cluster = ~state
)

# ============================================
# 4. Event Study
# ============================================
# Create time-to-treatment
df <- df %>% mutate(rel_time = year - 2015)

event_study <- feols(
  outcome ~ i(rel_time, treated, ref = -1) | state + year,
  data = df,
  cluster = ~state
)

iplot(event_study, main = "Event Study")

# ============================================
# 5. Export Results
# ============================================
modelsummary(
  list("DiD" = did_model, "Event Study" = event_study),
  stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01),
  output = "tables/results.tex",
  gof_map = c("nobs", "r.squared")
)
```

## Best Practices

1. **Clustering**: Always cluster standard errors at the level of treatment assignment.
2. **Fixed Effects**: Use `feols` (fixest) which is faster than `lm` or `plm` for high-dimensional FEs.
3. **Pre-trends**: Visualize them using event study plots (`iplot`).
4. **Data Validation**: Check for missing values and duplicates before running regressions.

## References

- [fixest documentation](https://lrberge.github.io/fixest/)
- [Cunningham (2021) Causal Inference: The Mixtape](https://mixtape.scunning.com/)
