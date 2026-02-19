# ==============================================================================
# 03_analysis.R  —  Main analysis: descriptives, regressions, maps
# ==============================================================================
# Input:  data/processed/shp_analysis.rds
#         swiss-location-data/ (shapefiles)
# Output: tables/tab_desc.tex, tables/tab_prepost.tex, tables/tab_mechanisms.tex,
#         tables/tab_wellbeing.tex
#         figures/fig_trends.pdf, figures/fig_map_language.pdf,
#         figures/fig_heterogeneity.pdf
# ==============================================================================

library(tidyverse)
library(fixest)
library(modelsummary)
library(sf)
library(viridis)
library(patchwork)

cat("=== 03_analysis.R ===\n")

base <- "g:/My Drive/sandbox/ai-paper"
df <- readRDS(file.path(base, "data/processed/shp_analysis.rds"))

# Rename FE labels for modelsummary output
setFixest_dict(c(idpers = "Individual", year = "Year"))

dir.create(file.path(base, "tables"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(base, "figures"), showWarnings = FALSE, recursive = TRUE)

# --- Publication theme --------------------------------------------------------
theme_paper <- theme_minimal(base_size = 12, base_family = "serif") +
  theme(
    plot.title       = element_text(face = "bold", size = 13),
    plot.subtitle    = element_text(size = 10, color = "grey40"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line        = element_line(linewidth = 0.5, color = "black"),
    legend.position  = "bottom",
    strip.text       = element_text(face = "bold"),
    plot.caption     = element_text(size = 8, color = "grey50", hjust = 0)
  )

pal_region <- c("German" = "#2166ac", "French" = "#d6604d")

# Swiss bounding box (CH1903+ / LV95 to match shapefile CRS)
ch_xlim <- c(2480000, 2840000)
ch_ylim <- c(1070000, 1300000)

# ==============================================================================
# PART 1: DESCRIPTIVE STATISTICS (Table 1)
# ==============================================================================
cat("\n--- Part 1: Descriptive Statistics ---\n")

desc <- df %>%
  filter(!is.na(hours_gap), !is.na(is_latin)) %>%
  rename(Language = lang_cat) %>%
  group_by(Language) %>%
  summarise(
    N = format(n(), big.mark = ",", trim = TRUE),
    Individuals = format(n_distinct(idpers), big.mark = ",", trim = TRUE),
    `Mean Age` = format(round(mean(age, na.rm = TRUE), 1), nsmall = 1, trim = TRUE),
    `Female (%)` = format(round(mean(female, na.rm = TRUE) * 100, 1), nsmall = 1, trim = TRUE),
    `Tertiary Edu (%)` = format(round(mean(edu_cat == "Tertiary", na.rm = TRUE) * 100, 1), nsmall = 1, trim = TRUE),
    `Contractual Hrs` = format(round(mean(pw74, na.rm = TRUE), 1), nsmall = 1, trim = TRUE),
    `Actual Hrs` = format(round(mean(pw77, na.rm = TRUE), 1), nsmall = 1, trim = TRUE),
    `Hours Gap` = format(round(mean(hours_gap, na.rm = TRUE), 1), nsmall = 1, trim = TRUE),
    `Wants Less (%)` = format(round(mean(wants_less, na.rm = TRUE) * 100, 1), nsmall = 1, trim = TRUE),
    `Life Sat (0-10)` = format(round(mean(pc44, na.rm = TRUE), 2), nsmall = 2, trim = TRUE),
    `Job Sat (0-10)` = format(round(mean(pw228, na.rm = TRUE), 2), nsmall = 2, trim = TRUE),
    .groups = "drop"
  )

print(desc)

# Export to LaTeX
# Build Table 1 manually for clean formatting (Row = Variable, Col = Region)
desc_wide <- desc %>%
  pivot_longer(cols = -Language, names_to = "Variable", values_to = "Value") %>%
  pivot_wider(names_from = Language, values_from = "Value")

# Export to LaTeX
desc_tex <- kableExtra::kbl(
  desc_wide,
  format   = "latex",
  booktabs = TRUE,
  caption  = "Descriptive Statistics by Language Region",
  label    = "desc"
) %>%
  kableExtra::kable_styling(latex_options = c("hold_position"), font_size = 8)

writeLines(desc_tex, file.path(base, "tables/tab_desc.tex"))
cat("  Saved tables/tab_desc.tex\n")

# ==============================================================================
# PART 2: TRENDS FIGURE (Figure 1)
# ==============================================================================
cat("\n--- Part 2: Trends Figure ---\n")

trends <- df %>%
  filter(!is.na(hours_gap), !is.na(lang_cat)) %>%
  group_by(year, lang_cat) %>%
  summarise(
    mean_gap = mean(hours_gap, na.rm = TRUE),
    pct_wants_less = mean(wants_less, na.rm = TRUE) * 100,
    mean_life_sat = mean(pc44, na.rm = TRUE),
    mean_job_sat = mean(pw228, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

p_trend_gap <- ggplot(trends, aes(x = year, y = pct_wants_less, color = lang_cat)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = pal_region, name = "Language Region") +
  labs(
    x = NULL,
    y = "% Wanting Fewer Hours"
  ) +
  theme_paper

p_trend_sat <- ggplot(trends, aes(x = year, y = mean_life_sat, color = lang_cat)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = pal_region, name = "Language Region") +
  labs(
    x = NULL,
    y = "Life Satisfaction (0-10)"
  ) +
  theme_paper

fig_trends <- p_trend_gap / p_trend_sat +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

ggsave(file.path(base, "figures/fig_trends.pdf"), fig_trends,
  width = 8, height = 9, device = cairo_pdf
)
cat("  Saved figures/fig_trends.pdf\n")

# ==============================================================================
# PART 3: MAP — Language Regions of Switzerland (Figure 3)
# ==============================================================================
cat("\n--- Part 3: Canton Maps ---\n")

# Load canton shapefile (finest geography in SHP data)
cantons_sf <- st_read(
  file.path(base, "swiss-location-data/swissBOUNDARIES3D_1_5_TLM_KANTONSGEBIET.shp"),
  quiet = TRUE
)

lakes_sf <- st_read(
  file.path(base, "swiss-location-data/swissTLMRegio_Lake.shp"),
  quiet = TRUE
)

# National border and clip lakes to Switzerland
ch_border <- st_union(cantons_sf)
lakes_sf <- lakes_sf %>%
  filter(st_area(.) > units::set_units(5e6, "m^2")) %>%
  st_transform(st_crs(cantons_sf)) %>%
  st_intersection(ch_border)

# Map canton IDs (KANTONSNUM 1-26, no Liechtenstein)
cantons_sf <- cantons_sf %>%
  mutate(canton = as.numeric(KANTONSNUM))

# Assign language region to cantons
french_cantons <- c(10, 22, 23, 24, 25, 26) # FR, VD, VS, NE, GE, JU
bilingual_cantons <- c(2, 18) # BE, GR
cantons_sf <- cantons_sf %>%
  mutate(
    lang_region = case_when(
      canton %in% french_cantons ~ "French",
      canton %in% bilingual_cantons ~ "Bilingual",
      canton == 21 ~ "Italian (excl.)",
      TRUE ~ "German"
    )
  )

pal_map <- c(
  "German" = "#2166ac", "French" = "#d6604d",
  "Bilingual" = "#b2abd2", "Italian (excl.)" = "grey80"
)

p_map_lang <- ggplot() +
  geom_sf(
    data = cantons_sf, aes(fill = lang_region),
    color = "white", linewidth = 0.3
  ) +
  geom_sf(data = ch_border, fill = NA, color = "black", linewidth = 0.7) +
  geom_sf(data = lakes_sf, fill = "#a6cee3", color = NA, alpha = 0.7) +
  coord_sf(xlim = ch_xlim, ylim = ch_ylim, expand = FALSE) +
  scale_fill_manual(values = pal_map, name = NULL) +
  theme_void(base_size = 11, base_family = "serif") +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal"
  )

# --- Map B: Overwork prevalence by canton ------------------------------------
overwork_canton <- df %>%
  filter(!is.na(wants_less), year >= 2021) %>%
  group_by(canton) %>%
  summarise(
    pct_wants_less = mean(wants_less, na.rm = TRUE) * 100,
    .groups = "drop"
  )

cantons_sf2 <- cantons_sf %>%
  left_join(overwork_canton, by = "canton")

p_map_overwork <- ggplot() +
  geom_sf(
    data = cantons_sf2, aes(fill = pct_wants_less),
    color = "white", linewidth = 0.3
  ) +
  geom_sf(data = ch_border, fill = NA, color = "black", linewidth = 0.7) +
  geom_sf(data = lakes_sf, fill = "#a6cee3", color = NA, alpha = 0.7) +
  coord_sf(xlim = ch_xlim, ylim = ch_ylim, expand = FALSE) +
  scale_fill_viridis_c(
    option = "mako", direction = -1,
    name = "% Wanting\nFewer Hours",
    na.value = "grey90"
  ) +
  theme_void(base_size = 11, base_family = "serif") +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal"
  )

fig_maps <- p_map_lang + p_map_overwork +
  plot_layout(ncol = 2)

ggsave(file.path(base, "figures/fig_map_language.pdf"), fig_maps,
  width = 14, height = 6, device = cairo_pdf
)
cat("  Saved figures/fig_map_language.pdf\n")

# ==============================================================================
# PART 4: DESCRIPTIVE MEANS OF MECHANISMS (Table 2)
# ==============================================================================
cat("\n--- Part 4: Descriptive Means of Mechanisms ---\n")

pp_data <- df %>%
  filter(!is.na(hours_gap), !is.na(is_latin)) %>%
  mutate(
    region = ifelse(is_latin == 1, "French", "German")
  )

prepost <- pp_data %>%
  group_by(region) %>%
  summarise(
    `Hours Gap` = format(round(mean(hours_gap, na.rm = TRUE), 2), nsmall = 2, trim = TRUE),
    `Wants Less (\\%)` = format(round(mean(wants_less, na.rm = TRUE) * 100, 1), nsmall = 1, trim = TRUE),
    `Exhaustion` = format(round(mean(pf51, na.rm = TRUE), 2), nsmall = 2, trim = TRUE),
    `Work-Life Int.` = format(round(mean(pf50, na.rm = TRUE), 2), nsmall = 2, trim = TRUE),
    `Disconnect` = format(round(mean(pf52, na.rm = TRUE), 2), nsmall = 2, trim = TRUE),
    `Free Time Sat.` = format(round(mean(pa05, na.rm = TRUE), 2), nsmall = 2, trim = TRUE),
    N = format(n(), big.mark = ",", trim = TRUE),
    .groups = "drop"
  )

print(prepost)

# Build LaTeX table manually for clean formatting
prepost_wide <- prepost %>%
  pivot_longer(cols = -region, names_to = "Variable", values_to = "Value") %>%
  pivot_wider(names_from = region, values_from = Value)

# Export as kable
prepost_tex <- kableExtra::kbl(
  prepost_wide,
  format   = "latex",
  booktabs = TRUE,
  caption  = "Descriptive Means of Mechanism and Satisfaction Variables by Language Region",
  label    = "prepost",
  escape   = FALSE
) %>%
  kableExtra::kable_styling(
    latex_options = c("hold_position"),
    font_size     = 8
  )

writeLines(prepost_tex, file.path(base, "tables/tab_prepost.tex"))
cat("  Saved tables/tab_prepost.tex\n")

# ==============================================================================
# PART 5A: BURNOUT & BOUNDARY MECHANISMS (Table 3 — headline)
# ==============================================================================
cat("\n--- Part 5A: Burnout & Boundary Mechanisms ---\n")

reg_data <- df %>%
  filter(!is.na(hours_gap), !is.na(is_latin))

# (1) Work Stress -- REMOVED per user request
# m_stress <- feols(...)

# (2) Exhaustion after work
m_exhaust <- feols(
  pf51 ~ hours_gap * is_latin + age + age_sq + female + has_kids | idpers + year,
  data = reg_data %>% filter(!is.na(pf51)), cluster = ~idpers
)

# (3) Work-life interference
m_wli <- feols(
  pf50 ~ hours_gap * is_latin + age + age_sq + female + has_kids | idpers + year,
  data = reg_data %>% filter(!is.na(pf50)), cluster = ~idpers
)

# (4) Difficulty disconnecting from work
m_disc <- feols(
  pf52 ~ hours_gap * is_latin + age + age_sq + female + has_kids | idpers + year,
  data = reg_data %>% filter(!is.na(pf52)), cluster = ~idpers
)

# Shared GOF map for modelsummary tables
gof_custom <- list(
  list(raw = "nobs", clean = "Observations", fmt = 0),
  list(raw = "r.squared", clean = "R$^2$", fmt = 3),
  list(raw = "FE: idpers", clean = "FE: Individual", fmt = 0),
  list(raw = "FE: year", clean = "FE: Year", fmt = 0)
)

msummary(
  list(
    "Exhaustion" = m_exhaust,
    "Work-Life Int." = m_wli,
    "Disconnect" = m_disc
  ),
  output = file.path(base, "tables/tab_mechanisms.tex"),
  stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  coef_map = c(
    "hours_gap"              = "Hours Gap",
    "is_latin"               = "French Region",
    "hours_gap:is_latin"     = "Hours Gap $\\times$ French"
  ),
  gof_map = gof_custom,
  title = "Burnout and Boundary-Setting Mechanisms\\label{tab:mechanisms}",
  notes = list("\\scriptsize * p<0.1, ** p<0.05, *** p<0.01. Individual and year fixed effects. Standard errors clustered at the individual level. Controls: age, age\\textsuperscript{2}, female, has children. Exhaustion, work-life interference, and disconnecting are scaled 0--10."),
  stars_note = FALSE,
  escape = FALSE
)
cat("  Saved tables/tab_mechanisms.tex\n")

# ==============================================================================
# PART 5B: SATISFACTION OUTCOMES (Table 4 — supporting)
# ==============================================================================
cat("\n--- Part 5B: Satisfaction Outcomes ---\n")

reg_data_sat <- reg_data %>% filter(!is.na(pc44))

# (1) Life satisfaction
m_life <- feols(
  pc44 ~ hours_gap * is_latin + age + age_sq + female + has_kids | idpers + year,
  data = reg_data_sat, cluster = ~idpers
)

# (2) Job satisfaction
m_job <- feols(
  pw228 ~ hours_gap * is_latin + age + age_sq + female + has_kids | idpers + year,
  data = reg_data_sat %>% filter(!is.na(pw228)), cluster = ~idpers
)

# (3) Free time satisfaction
m_free <- feols(
  pa05 ~ hours_gap * is_latin + age + age_sq + female + has_kids | idpers + year,
  data = reg_data_sat %>% filter(!is.na(pa05)), cluster = ~idpers
)

# (4) Satisfaction with work conditions
m_cond <- feols(
  pw93 ~ hours_gap * is_latin + age + age_sq + female + has_kids | idpers + year,
  data = reg_data_sat %>% filter(!is.na(pw93)), cluster = ~idpers
)

# (5) Satisfaction with amount of work
m_amt <- feols(
  pw230 ~ hours_gap * is_latin + age + age_sq + female + has_kids | idpers + year,
  data = reg_data_sat %>% filter(!is.na(pw230)), cluster = ~idpers
)

msummary(
  list(
    "Life Sat." = m_life,
    "Job Sat." = m_job,
    "Free Time" = m_free,
    "Work Cond." = m_cond,
    "Work Amount" = m_amt
  ),
  output = file.path(base, "tables/tab_wellbeing.tex"),
  stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  coef_map = c(
    "hours_gap"              = "Hours Gap",
    "is_latin"               = "French Region",
    "hours_gap:is_latin"     = "Hours Gap $\\times$ French"
  ),
  gof_map = gof_custom,
  title = "The Effect of Overwork on Domain-Specific Satisfaction\\label{tab:wellbeing}",
  notes = list("\\scriptsize * p<0.1, ** p<0.05, *** p<0.01. Individual and year fixed effects. Standard errors clustered at the individual level. Controls: age, age\\textsuperscript{2}, female, has children. All outcomes scaled 0--10."),
  stars_note = FALSE,
  escape = FALSE
)
cat("  Saved tables/tab_wellbeing.tex\n")

# ==============================================================================
# PART 6: HETEROGENEITY — Coefficient plot (Figure 3)
# ==============================================================================
cat("\n--- Part 6: Heterogeneity ---\n")

# Define function to run heterogeneity analysis for a given outcome
run_het_analysis <- function(outcome_var, outcome_label, filename_suffix) {
  # Filter data for this outcome
  het_data <- reg_data %>% filter(!is.na(.data[[outcome_var]]))

  # Formula
  fml <- as.formula(paste(outcome_var, "~ hours_gap * is_latin + age + age_sq + female + has_kids | idpers + year"))
  fml_no_female <- as.formula(paste(outcome_var, "~ hours_gap * is_latin + age + age_sq + has_kids | idpers + year"))
  fml_no_kids <- as.formula(paste(outcome_var, "~ hours_gap * is_latin + age + age_sq + female | idpers + year"))

  # List of models
  models <- list()

  # 1. Full Sample
  models[["Full Sample"]] <- feols(fml, data = het_data, cluster = ~idpers)

  # 2. Gender
  models[["Women"]] <- feols(fml_no_female, data = het_data %>% filter(female == 1), cluster = ~idpers)
  models[["Men"]] <- feols(fml_no_female, data = het_data %>% filter(female == 0), cluster = ~idpers)

  # 3. Parental Status
  models[["Parents"]] <- feols(fml_no_kids, data = het_data %>% filter(has_kids == 1), cluster = ~idpers)
  models[["Non-Parents"]] <- feols(fml_no_kids, data = het_data %>% filter(has_kids == 0), cluster = ~idpers)

  # 4. Managers (ISCO Major Group 1)
  models[["Managers"]] <- feols(fml, data = het_data %>% filter(occupa == 1), cluster = ~idpers)
  models[["Non-Managers"]] <- feols(fml, data = het_data %>% filter(occupa != 1), cluster = ~idpers)

  # 5. Border Region
  models[["Border"]] <- feols(fml, data = het_data %>% filter(is_border == 1), cluster = ~idpers)
  models[["Non-Border"]] <- feols(fml, data = het_data %>% filter(is_border == 0), cluster = ~idpers)

  # Extract coefficients
  extract_coef <- function(mod, label) {
    ct <- coeftable(mod)
    idx <- which(rownames(ct) == "hours_gap:is_latin")
    if (length(idx) == 0) {
      return(NULL)
    }
    tibble(
      group = label,
      coef  = ct[idx, "Estimate"],
      se    = ct[idx, "Std. Error"],
      ci_lo = coef - 1.96 * se,
      ci_hi = coef + 1.96 * se
    )
  }

  results <- bind_rows(
    extract_coef(models[["Full Sample"]], "Full Sample"),
    tibble(group = "   ", coef = NA, se = NA, ci_lo = NA, ci_hi = NA), # Spacer 1

    extract_coef(models[["Women"]], "Women"),
    extract_coef(models[["Men"]], "Men"), # Explicitly include omitted
    tibble(group = "  ", coef = NA, se = NA, ci_lo = NA, ci_hi = NA), # Spacer 2

    extract_coef(models[["Parents"]], "Parents"),
    extract_coef(models[["Non-Parents"]], "Non-Parents"),
    tibble(group = " ", coef = NA, se = NA, ci_lo = NA, ci_hi = NA), # Spacer 3

    extract_coef(models[["Managers"]], "Managers"),
    extract_coef(models[["Non-Managers"]], "Non-Managers"),
    tibble(group = "", coef = NA, se = NA, ci_lo = NA, ci_hi = NA), # Spacer 4

    extract_coef(models[["Border"]], "Border Residents"),
    extract_coef(models[["Non-Border"]], "Non-Border Residents")
  ) %>%
    mutate(group = fct_inorder(group)) %>%
    # Reverse order for plotting (top to bottom)
    mutate(group = fct_rev(group))

  # Normalize Full Sample coefficient to 0
  # Note: Since Full Sample is first, index 1 (or last after rev)
  # But we can find it by group name.
  base_coef <- results$coef[results$group == "Full Sample"]

  # Avoid NA in subtraction
  results <- results %>%
    mutate(
      coef  = if_else(!is.na(coef), coef - base_coef, NA_real_),
      ci_lo = if_else(!is.na(ci_lo), ci_lo - base_coef, NA_real_),
      ci_hi = if_else(!is.na(ci_hi), ci_hi - base_coef, NA_real_)
    )

  # Plot
  p <- ggplot(results, aes(x = coef, y = group)) +
    geom_vline(xintercept = 0, color = "grey60", linewidth = 0.5) +
    geom_errorbarh(aes(xmin = ci_lo, xmax = ci_hi), height = 0.2, color = "#d6604d", linewidth = 0.8) +
    geom_point(size = 3, color = "#d6604d") +
    labs(
      x = bquote(.(outcome_label) ~ (hat(beta)[3] ~ "Hours Gap" %*% "French")),
      y = NULL
    ) +
    theme_paper

  ggsave(file.path(base, paste0("figures/fig_heterogeneity_", filename_suffix, ".pdf")),
    p,
    width = 8, height = 5, device = cairo_pdf
  )
  cat(paste0("  Saved figures/fig_heterogeneity_", filename_suffix, ".pdf\n"))
}

# Run for 3 outcomes
run_het_analysis("pf51", "Exhaustion", "exhaust")
run_het_analysis("pf50", "Work-Life Interference", "wli")
run_het_analysis("pf52", "Ability to Disconnect", "disc")

cat("\n=== 03_analysis.R complete ===\n")
