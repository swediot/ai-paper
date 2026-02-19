# ==============================================================================
# 05_appendix_figures.R  —  Supplementary figures for the appendix
# ==============================================================================

library(tidyverse)
library(sf)
library(viridis)
library(patchwork)

cat("=== 05_appendix_figures.R ===\n")

base <- "g:/My Drive/sandbox/ai-paper"
df <- readRDS(file.path(base, "data/processed/shp_analysis.rds"))

dir.create(file.path(base, "figures"), showWarnings = FALSE, recursive = TRUE)

# --- Publication theme --------------------------------------------------------
theme_paper <- theme_minimal(base_size = 12, base_family = "serif") +
    theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line        = element_line(linewidth = 0.5, color = "black"),
        legend.position  = "bottom",
        strip.text       = element_text(face = "bold"),
        plot.caption     = element_text(size = 8, color = "grey50", hjust = 0)
    )

pal_region <- c("German" = "#2166ac", "French" = "#d6604d")

# Swiss bounding box (CH1903+ / LV95)
ch_xlim <- c(2480000, 2840000)
ch_ylim <- c(1070000, 1300000)

# ==============================================================================
# A. Trends in mean actual vs. contractual hours by region
# ==============================================================================
cat("\n--- A: Hours trends ---\n")

hours_trends <- df %>%
    filter(!is.na(pw77), !is.na(pw74), !is.na(lang_cat)) %>%
    group_by(year, lang_cat) %>%
    summarise(
        actual = mean(pw77, na.rm = TRUE),
        contractual = mean(pw74, na.rm = TRUE),
        .groups = "drop"
    ) %>%
    pivot_longer(cols = c(actual, contractual), names_to = "type", values_to = "hours")

p_hours <- ggplot(hours_trends, aes(x = year, y = hours, color = lang_cat, linetype = type)) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 1.5) +
    scale_color_manual(values = pal_region, name = "Region") +
    scale_linetype_manual(
        values = c("actual" = "solid", "contractual" = "dashed"),
        name = "Hours Type"
    ) +
    labs(x = NULL, y = "Hours per Week") +
    theme_paper

ggsave(file.path(base, "figures/fig_app_hours_trends.pdf"), p_hours,
    width = 9, height = 5, device = cairo_pdf
)
cat("  Saved figures/fig_app_hours_trends.pdf\n")

# ==============================================================================
# B. Map: Mean job satisfaction by canton
# ==============================================================================
cat("\n--- B: Canton job satisfaction map ---\n")

cantons_sf <- st_read(
    file.path(base, "swiss-location-data/swissBOUNDARIES3D_1_5_TLM_KANTONSGEBIET.shp"),
    quiet = TRUE
) %>%
    mutate(canton = as.numeric(KANTONSNUM))

lakes_sf <- st_read(
    file.path(base, "swiss-location-data/swissTLMRegio_Lake.shp"),
    quiet = TRUE
)

# Clip lakes to Swiss national boundary
ch_border <- st_union(cantons_sf)
lakes_sf <- lakes_sf %>%
    filter(st_area(.) > units::set_units(5e6, "m^2")) %>%
    st_transform(st_crs(cantons_sf)) %>%
    st_intersection(ch_border)

# Compute mean job satisfaction by canton (post-pandemic)
jsat_canton <- df %>%
    filter(!is.na(pw228), year >= 2021) %>%
    group_by(canton) %>%
    summarise(mean_job_sat = mean(pw228, na.rm = TRUE), .groups = "drop")

cantons_jsat <- cantons_sf %>%
    left_join(jsat_canton, by = "canton")

p_map_jsat <- ggplot() +
    geom_sf(
        data = cantons_jsat, aes(fill = mean_job_sat),
        color = "white", linewidth = 0.3
    ) +
    geom_sf(data = ch_border, fill = NA, color = "black", linewidth = 0.7) +
    geom_sf(data = lakes_sf, fill = "#a6cee3", color = NA, alpha = 0.7) +
    coord_sf(xlim = ch_xlim, ylim = ch_ylim, expand = FALSE) +
    scale_fill_viridis_c(
        option = "mako", direction = -1,
        name = "Mean Job\nSatisfaction"
    ) +
    theme_void(base_size = 11, base_family = "serif") +
    theme(
        legend.position = "bottom",
        legend.direction = "horizontal"
    )

ggsave(file.path(base, "figures/fig_app_map_job_sat.pdf"), p_map_jsat,
    width = 8, height = 6, device = cairo_pdf
)
cat("  Saved figures/fig_app_map_job_sat.pdf\n")

# ==============================================================================
# C. Cohort-specific trends (3 panels: Boomers, Gen X, Millennials)
# ==============================================================================
cat("\n--- C: Cohort-specific trends ---\n")

# Boomers (1946-1964): start from 1999 (panel start)
# Gen X (1965-1980): start from 1999
# Millennials (1981-1996): start from 2004 (midpoint 1988 + 16 = 2004,
#   i.e. when the median Millennial reached working age)

cohort_trends <- df %>%
    filter(
        !is.na(wants_less), !is.na(cohort),
        cohort %in% c("Boomer", "Gen X", "Millennial")
    ) %>%
    mutate(cohort_label = factor(case_when(
        cohort == "Boomer" ~ "Boomers (1946\u20131964)",
        cohort == "Gen X" ~ "Gen X (1965\u20131980)",
        cohort == "Millennial" ~ "Millennials (1981\u20131996)"
    ), levels = c("Boomers (1946\u20131964)", "Gen X (1965\u20131980)", "Millennials (1981\u20131996)"))) %>%
    # Filter Millennials to start from 2004
    filter(!(cohort == "Millennial" & year < 2004)) %>%
    group_by(year, cohort_label, lang_cat) %>%
    summarise(pct = mean(wants_less, na.rm = TRUE) * 100, .groups = "drop")

p_cohort <- ggplot(cohort_trends, aes(x = year, y = pct, color = lang_cat)) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 1.5) +
    facet_wrap(~cohort_label, ncol = 3) +
    scale_color_manual(values = pal_region, name = "Region") +
    labs(x = NULL, y = "% Wanting Fewer Hours") +
    theme_paper

ggsave(file.path(base, "figures/fig_app_cohort_trends.pdf"), p_cohort,
    width = 14, height = 4.5, device = cairo_pdf
)
cat("  Saved figures/fig_app_cohort_trends.pdf\n")

cat("\n=== 05_appendix_figures.R complete ===\n")
