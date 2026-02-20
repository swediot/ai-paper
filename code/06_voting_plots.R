# 06_voting_plots.R  —  Plots for voting data (referenda)
# Recreates Regression Discontinuity Design (RDD) binned scatter plots
# showing the percentage of "Yes" votes by distance to the language border.

library(tidyverse)
library(readxl)
library(sf)
library(patchwork)
library(janitor)

cat("=== 06_voting_plots.R (RDD Version) ===\n")

# --- Define Voting Files & Titles ---
voting_files <- list(
  list(file = "Voting_data/575.00-abstimmungsergebnis-pro-kanton-bezirk-und-gemeinde.xlsx", title = "1:12 Initiative (2013)"),
  list(file = "Voting_data/583.00-abstimmungsergebnis-pro-kanton-bezirk-und-gemeinde.xlsx", title = "Minimum Wage (2014)"),
  list(file = "Voting_data/601.00-abstimmungsergebnis-pro-kanton-bezirk-und-gemeinde.xlsx", title = "Basic Income (2016)"),
  list(file = "Voting_data/6_week_holiday_je-d-17.03.03.dw.557.c.xlsx", title = "6 Weeks Holiday (2012)")
)

# --- 1. Load Spatial and Language Data ---
cat("Calculating distances to language border...\n")

# Read municipality shapefile
muni_shp <- st_read("swiss-location-data/swissBOUNDARIES3D_1_5_TLM_HOHEITSGEBIET.shp", quiet = TRUE) %>%
  filter(OBJEKTART == "Gemeindegebiet") %>%
  mutate(BFS_NUMMER = as.integer(BFS_NUMMER))

# Read language data from AMTOVZ and determine dominant language per municipality
amtovz <- read_delim("swiss-location-data/zip_code_canton/AMTOVZ_CSV_LV95.csv", delim = ";", show_col_types = FALSE) %>%
  clean_names()

lang_map <- amtovz %>%
  group_by(bfs_nr) %>%
  summarize(sprache = names(which.max(table(sprache)))) %>%
  ungroup() %>%
  mutate(lang_region = ifelse(sprache == "de", "German", "Romance"))

# Join and calculate centroids
muni_cents <- muni_shp %>%
  inner_join(lang_map, by = c("BFS_NUMMER" = "bfs_nr")) %>%
  st_centroid()

# --- 2. Calculate Distance to Border ---
german_cents <- muni_cents %>% filter(lang_region == "German")
romance_cents <- muni_cents %>% filter(lang_region == "Romance")

# Distance from German municipalities to nearest Romance municipality (Negative)
d_g2r <- st_distance(german_cents, romance_cents)
german_cents$dist_to_border <- apply(d_g2r, 1, min) * -1

# Distance from Romance municipalities to nearest German municipality (Positive)
d_r2g <- st_distance(romance_cents, german_cents)
romance_cents$dist_to_border <- apply(d_r2g, 1, min)

# Combine distance data
muni_dists <- bind_rows(
  german_cents %>% st_drop_geometry(),
  romance_cents %>% st_drop_geometry()
) %>%
  select(BFS_NUMMER, lang_region, dist_to_border, EINWOHNERZ) %>%
  mutate(dist_km = as.numeric(dist_to_border) / 1000)

cat("Spatial computation complete.\n")

# --- 3. Read Voting Data ---
cat("Reading Excel voting records...\n")

read_voting <- function(file, title) {
  raw <- read_excel(file, sheet = "Gemeinden", col_names = FALSE, n_max = 15)
  ja_row <- NULL
  gem_row <- NULL
  for (i in seq_len(nrow(raw))) {
    vals <- as.character(raw[i, ])
    if (is.null(ja_row) && any(grepl("Ja in", vals, ignore.case = TRUE))) ja_row <- i
    if (is.null(gem_row) && any(grepl("Gemeinde-Nr", vals, ignore.case = TRUE))) gem_row <- i
  }

  if (ja_row == gem_row) {
    header <- as.character(raw[ja_row, ])
    data_skip <- gem_row
  } else {
    labels <- as.character(raw[gem_row, ])
    metrics <- as.character(raw[ja_row, ])
    header <- ifelse(!is.na(labels) & labels != "", labels, metrics)
    data_skip <- gem_row
  }

  header[is.na(header) | header == ""] <- paste0("col_", seq_along(header))[is.na(header) | header == ""]

  df <- read_excel(file, sheet = "Gemeinden", skip = data_skip, col_names = FALSE)
  names(df) <- header

  df <- janitor::clean_names(df)
  gemeinde_col <- grep("gemeinde.*nr", names(df), ignore.case = TRUE, value = TRUE)[1]
  ja_col <- grep("ja.*in", names(df), ignore.case = TRUE, value = TRUE)[1]

  if (is.na(gemeinde_col) || is.na(ja_col)) {
    stop(paste("Failed to find columns in", file))
  }

  df %>%
    filter(!is.na(.data[[gemeinde_col]])) %>%
    mutate(
      Gemeinde_Nr = as.integer(.data[[gemeinde_col]]),
      Ja_in_ = as.numeric(.data[[ja_col]]),
      title = title
    ) %>%
    select(Gemeinde_Nr, Ja_in_, title)
}

voting_data <- map_dfr(voting_files, ~ read_voting(.x$file, .x$title))

# --- 4. Merge, Bin, and Plot ---
cat("Aggregating and plotting...\n")

# Join voting data with spatial distances
plot_data <- voting_data %>%
  inner_join(muni_dists, by = c("Gemeinde_Nr" = "BFS_NUMMER")) %>%
  filter(!is.na(Ja_in_), !is.na(dist_km)) %>%
  # Bin into 1km distance bins
  mutate(dist_bin = round(dist_km))

# Function to process and plot each referendum
create_rdd_plot <- function(raw_df, referendum_title) {
  # 1. Run sharp RDD regression (Linear specification, bandwidth 100km)
  # y = b0 + b1*is_romance + b2*dist_km + b3*(is_romance*dist_km)
  df_rdd <- raw_df %>%
    filter(abs(dist_km) <= 100) %>%
    mutate(is_romance = as.numeric(dist_km > 0))

  model <- lm(Ja_in_ ~ is_romance * dist_km, data = df_rdd, weights = EINWOHNERZ)
  coef_est <- coef(summary(model))["is_romance", "Estimate"]
  pval <- coef(summary(model))["is_romance", "Pr(>|t|)"]

  stars <- dplyr::case_when(
    pval < 0.01 ~ "***",
    pval < 0.05 ~ "**",
    pval < 0.1 ~ "*",
    .default = ""
  )
  rdd_label <- sprintf("RDD Est: %.2f%s", coef_est, stars)

  # 2. Calculate binned averages for the scatter plot
  df_binned <- raw_df %>%
    group_by(dist_bin, lang_region) %>%
    summarize(
      Ja_mean = sum(Ja_in_ * EINWOHNERZ, na.rm = TRUE) / sum(EINWOHNERZ, na.rm = TRUE),
      Ja_mean = ifelse(is.na(Ja_mean), mean(Ja_in_, na.rm = TRUE), Ja_mean),
      .groups = "drop"
    )

  # Estimate y-position for annotations
  ymax <- max(df_binned$Ja_mean, na.rm = TRUE) + 5

  # 3. Generate Plot (using 1st degree polynomial fit: method="lm", formula=y~x)
  ggplot(df_binned, aes(x = dist_bin, y = Ja_mean)) +
    geom_vline(xintercept = 0, color = "#d73027", linewidth = 0.8) +
    geom_point(color = "#313695", size = 1.2, alpha = 0.8) +
    geom_smooth(data = filter(df_binned, dist_bin <= 0), method = "lm", formula = y ~ x, color = "black", se = FALSE, linewidth = 0.8) +
    geom_smooth(data = filter(df_binned, dist_bin > 0), method = "lm", formula = y ~ x, color = "black", se = FALSE, linewidth = 0.8) +
    scale_y_continuous(labels = scales::number_format(accuracy = 1)) +
    coord_cartesian(xlim = c(-100, 100)) +
    scale_x_continuous(breaks = seq(-100, 100, by = 20)) +
    labs(
      title = referendum_title,
      subtitle = rdd_label,
      x = "distance to language border (km)",
      y = "% yes votes"
    ) +
    theme_minimal(base_size = 12, base_family = "serif") +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, face = "italic", size = 10),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(color = "gray90"),
      panel.grid.major.y = element_line(color = "gray90"),
      axis.line = element_line(color = "black")
    ) +
    annotate("text", x = -50, y = ymax, label = "German language", size = 3.5, family = "serif", color = "black") +
    annotate("text", x = 50, y = ymax, label = "Romance language", size = 3.5, family = "serif", color = "black")
}

# Generate plots directly from underlying raw municipality data
plots <- plot_data %>%
  split(.$title) %>%
  map(~ create_rdd_plot(.x, unique(.x$title)))

# Combine into a 2x2 grid
# Note: Reorder to match the 4 available data files logically
panel <- wrap_plots(
  plots[[1]], plots[[4]],
  plots[[2]], plots[[3]],
  ncol = 2
)

# Save to figures folder
dir.create("figures", showWarnings = FALSE)
ggsave("figures/voting_rdd_panel.pdf", panel, width = 10, height = 8, device = cairo_pdf)
cat("Successfully generated figures/voting_rdd_panel.pdf\n")
