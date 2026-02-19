df <- readRDS("g:/My Drive/sandbox/ai-paper/data/processed/shp_analysis.rds")
cat("Total rows:", nrow(df), "\n")
cat("Female==1:", sum(df$female == 1, na.rm = TRUE), "\n")
cat("Has_kids==1:", sum(df$has_kids == 1, na.rm = TRUE), "\n")
cat("Occup 1000-1999:", sum(df$occupa >= 1000 & df$occupa < 2000, na.rm = TRUE), "\n")
cat("Is_border==1:", sum(df$is_border == 1, na.rm = TRUE), "\n")

cat("Head is_border:", head(df$is_border), "\n")
cat("Head occupa:", head(df$occupa), "\n")
