# Read variable names from the SHP long file and one wave file
library(haven)

cat("=== shplong_p_user.dta columns ===\n")
long_path <- "g:/My Drive/sandbox/ai-paper/swiss-household-panel/data/SHP-Data-Longfile-STATA/shplong_p_user.dta"
d <- read_dta(long_path, n_max = 2)
cat("Total columns:", ncol(d), "\n")
cat("Total rows hint:", nrow(d), "(just 2 rows loaded)\n\n")

# Print all column names
cat(paste(names(d), collapse = "\n"), "\n")

# Show labels for key variables
cat("\n=== Variable labels (subset) ===\n")
labs <- sapply(d, function(x) attr(x, "label"))
# Filter for work/satisfaction/language related
idx <- grepl("hour|work|satis|lang|region|canton|desire|wish|want|occupation|isco|employ|job|educ|age|sex|birth|child|well|life|panas|worry",
    labs,
    ignore.case = TRUE
)
cat("\nWork/satisfaction/demographics/language vars:\n")
for (v in names(labs)[idx]) {
    cat(sprintf("  %-25s => %s\n", v, labs[v]))
}

cat("\n\n=== Wave 25 (2023) person file columns ===\n")
w25_dir <- "g:/My Drive/sandbox/ai-paper/swiss-household-panel/data/SHP-Data-W1-W25-STATA/W25_2023"
w25_files <- list.files(w25_dir, pattern = "\\.dta$", full.names = TRUE)
cat("Files in W25_2023:\n")
cat(paste(basename(w25_files), collapse = "\n"), "\n\n")

# Read person file
p_file <- w25_files[grepl("_p_user", w25_files)]
if (length(p_file) > 0) {
    dp <- read_dta(p_file[1], n_max = 2)
    labsp <- sapply(dp, function(x) attr(x, "label"))
    idx2 <- grepl("hour|work|satis|lang|region|canton|desire|wish|want|job|employ", labsp, ignore.case = TRUE)
    cat("Wave-25 person work/satisfaction vars:\n")
    for (v in names(labsp)[idx2]) {
        cat(sprintf("  %-25s => %s\n", v, labsp[v]))
    }
}

# Also check household file for language region
h_file <- w25_files[grepl("_h_user", w25_files)]
if (length(h_file) > 0) {
    dh <- read_dta(h_file[1], n_max = 2)
    labsh <- sapply(dh, function(x) attr(x, "label"))
    idx3 <- grepl("lang|region|canton|commun|munic", labsh, ignore.case = TRUE)
    cat("\nWave-25 household language/region vars:\n")
    for (v in names(labsh)[idx3]) {
        cat(sprintf("  %-25s => %s\n", v, labsh[v]))
    }
}
