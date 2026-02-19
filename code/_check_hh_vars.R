# Check household long file for language/canton/region variables
library(haven)

hh_path <- "g:/My Drive/sandbox/ai-paper/swiss-household-panel/data/SHP-Data-Longfile-STATA/shplong_h_user.dta"
dh <- read_dta(hh_path, n_max = 2)
cat("=== HH Long file columns ===\n")
cat("Total columns:", ncol(dh), "\n\n")

labs <- sapply(dh, function(x) attr(x, "label"))
# Show all vars with their labels
idx <- grepl("lang|region|canton|commun|munic|weight|house|urban|rural|child|noga", labs, ignore.case = TRUE)
cat("Language/region/canton/community HH vars:\n")
for (v in names(labs)[idx]) {
    cat(sprintf("  %-25s => %s\n", v, labs[v]))
}

# Also check what 'pw46' actually is
cat("\n\n=== Checking pw46 and pw84 in person long file ===\n")
p_path <- "g:/My Drive/sandbox/ai-paper/swiss-household-panel/data/SHP-Data-Longfile-STATA/shplong_p_user.dta"
dp <- read_dta(p_path, n_max = 100, col_select = c("idpers", "year", "pw46", "pw74", "pw77", "pw42", "pw84", "pw85", "pw228", "pc44", "wstat", "plingu", "idhous"))
cat("pw46 label:", attr(dp$pw46, "label"), "\n")
cat("pw74 label:", attr(dp$pw74, "label"), "\n")
cat("pw77 label:", attr(dp$pw77, "label"), "\n")
cat("pw42 label:", attr(dp$pw42, "label"), "\n")
cat("pw84 label:", attr(dp$pw84, "label"), "\n")
cat("pw228 label:", attr(dp$pw228, "label"), "\n")
cat("pc44 label:", attr(dp$pc44, "label"), "\n")

# Print value labels for pw84
cat("\npw84 value labels:\n")
vl <- attr(dp$pw84, "labels")
if (!is.null(vl)) print(vl)

# Summary of key vars
cat("\nSummary of pw46 (reference hours):\n")
print(summary(dp$pw46))
cat("\nSummary of pw74 (contractual hours):\n")
print(summary(dp$pw74))
cat("\nSummary of pw77 (hours worked per week):\n")
print(summary(dp$pw77))
cat("\nSummary of pw42 (part-time %):\n")
print(summary(dp$pw42))
cat("\nSummary of pw84 (preference):\n")
print(summary(dp$pw84))
