suppressPackageStartupMessages({
    library(tidyverse)
    library(fixest)
})

outfile <- "g:/My Drive/sandbox/ai-paper/data/log_explore2.txt"
sink(outfile)

base <- "g:/My Drive/sandbox/ai-paper"
df <- readRDS(file.path(base, "data/processed/shp_analysis.rds"))

cat("Rows:", nrow(df), " Cols:", ncol(df), "\n")
cat("Years:", min(df$year), "-", max(df$year), "\n\n")

pf <- function(ct, rows = NULL) {
    if (!is.null(rows)) {
        rows <- intersect(rows, rownames(ct))
        if (length(rows) == 0) {
            cat("  (no matching rows)\n")
            return(invisible())
        }
        ct <- ct[rows, , drop = FALSE]
    }
    for (i in seq_len(nrow(ct))) {
        s <- ifelse(ct[i, 4] < 0.01, "***", ifelse(ct[i, 4] < 0.05, "**", ifelse(ct[i, 4] < 0.1, "*", "")))
        cat(sprintf("  %-40s %9.4f (%7.4f) p=%.4f %s\n", rownames(ct)[i], ct[i, 1], ct[i, 2], ct[i, 4], s))
    }
}

safe_run <- function(label, expr) {
    cat(paste0("\n=== ", label, " ===\n"))
    tryCatch(expr, error = function(e) cat("  ERROR:", e$message, "\n"))
}

rd <- df %>% filter(!is.na(hours_gap), !is.na(is_latin), !is.na(pc44))

# ================================================================
# CURRENT MODELS
# ================================================================
safe_run("CURRENT: Event Study (wants_less)", {
    m <- feols(wants_less ~ i(year, is_latin, ref = 2019) + age + age_sq + female + has_kids | idpers + year,
        data = df %>% filter(!is.na(wants_less), !is.na(is_latin)), cluster = ~idpers
    )
    pf(coeftable(m), grep("year::", rownames(coeftable(m)), value = TRUE))
})

safe_run("CURRENT: Life Sat ~ hours_gap*french", {
    m <- feols(pc44 ~ hours_gap * is_latin + age + age_sq + female + has_kids | idpers + year, data = rd, cluster = ~idpers)
    pf(coeftable(m), c("hours_gap", "is_latin", "hours_gap:is_latin"))
})

safe_run("CURRENT: Job Sat ~ hours_gap*french", {
    m <- feols(pw228 ~ hours_gap * is_latin + age + age_sq + female + has_kids | idpers + year,
        data = rd %>% filter(!is.na(pw228)), cluster = ~idpers
    )
    pf(coeftable(m), c("hours_gap", "is_latin", "hours_gap:is_latin"))
})

# ================================================================
# NEW: Well-being mechanism outcomes
# ================================================================
safe_run("NEW 1: Work Stress (pw604) ~ hours_gap*french", {
    m <- feols(pw604 ~ hours_gap * is_latin + age + age_sq + female + has_kids | idpers + year,
        data = df %>% filter(!is.na(pw604), !is.na(hours_gap), !is.na(is_latin)), cluster = ~idpers
    )
    pf(coeftable(m), c("hours_gap", "is_latin", "hours_gap:is_latin"))
})

safe_run("NEW 2: Exhaustion (pf51) ~ hours_gap*french", {
    m <- feols(pf51 ~ hours_gap * is_latin + age + age_sq + female + has_kids | idpers + year,
        data = df %>% filter(!is.na(pf51), !is.na(hours_gap), !is.na(is_latin)), cluster = ~idpers
    )
    pf(coeftable(m), c("hours_gap", "is_latin", "hours_gap:is_latin"))
})

safe_run("NEW 3: Disconnect (pf52) ~ hours_gap*french", {
    m <- feols(pf52 ~ hours_gap * is_latin + age + age_sq + female + has_kids | idpers + year,
        data = df %>% filter(!is.na(pf52), !is.na(hours_gap), !is.na(is_latin)), cluster = ~idpers
    )
    pf(coeftable(m), c("hours_gap", "is_latin", "hours_gap:is_latin"))
})

safe_run("NEW 4: Work-Life Interf (pf50) ~ hours_gap*french", {
    m <- feols(pf50 ~ hours_gap * is_latin + age + age_sq + female + has_kids | idpers + year,
        data = df %>% filter(!is.na(pf50), !is.na(hours_gap), !is.na(is_latin)), cluster = ~idpers
    )
    pf(coeftable(m), c("hours_gap", "is_latin", "hours_gap:is_latin"))
})

safe_run("NEW 5: Sat Amount of Work (pw230) ~ hours_gap*french", {
    m <- feols(pw230 ~ hours_gap * is_latin + age + age_sq + female + has_kids | idpers + year,
        data = df %>% filter(!is.na(pw230), !is.na(hours_gap), !is.na(is_latin)), cluster = ~idpers
    )
    pf(coeftable(m), c("hours_gap", "is_latin", "hours_gap:is_latin"))
})

safe_run("NEW 6: Sat Work Conditions (pw93) ~ hours_gap*french", {
    m <- feols(pw93 ~ hours_gap * is_latin + age + age_sq + female + has_kids | idpers + year,
        data = df %>% filter(!is.na(pw93), !is.na(hours_gap), !is.na(is_latin)), cluster = ~idpers
    )
    pf(coeftable(m), c("hours_gap", "is_latin", "hours_gap:is_latin"))
})

safe_run("NEW 7: Free Time Sat (pa05) ~ hours_gap*french", {
    m <- feols(pa05 ~ hours_gap * is_latin + age + age_sq + female + has_kids | idpers + year,
        data = df %>% filter(!is.na(pa05), !is.na(hours_gap), !is.na(is_latin)), cluster = ~idpers
    )
    pf(coeftable(m), c("hours_gap", "is_latin", "hours_gap:is_latin"))
})

# ================================================================
# NEW: Event study heterogeneity
# ================================================================
safe_run("NEW 8: Event Study WOMEN ONLY", {
    m <- feols(wants_less ~ i(year, is_latin, ref = 2019) + age + age_sq + has_kids | idpers + year,
        data = df %>% filter(!is.na(wants_less), !is.na(is_latin), female == 1), cluster = ~idpers
    )
    pf(coeftable(m), grep("year::", rownames(coeftable(m)), value = TRUE))
})

safe_run("NEW 9: Event Study MEN ONLY", {
    m <- feols(wants_less ~ i(year, is_latin, ref = 2019) + age + age_sq + has_kids | idpers + year,
        data = df %>% filter(!is.na(wants_less), !is.na(is_latin), female == 0), cluster = ~idpers
    )
    pf(coeftable(m), grep("year::", rownames(coeftable(m)), value = TRUE))
})

safe_run("NEW 10: Event Study MILLENNIALS ONLY", {
    m <- feols(wants_less ~ i(year, is_latin, ref = 2019) + age + age_sq + female + has_kids | idpers + year,
        data = df %>% filter(!is.na(wants_less), !is.na(is_latin), cohort == "Millennial"), cluster = ~idpers
    )
    pf(coeftable(m), grep("year::", rownames(coeftable(m)), value = TRUE))
})

safe_run("NEW 11: Event Study TERTIARY EDU ONLY", {
    m <- feols(wants_less ~ i(year, is_latin, ref = 2019) + age + age_sq + female + has_kids | idpers + year,
        data = df %>% filter(!is.na(wants_less), !is.na(is_latin), edu_cat == "Tertiary"), cluster = ~idpers
    )
    pf(coeftable(m), grep("year::", rownames(coeftable(m)), value = TRUE))
})

# ================================================================
# NEW: overwork_5h as alternative DV
# ================================================================
safe_run("NEW 12: overwork_5h Event Study", {
    m <- feols(overwork_5h ~ i(year, is_latin, ref = 2019) + age + age_sq + female + has_kids | idpers + year,
        data = df %>% filter(!is.na(overwork_5h), !is.na(is_latin)), cluster = ~idpers
    )
    pf(coeftable(m), grep("year::", rownames(coeftable(m)), value = TRUE))
})

# ================================================================
# NEW: Triple interaction (no year FE, use post_2020)
# ================================================================
safe_run("NEW 13: Life Sat ~ hours_gap*french*post_2020 (canton+year FE)", {
    m <- feols(pc44 ~ hours_gap * is_latin * post_2020 + age + age_sq + female + has_kids | canton + year,
        data = rd, cluster = ~canton
    )
    ct <- coeftable(m)
    target <- grep("hours_gap|is_latin|post_2020", rownames(ct), value = TRUE)
    pf(ct, target)
})

# ================================================================
# NEW: DiD with canton FE (not individual FE, so post_2020 not collinear)
# ================================================================
safe_run("NEW 14: DiD wants_less ~ post_2020*french (canton+year FE)", {
    m <- feols(wants_less ~ is_latin * post_2020 + age + age_sq + female + has_kids | canton + year,
        data = df %>% filter(!is.na(wants_less), !is.na(is_latin)), cluster = ~canton
    )
    pf(coeftable(m))
})

# ================================================================
# NEW: Stress/burnout outcomes in event study format
# ================================================================
safe_run("NEW 15: Event Study on EXHAUSTION (pf51)", {
    m <- feols(pf51 ~ i(year, is_latin, ref = 2019) + age + age_sq + female + has_kids | idpers + year,
        data = df %>% filter(!is.na(pf51), !is.na(is_latin)), cluster = ~idpers
    )
    pf(coeftable(m), grep("year::", rownames(coeftable(m)), value = TRUE))
})

safe_run("NEW 16: Event Study on WORK STRESS (pw604)", {
    m <- feols(pw604 ~ i(year, is_latin, ref = 2019) + age + age_sq + female + has_kids | idpers + year,
        data = df %>% filter(!is.na(pw604), !is.na(is_latin)), cluster = ~idpers
    )
    pf(coeftable(m), grep("year::", rownames(coeftable(m)), value = TRUE))
})

sink()
cat("Results saved to", outfile, "\n")
