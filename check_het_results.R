library(tidyverse)
library(fixest)

base <- "g:/My Drive/sandbox/ai-paper"
df <- readRDS(file.path(base, "data/processed/shp_analysis.rds"))

# Define subgroups
reg_data <- df %>% filter(!is.na(hours_gap), !is.na(is_latin))

outcomes <- c("pf51", "pf50", "pf52")
names(outcomes) <- c("Exhaustion", "WLI", "Disconnect")

# Write to file
sink("het_results.txt")

for (outcome_name in names(outcomes)) {
    y <- outcomes[[outcome_name]]
    cat("\n==============================================\n")
    cat("OUTCOME:", outcome_name, "(", y, ")\n")
    cat("==============================================\n")

    dat <- reg_data %>% filter(!is.na(.data[[y]]))

    fml <- as.formula(paste(y, "~ hours_gap * is_latin + age + age_sq + female + has_kids | idpers + year"))
    fml_no_female <- as.formula(paste(y, "~ hours_gap * is_latin + age + age_sq + has_kids | idpers + year"))
    fml_no_kids <- as.formula(paste(y, "~ hours_gap * is_latin + age + age_sq + female | idpers + year"))

    get_coef <- function(lbl, model) {
        co <- coef(model)["hours_gap:is_latin"]
        se <- se(model, cluster = ~idpers)["hours_gap:is_latin"]
        cat(sprintf("%-15s: %.4f (SE: %.4f)\n", lbl, co, se))
    }

    # Full
    m <- feols(fml, dat, cluster = ~idpers)
    get_coef("Full Sample", m)

    # Gender
    m <- feols(fml_no_female, dat %>% filter(female == 1), cluster = ~idpers)
    get_coef("Women", m)
    m <- feols(fml_no_female, dat %>% filter(female == 0), cluster = ~idpers)
    get_coef("Men", m)

    # Parents
    m <- feols(fml_no_kids, dat %>% filter(has_kids == 1), cluster = ~idpers)
    get_coef("Parents", m)
    m <- feols(fml_no_kids, dat %>% filter(has_kids == 0), cluster = ~idpers)
    get_coef("Non-Parents", m)

    # Managers (occupa == 1)
    m <- feols(fml, dat %>% filter(occupa == 1), cluster = ~idpers)
    get_coef("Managers", m)
    m <- feols(fml, dat %>% filter(occupa != 1), cluster = ~idpers)
    get_coef("Non-Managers", m)

    # Border
    m <- feols(fml, dat %>% filter(is_border == 1), cluster = ~idpers)
    get_coef("Border", m)
    m <- feols(fml, dat %>% filter(is_border == 0), cluster = ~idpers)
    get_coef("Non-Border", m)
}
sink()
