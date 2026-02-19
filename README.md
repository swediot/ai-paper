# The Cultural Cost of Overwork: Evidence from the Swiss Röstigraben

This repository contains the source code, data analysis scripts, and LaTeX source for the paper "The Cultural Cost of Overwork: Evidence from the Swiss Röstigraben".

## Abstract

We exploit Switzerland's linguistic border between French- and German-speaking regions — the *Röstigraben* — to test whether cultural norms shape the psychological cost of overwork. Using 25 waves of the Swiss Household Panel (1999–2023), we show that overwork (the gap between actual and contractual hours) is significantly more psychologically costly for French-speaking workers. Each additional hour of overwork increases exhaustion, work-life interference, and difficulty disconnecting more in French-speaking regions than in German-speaking ones, despite identical institutional constraints.

## Prerequisites

You will need the following software installed:

1.  **R**: [Download R](https://cran.r-project.org/)
    *   Required packages: `tidyverse`, `fixest`, `modelsummary`, `sf`, `viridis`, `patchwork`, `kableExtra`
2.  **LaTeX Distribution**: [Download TeX Live](https://www.tug.org/texlive/) or [MiKTeX](https://miktex.org/)

## Step 1: Run Data Analysis (R)

Open R or RStudio in the project root directory and run the scripts in numerical order:

```bash
# 1. Clean Data (SHP)
Rscript code/01_clean_shp.R

# 2. Construct Analysis Variables
Rscript code/02_vars.R

# 3. Main Analysis: Regressions, Tables, and Figures
Rscript code/03_analysis.R

# 4. Robustness Checks
Rscript code/04_robustness.R

# 5. Appendix Figures
Rscript code/05_appendix_figures.R
```

This will generate all necessary figures in `figures/` and tables in `tables/`.

## Step 2: Compile Paper (LaTeX)

Once the figures and tables are generated, compile the LaTeX document:

```bash
cd paper
pdflatex main.tex
bibtex main
pdflatex main.tex
pdflatex main.tex
```

The final PDF will be located at `paper/main.pdf`.