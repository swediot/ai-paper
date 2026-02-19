# Quiet Quitting Paper - Compilation Instructions

This project contains the source code and data analysis scripts for the paper "The Cultural Bounds of 'Quiet Quitting'".

## Prerequisites
You will need the following software installed:
1.  **R**: [Download R](https://cran.r-project.org/)
2.  **LaTeX Distribution**: [Download TeX Live](https://www.tug.org/texlive/) or [MiKTeX](https://miktex.org/)

## Step 1: Run Data Analysis (R)
Open R or RStudio in the project root directory and run the scripts in order:

```bash
# 1. Clean Data
Rscript code/01_clean_shp.R

# 2. Construct Variables
Rscript code/02_vars.R

# 3. Run Analysis and Generate Tables/Figures
Rscript code/03_analysis.R
```

This will generate:
- `figures/fig_event_study.pdf`
- `tables/tab_event_study.tex`
- `tables/tab_wellbeing.tex`

## Step 2: Compile Paper (LaTeX)
Once the figures and tables are generated, compile the LaTeX document:

```bash
cd paper
pdflatex main.tex
bibtex main
pdflatex main.tex
pdflatex main.tex
```

The final PDF will be `paper/main.pdf`.