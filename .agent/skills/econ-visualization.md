---
name: econ-visualization
description: Create publication-quality charts and graphs for economics papers using ggplot2.
workflow_stage: communication
compatibility:
  - claude-code
  - cursor
  - codex
  - gemini-cli
author: Awesome Econ AI Community
version: 1.1.0
tags:
  - visualization
  - ggplot2
  - charts
  - publication
---

# Econ Visualization

## Purpose

This skill creates publication-quality figures for economics papers, using clean styling, consistent scales, and export-ready formats. It combines general best practices with economics-specific conventions.

## When to Use

- Building figures for empirical results and descriptive analysis
- Standardizing chart style across a paper or presentation
- Exporting figures to PDF or PNG at journal quality

## Instructions

### Step 1: Understand the Context

Before generating any code, ask the user:
- What is the dataset and key variables?
- What chart type is needed (line, bar, scatter, event study)?
- What output format and size are required?

### Step 2: Generate the Output

Based on the context, generate code that:
1. **Uses a consistent theme** (see template below)
2. **Labels axes and legends clearly** with proper capitalization and units
3. **Exports figures** at high resolution (PDF for LaTeX)
4. **Includes reproducible steps** for data preparation

### Step 3: Verify and Explain

After generating output:
- Explain how to regenerate or update the plot
- Suggest alternatives (log scales, faceting, smoothing)
- Verify that color schemes are colorblind-friendly and printable in grayscale/bw

## Standard Theme Template

Use this theme to ensure consistency across the paper.

```r
theme_paper <- function() {
  theme_minimal(base_size = 12, base_family = "serif") +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(), # Cleaner look often preferred in econ
      panel.border = element_rect(fill = NA, color = "black", linewidth = 0.5),
      legend.position = "bottom",
      plot.title = element_text(face = "bold", size = 13),
      plot.subtitle = element_text(size = 10, color = "grey40"),
      axis.title = element_text(face = "bold"),
      strip.text = element_text(face = "bold")
    )
}
theme_set(theme_paper())
```

## Example Output

```r
# ============================================
# Publication-Quality Figure in R
# ============================================
library(tidyverse)

df <- read_csv("data.csv")

ggplot(df, aes(x = year, y = gdp_per_capita, color = country)) +
  geom_line(linewidth = 1) + # Note: size is deprecated, use linewidth
  scale_y_continuous(labels = scales::comma) +
  scale_color_viridis_d(option = "mako", end = 0.9) + # Professional palette
  labs(
    title = "GDP per Capita Over Time",
    subtitle = "Constant 2015 USD",
    x = "Year",
    y = "GDP per Capita",
    color = "Country",
    caption = "Source: World Bank WDI."
  ) +
  theme_paper()

ggsave("figures/gdp_per_capita.pdf", width = 7, height = 4, device = cairo_pdf)
```

## Best Practices (Merged)

1. **Clarity**: Minimize chart junk. Every element should convey information.
2. **Consistency**: Use the same color schemes, fonts, and theme throughout.
3. **Formats**: Use vector formats (PDF) for LaTeX.
4. **Grayscale**: Ensure plots are readable when printed in black and white (use shapes + linetypes).
5. **Accessibility**: Use color palettes that are accessible (e.g., `viridis`, `Okabe-Ito`).

## Common Pitfalls

- Overcrowded plots without clear labeling
- Inconsistent scales across figures
- Exporting low-resolution images
- Using default ggplot2 gray background (often discouraged in journals)

## References

- [ggplot2 documentation](https://ggplot2.tidyverse.org/)
- [Tufte (2001) The Visual Display of Quantitative Information](https://www.edwardtufte.com/tufte/books_vdqi)
