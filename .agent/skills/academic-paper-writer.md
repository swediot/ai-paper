---
name: academic-paper-writer
description: Comprehensive guide for drafting economics papers (Structure, Style, LaTeX, Tables, Math)
workflow_stage: writing
compatibility:
  - claude-code
  - cursor
  - codex
  - gemini-cli
author: Awesome Econ AI Community
version: 2.0.0
tags:
  - LaTeX
  - academic-writing
  - economics
  - tables
  - theory
---

# Academic Paper Writer

## Purpose

This master skill helps economists draft, structure, and polish academic papers. It consolidates guidance on:
1.  **Structure**: IMRAD organization
2.  **Style**: Economic prose and anti-patterns
3.  **LaTeX**: Project setup and templates
4.  **Math**: Formal model notation
5.  **Tables**: Publication-quality formatting

## 1. Paper Structure (IMRAD)

### Standard Organization
1.  **Introduction**: Motivation, Research Question, Contribution, Findings, Roadmap.
2.  **Literature Review**: Synthesize related work (don't just list).
3.  **Data & Methods**: Sources, sample, identification strategy.
4.  **Results**: Main estimates, interpretation, robustness.
5.  **Conclusion**: Summary, implications, limitations.

### Writing the Introduction
- **Hook**: Start with a concrete puzzle or fact, not generic background.
- **Contribution**: "We contribute by..." (be specific).
- **Results First**: State the main finding in the first paragraph.
- **Roadmap**: End with "Section 2 describes..."

## 2. Writing Style Guide

### Anti-Patterns (What to Avoid)
- **The "AI Voice"**: Avoid "Moreover", "Furthermore", "In conclusion", "delve", "tapestry", "landscape".
- **Hollow Summaries**: Don't say "Table 1 shows the results." Say "Table 1 shows that X increases Y by..."
- **Vague Intensifiers**: Avoid "crucial", "vital", "pivotal". Use precise terms.

### Best Practices
- **Active Voice**: "We estimate..." instead of "It is estimated that..."
- **Precision**: Distinguish "effect" (causal) from "association" (correlational).
- **Signaling**: Use "We find", "We argue" to distinguish your work from citations.
- **Conciseness**: Delete the first 5 words of a sentence if possible (e.g., "It is important to note that...").
- **Formatting**: Never bold any words in a text, use italics extremely sparingly

## 3. LaTeX Setup

### Project Structure
- `main.tex`: Root file
- `sections/`: `01_intro.tex`, `02_lit_review.tex`, etc.
- `tables/`: Generated .tex files
- `figures/`: PDF plots
- `references.bib`: BibTeX file

### Preamble Template
```latex
\documentclass[12pt]{article}
\usepackage[utf8]{inputenc}
\usepackage{geometry}
\geometry{a4paper, margin=1in}
\usepackage{amsmath, amssymb, amsthm} # Math
\usepackage{graphicx}
\usepackage{booktabs} # Tables
\usepackage{natbib}   # Citations
\usepackage{hyperref} # Links
\usepackage{setspace}
\onehalfspacing

\title{Title of the Paper}
\author{Author Name}
\date{\today}

\begin{document}
\maketitle
\begin{abstract}Abstract...\end{abstract}
\input{sections/01_intro}
% ...
\bibliography{references}
\bibliographystyle{aer}
\end{document}
```

## 4. Tables and Results

### Formatting Rules
1.  **Use `booktabs`**: `\toprule`, `\midrule`, `\bottomrule`. No vertical lines.
2.  **Self-Contained**: Captions must explain the table without reading text.
3.  **Notes**: Define all variables, standard errors, and stars in table notes.

### Example Regression Table
```latex
\begin{table}[htbp]\centering
\caption{Effect of Treatment on Outcome}
\label{tab:main}
\begin{tabular}{lccc}
\toprule
 & (1) OLS & (2) +FE & (3) Preferred \\
\midrule
Treatment & 0.125*** & 0.102** & 0.098** \\
          & (0.041)  & (0.046) & (0.045) \\
\midrule
Observations & 2,145 & 2,145 & 2,145 \\
R-squared    & 0.18  & 0.31  & 0.35 \\
\bottomrule
\end{tabular}
\begin{tablenotes}
\small
\item Notes: Standard errors clustered at state level. * p<0.1, ** p<0.05, *** p<0.01.
\end{tablenotes}
\end{table}
```

## 5. Math and Theory

### Notation Conventions
- **Utility**: $u(\cdot)$, $U$
- **Parameters**: Greek letters ($\alpha, \beta, \delta$)
- **Time**: Subscript $t$ ($c_t$, $y_t$)
- **Expectation**: $\mathbb{E}[\cdot]$

### Mathematical Model Template
```latex
\section{Model}
Consider an agent maximizing lifetime utility:
\begin{equation}
    \max_{\{c_t\}_{t=0}^\infty} \sum_{t=0}^\infty \beta^t u(c_t)
\end{equation}
subject to the budget constraint:
\begin{equation} \label{eq:bc}
    c_t + k_{t+1} = w_t + (1+r_t)k_t
\end{equation}
The first-order condition with respect to $c_t$ yields the Euler equation:
\begin{equation}
    u'(c_t) = \beta (1+r_{t+1}) u'(c_{t+1})
\end{equation}
```

## References
- [Cochrane (2005) Writing Tips for PhD Students](https://www.johnhcochrane.com/research-all/writing-tips-for-phd-studentsnbsp)
- [Thomson (2011) A Guide for the Young Economist](https://mitpress.mit.edu/books/guide-young-economist)
- [AEA Author Guidelines](https://www.aeaweb.org/journals/policies/author-instructions)
