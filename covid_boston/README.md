# COVID-19 Emergency Department Surveillance — Boston, MA

## Project Description

This repository analyzes COVID-19-associated emergency department (ED) visits in Massachusetts using CDC's National Syndromic Surveillance Program (NSSP) BioSense Platform data. The analysis emphasizes week-over-week change dynamics and a special focus on the college-age population (18–24 years), a key demographic in Boston's population given the city's concentration of higher-education institutions.

The pipeline ingests NSSP syndromic surveillance data, computes week-over-week percentage changes, identifies surge periods, and stratifies trends by age group — with particular attention to whether college-age visit patterns diverge from the general Boston population.

---

## Data Sources

| Source | Description | URL |
|--------|-------------|-----|
| CDC NSSP ED Visits | Weekly COVID-19-related ED visit percentages by state | https://data.cdc.gov/resource/7xva-uux8.json |
| CDC NSSP Trajectories | State-level trend trajectories for respiratory pathogens | https://data.cdc.gov/resource/rdmq-nq56.json |

---

## Methods Summary

1. **Data acquisition**: CDC NSSP data queried for COVID-19 pathogen in Massachusetts via Socrata REST API.
2. **Data cleaning**: Column names standardized; MMWR weeks, dates, and week-over-week change variables derived; college-age population (18–24) flagged.
3. **Analysis**: Overall week-over-week change calculated; all-population vs. college-age trends compared; surge periods identified using a 2× rolling-mean threshold.
4. **Visualization**: Time series, week-over-week bar charts, and age-stratified comparison charts.

---

## Repository Structure

```
covid_boston/
├── README.md
├── .gitignore
├── scripts/
│   ├── 01_data_pull.R       # Query CDC NSSP for COVID-19 data
│   ├── 02_clean.R           # Clean data, create WoW change and college-age flag
│   ├── 03_analysis.R        # WoW analysis, age stratification, rate calculations
│   └── 04_visualization.R   # Time series, change charts, age-stratified plots
├── data/                    # Raw and cleaned data (gitignored)
└── output/                  # Tables and figures (gitignored)
```

---

## How to Run

```r
source("scripts/01_data_pull.R")
source("scripts/02_clean.R")
source("scripts/03_analysis.R")
source("scripts/04_visualization.R")
```

---

## Required R Packages

```r
install.packages(c(
  "tidyverse",   # dplyr, ggplot2, tidyr, readr, stringr
  "janitor",     # clean_names()
  "httr",        # HTTP requests
  "jsonlite",    # JSON parsing
  "lubridate",   # Date manipulation
  "MMWRweek",    # MMWR week calculation
  "viridis",     # Color palettes
  "scales",      # Axis formatting
  "zoo",         # Rolling averages
  "here"         # Relative paths
))
```

---

## Author

**Tahir Arif, MPH**  
Epidemiologist  
Date: March 2026

---

## Notes

- College-age population (18–24) is flagged using NSSP age group variables where available. Boston has an estimated 150,000+ college students, making this age group epidemiologically important.
- Week-over-week change is computed as: `(current_week - prior_week) / prior_week × 100`. Undefined for the first week or when the prior week is zero.
- COVID-19 surges are flagged when a week's ED visit % exceeds 2× the 4-week rolling mean.
