# =============================================================================
# Title:       02_clean.R
# Author:      Tahir Arif, MPH
# Date:        March 2026
# Description: Clean raw NSSP COVID-19 ED visit data. Standardizes columns,
#              parses dates, derives MMWR week variables, computes week-over-
#              week change, and flags the college-age population (18-24 years).
# =============================================================================

# ---- Libraries ---------------------------------------------------------------
library(tidyverse)   # dplyr, tidyr, readr, stringr, forcats
library(janitor)     # clean_names(), remove_empty()
library(lubridate)   # Date parsing
library(MMWRweek)    # MMWR epidemiologic week
library(zoo)         # Rolling averages (rollmean)
library(here)        # Relative paths

# ---- Paths -------------------------------------------------------------------
RAW_PATH   <- here("data", "raw_covid_nssp.csv")
CLEAN_PATH <- here("data", "clean_covid.csv")

# ---- Load raw data -----------------------------------------------------------
message("Loading raw COVID-19 data from: ", RAW_PATH)
covid_raw <- read_csv(RAW_PATH, show_col_types = FALSE)
message("Raw dimensions: ", nrow(covid_raw), " rows × ", ncol(covid_raw), " cols")

# ---- Step 1: Standardize column names ----------------------------------------
covid <- covid_raw |>
  clean_names() |>
  remove_empty(which = c("rows", "cols"))

message("Cleaned column names: ", paste(names(covid), collapse = ", "))

# ---- Step 2: Parse dates and derive time variables ---------------------------
covid <- covid |>
  mutate(
    week_end_date   = as_date(parse_date_time(week_end_date,
                               orders = c("ymdHMS", "ymd", "mdy"))),
    week_start_date = week_end_date - days(6),
    year            = year(week_end_date),
    month_num       = month(week_end_date),
    month           = month(week_end_date, label = TRUE, abbr = TRUE)
  )

# ---- Step 3: MMWR week -------------------------------------------------------
mmwr_vars <- MMWRweek(covid$week_end_date)
covid <- covid |>
  mutate(
    mmwr_week = mmwr_vars$MMWRweek,
    mmwr_year = mmwr_vars$MMWRyear
  )

# ---- Step 4: Classify COVID-19 waves / seasons --------------------------------
# COVID-19 does not follow a strict respiratory season like influenza.
# We classify into named pandemic/endemic waves by approximate date.

classify_covid_wave <- function(date) {
  dplyr::case_when(
    date < as_date("2021-01-01") ~ "Wave 1–2 (2020)",
    date < as_date("2021-07-01") ~ "Alpha Wave (early 2021)",
    date < as_date("2022-01-01") ~ "Delta Wave (mid 2021)",
    date < as_date("2022-07-01") ~ "Omicron BA.1/BA.2 (early 2022)",
    date < as_date("2023-01-01") ~ "Omicron BA.4/BA.5 (mid 2022)",
    date < as_date("2023-07-01") ~ "XBB Variants (early 2023)",
    date < as_date("2024-01-01") ~ "EG.5/HV.1 (mid 2023)",
    date < as_date("2024-07-01") ~ "JN.1 Wave (early 2024)",
    TRUE                         ~ "KP/LF Variants (2024–2025)"
  )
}

covid <- covid |>
  mutate(wave = classify_covid_wave(week_end_date))

message("Waves identified:")
print(table(covid$wave))

# ---- Step 5: Coerce numeric columns -----------------------------------------
# Identify columns that should be numeric (percentages, counts)
pct_cols <- names(covid)[str_detect(names(covid), "percent|pct|rate|count|visits|num")]

covid <- covid |>
  mutate(across(all_of(pct_cols), ~ suppressWarnings(as.numeric(.x))))

# Identify the primary ED visit percentage column
pct_col <- names(covid)[str_detect(names(covid), "pct_ed_visits|percent_visits|percent|visits_pct")][1]
message("Primary percentage column: ", pct_col)

# ---- Step 6: College-age population flag (18-24 years) -----------------------
# Flag records corresponding to the 18-24 age group.
# Boston has a very large college population — this stratum is epidemiologically
# important for understanding community transmission dynamics.

if ("age_group" %in% names(covid)) {
  covid <- covid |>
    mutate(
      college_age = str_detect(
        tolower(age_group),
        "18.?24|18 to 24|18-24|18_24|young adult"
      )
    )
  message("College-age records (18-24): ", sum(covid$college_age, na.rm = TRUE))
  message("Age groups present:")
  print(sort(unique(covid$age_group)))
} else {
  # No age column — add placeholder flag
  covid <- covid |>
    mutate(college_age = NA_integer_)
  message("No age_group column found — college_age flag set to NA.")
}

# ---- Step 7: Week-over-week (WoW) change -------------------------------------
# Compute WoW change for all-ages records (or all rows if no age column).
# WoW = (current week % - prior week %) / prior week % × 100
#
# Steps:
#   1. Arrange by date
#   2. Lag one row to get prior week value
#   3. Compute absolute change and percent change

wow_df <- covid |>
  # If age groups exist, compute WoW separately per age group
  {
    if ("age_group" %in% names(covid)) {
      group_by(., age_group)
    } else {
      .
    }
  } |>
  arrange(week_end_date) |>
  mutate(
    prior_week_pct    = lag(.data[[pct_col]], 1),
    wow_abs_change    = .data[[pct_col]] - prior_week_pct,
    wow_pct_change    = if_else(
      prior_week_pct > 0,
      (wow_abs_change / prior_week_pct) * 100,
      NA_real_
    ),
    # Direction flag: increasing, decreasing, stable (±10%)
    trend_direction   = case_when(
      is.na(wow_pct_change)         ~ "Unknown",
      wow_pct_change > 10           ~ "Increasing",
      wow_pct_change < -10          ~ "Decreasing",
      TRUE                          ~ "Stable"
    )
  ) |>
  ungroup()

covid <- wow_df

# ---- Step 8: Rolling 4-week mean and surge flag ------------------------------
# Surge = current week > 2× 4-week rolling average
# Use zoo::rollmean with k=4, align="right" to avoid look-ahead bias

covid <- covid |>
  {
    if ("age_group" %in% names(covid)) {
      group_by(., age_group)
    } else {
      .
    }
  } |>
  arrange(week_end_date) |>
  mutate(
    rolling_4wk_mean = zoo::rollmean(.data[[pct_col]], k = 4,
                                     fill = NA, align = "right"),
    surge_flag       = .data[[pct_col]] > 2 * rolling_4wk_mean
  ) |>
  ungroup()

message("Surge weeks identified: ", sum(covid$surge_flag, na.rm = TRUE))

# ---- Step 9: Handle missing values -------------------------------------------
missing_report <- covid |>
  summarise(across(everything(), ~ sum(is.na(.)))) |>
  pivot_longer(everything(), names_to = "column", values_to = "n_missing") |>
  filter(n_missing > 0) |>
  arrange(desc(n_missing))

message("\nMissingness report:")
print(missing_report)

# Remove rows with missing week_end_date (ungroupable in time)
covid <- covid |> filter(!is.na(week_end_date))

# ---- Save cleaned data -------------------------------------------------------
write_csv(covid, CLEAN_PATH)
message("\nClean COVID-19 data saved to: ", CLEAN_PATH)
message("Rows: ", nrow(covid), "  Cols: ", ncol(covid))
