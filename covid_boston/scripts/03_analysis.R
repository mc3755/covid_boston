# =============================================================================
# Title:       03_analysis.R
# Author:      Tahir Arif, MPH
# Date:        March 2026
# Description: Epidemiologic analysis of COVID-19 ED visits in Massachusetts.
#              Computes week-over-week changes, surge period identification,
#              wave-level summaries, and college-age (18-24) stratification
#              vs. all-ages population comparisons.
# =============================================================================

# ---- Libraries ---------------------------------------------------------------
library(tidyverse)   # dplyr, tidyr, readr, stringr, purrr
library(janitor)     # tabyl(), adorn_totals()
library(lubridate)   # Date manipulation
library(scales)      # Number formatting
library(here)        # Relative paths

# ---- Paths -------------------------------------------------------------------
CLEAN_PATH      <- here("data", "clean_covid.csv")
OUT_WEEKLY      <- here("output", "table_covid_weekly.csv")
OUT_WAVE        <- here("output", "table_covid_wave_summary.csv")
OUT_WOW         <- here("output", "table_covid_wow_changes.csv")
OUT_AGE         <- here("output", "table_covid_age_comparison.csv")
OUT_SURGES      <- here("output", "table_covid_surge_periods.csv")

# ---- Load data ---------------------------------------------------------------
message("Loading cleaned COVID-19 data from: ", CLEAN_PATH)
covid <- read_csv(CLEAN_PATH, show_col_types = FALSE)

# Identify primary percentage column
pct_col <- names(covid)[str_detect(names(covid), "pct_ed_visits|percent_visits|percent|visits_pct")][1]
message("Primary column: ", pct_col)

# ---- Analysis 1: Weekly summary (all ages) ------------------------------------
# Aggregate to one row per week; if age groups exist, use "all ages" row
# or aggregate across groups

weekly_all <- covid |>
  {
    if ("age_group" %in% names(covid)) {
      filter(., str_detect(tolower(age_group), "all|overall|total"))
    } else {
      .
    }
  } |>
  group_by(week_end_date, mmwr_week, mmwr_year, wave) |>
  summarise(
    pct_ed_visits    = mean(.data[[pct_col]], na.rm = TRUE),
    wow_abs_change   = mean(wow_abs_change,   na.rm = TRUE),
    wow_pct_change   = mean(wow_pct_change,   na.rm = TRUE),
    trend_direction  = first(trend_direction),
    surge_flag       = any(surge_flag, na.rm = TRUE),
    .groups          = "drop"
  ) |>
  arrange(week_end_date)

write_csv(weekly_all, OUT_WEEKLY)
message("Weekly summary saved to: ", OUT_WEEKLY)

# ---- Analysis 2: Wave-level summary ------------------------------------------
# For each named COVID-19 wave: mean %, peak %, duration, surge weeks

wave_summary <- weekly_all |>
  group_by(wave) |>
  summarise(
    start_date        = min(week_end_date),
    end_date          = max(week_end_date),
    n_weeks           = n(),
    mean_pct          = round(mean(pct_ed_visits, na.rm = TRUE), 3),
    sd_pct            = round(sd(pct_ed_visits, na.rm = TRUE), 3),
    peak_pct          = round(max(pct_ed_visits, na.rm = TRUE), 3),
    peak_week_date    = as.character(week_end_date[which.max(pct_ed_visits)]),
    n_surge_weeks     = sum(surge_flag, na.rm = TRUE),
    pct_surge_weeks   = round(n_surge_weeks / n_weeks * 100, 1),
    .groups           = "drop"
  ) |>
  arrange(start_date)

message("\nWave summary:")
print(wave_summary)

write_csv(wave_summary, OUT_WAVE)
message("Wave summary saved to: ", OUT_WAVE)

# ---- Analysis 3: Week-over-week change analysis ------------------------------
# Identify the largest single-week increases and decreases

wow_analysis <- weekly_all |>
  filter(!is.na(wow_pct_change)) |>
  mutate(
    wow_category = case_when(
      wow_pct_change >= 50  ~ "Large increase (≥50%)",
      wow_pct_change >= 20  ~ "Moderate increase (20-49%)",
      wow_pct_change >= 10  ~ "Small increase (10-19%)",
      wow_pct_change > -10  ~ "Stable (-10 to 10%)",
      wow_pct_change > -20  ~ "Small decrease (-10 to -20%)",
      wow_pct_change > -50  ~ "Moderate decrease (-20 to -50%)",
      TRUE                  ~ "Large decrease (>50%)"
    )
  )

# Summary table of WoW change categories
wow_category_tbl <- wow_analysis |>
  count(wow_category) |>
  mutate(pct_of_weeks = round(n / sum(n) * 100, 1)) |>
  arrange(desc(n))

message("\nWoW change categories:")
print(wow_category_tbl)

# Top 10 largest single-week surges
message("\nTop 10 largest week-over-week increases:")
wow_analysis |>
  slice_max(wow_pct_change, n = 10) |>
  select(week_end_date, wave, pct_ed_visits, wow_abs_change, wow_pct_change) |>
  print()

write_csv(wow_analysis, OUT_WOW)
message("WoW analysis saved to: ", OUT_WOW)

# ---- Analysis 4: College-age (18-24) vs. all-ages comparison -----------------
if ("age_group" %in% names(covid) && any(!is.na(covid$college_age))) {
  
  message("\nRunning college-age stratification...")
  
  # Extract overall and college-age rows
  all_ages_df     <- covid |> filter(str_detect(tolower(age_group), "all|overall|total"))
  college_age_df  <- covid |> filter(college_age == TRUE | str_detect(tolower(age_group), "18.?24"))
  
  # Compute weekly summaries for each group
  summarise_group <- function(df, group_label) {
    df |>
      group_by(week_end_date, wave) |>
      summarise(
        pct_ed_visits = mean(.data[[pct_col]], na.rm = TRUE),
        .groups       = "drop"
      ) |>
      mutate(age_group_label = group_label)
  }
  
  age_comparison <- bind_rows(
    summarise_group(all_ages_df,    "All Ages"),
    summarise_group(college_age_df, "College Age (18-24)")
  )
  
  # Pivot wide for ratio calculation
  age_wide <- age_comparison |>
    pivot_wider(
      id_cols      = c(week_end_date, wave),
      names_from   = age_group_label,
      values_from  = pct_ed_visits
    ) |>
    janitor::clean_names() |>
    mutate(
      college_to_all_ratio = round(college_age_18_24 / all_ages, 2)
    )
  
  write_csv(age_wide, OUT_AGE)
  message("Age comparison saved to: ", OUT_AGE)
  
  # Summary by wave
  age_wave_summary <- age_comparison |>
    group_by(age_group_label, wave) |>
    summarise(
      mean_pct = round(mean(pct_ed_visits, na.rm = TRUE), 3),
      peak_pct = round(max(pct_ed_visits, na.rm = TRUE), 3),
      .groups  = "drop"
    )
  message("\nAge-stratified wave summary:")
  print(age_wave_summary)
  
} else {
  message("\nNo age_group data available — skipping college-age analysis.")
  tibble(note = "Age-stratified analysis requires age_group variable in NSSP pull.") |>
    write_csv(OUT_AGE)
}

# ---- Analysis 5: Surge period table ------------------------------------------
# Identify contiguous surge periods (consecutive surge weeks)

surge_periods <- weekly_all |>
  filter(!is.na(surge_flag)) |>
  arrange(week_end_date) |>
  mutate(
    # Identify run breaks: a new surge starts when surge status changes
    surge_run = cumsum(surge_flag != lag(surge_flag, default = FALSE))
  ) |>
  filter(surge_flag) |>
  group_by(surge_run) |>
  summarise(
    surge_start  = min(week_end_date),
    surge_end    = max(week_end_date),
    n_weeks      = n(),
    peak_pct     = round(max(pct_ed_visits), 3),
    wave         = first(wave),
    .groups      = "drop"
  ) |>
  select(-surge_run) |>
  arrange(surge_start)

message("\nSurge periods identified:")
print(surge_periods)

write_csv(surge_periods, OUT_SURGES)
message("Surge periods saved to: ", OUT_SURGES)

# ---- Print summary -----------------------------------------------------------
message("\n=== KEY FINDINGS ===")
message("Total weeks analyzed: ", nrow(weekly_all))
message("Waves covered: ", paste(unique(wave_summary$wave), collapse = " | "))
message("Overall mean % ED visits: ", round(mean(weekly_all$pct_ed_visits, na.rm = TRUE), 2), "%")
message("Overall peak % ED visits: ", round(max(weekly_all$pct_ed_visits, na.rm = TRUE), 2), "%")
message("Total surge periods: ", nrow(surge_periods))
