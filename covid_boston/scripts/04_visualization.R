# =============================================================================
# Title:       04_visualization.R
# Author:      Tahir Arif, MPH
# Date:        March 2026
# Description: ggplot2 visualizations of COVID-19 ED visit trends in
#              Massachusetts. Produces: (1) annotated time series with wave
#              shading, (2) week-over-week change bar chart, (3) age-stratified
#              comparison (all ages vs. college-age 18-24).
# =============================================================================

# ---- Libraries ---------------------------------------------------------------
library(tidyverse)   # ggplot2, dplyr, tidyr, readr, forcats
library(lubridate)   # Date operations
library(viridis)     # Color scales
library(scales)      # Axis formatting
library(here)        # Relative paths

# ---- Shared plot theme -------------------------------------------------------
theme_epi <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title       = element_text(face = "bold", size = base_size + 2, hjust = 0),
      plot.subtitle    = element_text(size = base_size - 1, color = "grey40", hjust = 0),
      plot.caption     = element_text(size = base_size - 3, color = "grey55", hjust = 0),
      axis.title       = element_text(size = base_size - 1, color = "grey30"),
      axis.text        = element_text(size = base_size - 2, color = "grey30"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "grey92", linewidth = 0.4),
      legend.position  = "bottom",
      legend.title     = element_text(face = "bold", size = base_size - 2),
      legend.text      = element_text(size = base_size - 2),
      plot.margin      = margin(12, 16, 8, 12)
    )
}
theme_set(theme_epi())

# ---- Paths -------------------------------------------------------------------
CLEAN_PATH <- here("data", "clean_covid.csv")
WEEKLY_PATH <- here("output", "table_covid_weekly.csv")
WOW_PATH    <- here("output", "table_covid_wow_changes.csv")
AGE_PATH    <- here("output", "table_covid_age_comparison.csv")
WAVE_PATH   <- here("output", "table_covid_wave_summary.csv")

OUT_TS      <- here("output", "fig_covid_timeseries.png")
OUT_WOW     <- here("output", "fig_covid_wow_changes.png")
OUT_AGE     <- here("output", "fig_covid_age_comparison.png")

# ---- Load data ---------------------------------------------------------------
message("Loading data...")
weekly_df <- read_csv(WEEKLY_PATH, show_col_types = FALSE)
wow_df    <- read_csv(WOW_PATH,    show_col_types = FALSE)
wave_df   <- read_csv(WAVE_PATH,   show_col_types = FALSE)

# Load age comparison only if it has real data
age_raw <- read_csv(AGE_PATH, show_col_types = FALSE)
has_age_data <- !"note" %in% names(age_raw)

# ---- Figure 1: Time series with wave shading ---------------------------------
# Shade named COVID-19 wave periods; annotate the overall peak week

# Build wave shading rectangles
wave_rects <- wave_df |>
  select(wave, start_date, end_date) |>
  mutate(
    start_date = as_date(start_date),
    end_date   = as_date(end_date)
  )

# Color palette: one subdued color per wave
n_waves     <- nrow(wave_rects)
wave_colors <- viridis(n_waves, option = "D", alpha = 0.12, begin = 0.1, end = 0.9)

fig1 <- ggplot(weekly_df, aes(x = week_end_date, y = pct_ed_visits)) +
  # Wave background shading
  geom_rect(
    data    = wave_rects,
    aes(xmin = start_date, xmax = end_date, ymin = -Inf, ymax = Inf, fill = wave),
    inherit.aes = FALSE,
    alpha   = 0.10
  ) +
  # Rolling mean line (smooth trend)
  geom_line(aes(color = "4-Week Rolling Mean"),
            stat   = "smooth",
            method = "loess", span = 0.15,
            linewidth = 1.2, se = FALSE) +
  # Raw weekly values
  geom_line(aes(color = "Weekly % ED Visits"), linewidth = 0.6, alpha = 0.7) +
  # Surge week points
  geom_point(
    data = weekly_df |> filter(surge_flag),
    aes(x = week_end_date, y = pct_ed_visits),
    color = "firebrick", size = 2, alpha = 0.75,
    shape = 17   # Triangle = alert
  ) +
  scale_color_manual(
    values = c("Weekly % ED Visits" = "#1B6CA8", "4-Week Rolling Mean" = "#E05C1B"),
    name   = NULL
  ) +
  scale_fill_viridis_d(option = "D", alpha = 0.15, name = "Wave", guide = "none") +
  scale_x_date(date_labels = "%b\n%Y", date_breaks = "6 months") +
  scale_y_continuous(labels = label_percent(scale = 1), limits = c(0, NA)) +
  labs(
    title    = "COVID-19 ED Visits in Massachusetts Follow Distinct Wave Patterns",
    subtitle = "Red triangles indicate surge weeks (> 2× 4-week rolling mean)",
    x        = "Week Ending Date",
    y        = "% of ED Visits",
    caption  = "Source: CDC NSSP BioSense Platform | Analysis: Tahir Arif, MPH"
  )

ggsave(OUT_TS, plot = fig1, width = 14, height = 6, dpi = 300, bg = "white")
message("Saved: ", OUT_TS)

# ---- Figure 2: Week-over-week change bar chart --------------------------------
# Show WoW % change as colored bars (green=decrease, red=increase)
# Limit to last 2 years for readability

wow_recent <- wow_df |>
  filter(
    !is.na(wow_pct_change),
    week_end_date >= max(week_end_date) - years(2)
  )

fig2 <- ggplot(wow_recent, aes(x = week_end_date, y = wow_pct_change,
                                fill = wow_pct_change > 0)) +
  geom_col(alpha = 0.8, width = 5) +
  geom_hline(yintercept = 0, color = "grey40", linewidth = 0.4) +
  scale_fill_manual(
    values = c("TRUE" = "#D62728", "FALSE" = "#2CA02C"),
    labels = c("TRUE" = "Increase", "FALSE" = "Decrease"),
    name   = "Direction"
  ) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "3 months") +
  scale_y_continuous(labels = label_percent(scale = 1)) +
  labs(
    title    = "COVID-19 ED Visits Show Rapid Week-Over-Week Swings During Surge Periods",
    subtitle = "Week-over-week percentage change in COVID-19-related ED visits (last 2 years)",
    x        = "Week Ending Date",
    y        = "Week-over-Week Change (%)",
    caption  = "Source: CDC NSSP BioSense Platform | Analysis: Tahir Arif, MPH"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(OUT_WOW, plot = fig2, width = 12, height = 5, dpi = 300, bg = "white")
message("Saved: ", OUT_WOW)

# ---- Figure 3: Age-stratified comparison (if data available) -----------------
if (has_age_data) {
  
  # age_raw should have columns: week_end_date, wave, all_ages, college_age_18_24
  age_long <- age_raw |>
    pivot_longer(
      cols      = c(all_ages, college_age_18_24),
      names_to  = "group",
      values_to = "pct_ed_visits"
    ) |>
    mutate(
      group = recode(group,
        "all_ages"           = "All Ages",
        "college_age_18_24"  = "College Age (18–24)"
      ),
      week_end_date = as_date(week_end_date)
    )
  
  fig3 <- ggplot(age_long, aes(x = week_end_date, y = pct_ed_visits,
                                color = group, group = group)) +
    geom_line(linewidth = 0.9, alpha = 0.9) +
    scale_color_manual(
      values = c("All Ages" = "#1B6CA8", "College Age (18–24)" = "#E05C1B"),
      name   = "Population Group"
    ) +
    scale_x_date(date_labels = "%b %Y", date_breaks = "6 months") +
    scale_y_continuous(labels = label_percent(scale = 1), limits = c(0, NA)) +
    labs(
      title    = "College-Age Adults (18–24) Show Distinct COVID-19 ED Visit Patterns",
      subtitle = "Weekly % of ED visits: all-ages vs. college-age population, Massachusetts",
      x        = "Week Ending Date",
      y        = "% of ED Visits",
      caption  = "Source: CDC NSSP BioSense Platform | Analysis: Tahir Arif, MPH"
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggsave(OUT_AGE, plot = fig3, width = 12, height = 5, dpi = 300, bg = "white")
  message("Saved: ", OUT_AGE)
  
} else {
  # Create a placeholder figure noting data unavailability
  fig3 <- ggplot() +
    annotate("text", x = 0.5, y = 0.5,
             label = "Age-stratified data not available\nin this dataset pull.\n\nRe-run 01_data_pull.R with age_group filter\nto enable this analysis.",
             size = 5, color = "grey50", hjust = 0.5, vjust = 0.5) +
    xlim(0, 1) + ylim(0, 1) +
    labs(title    = "College-Age vs. All-Ages Comparison: Data Unavailable",
         caption  = "Analysis: Tahir Arif, MPH") +
    theme(panel.border = element_rect(color = "grey80", fill = NA))
  
  ggsave(OUT_AGE, plot = fig3, width = 8, height = 5, dpi = 300, bg = "white")
  message("Placeholder age figure saved to: ", OUT_AGE)
}

message("\nAll COVID-19 figures saved. Done.")
