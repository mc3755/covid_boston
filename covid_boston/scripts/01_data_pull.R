# =============================================================================
# Title:       01_data_pull.R
# Author:      Tahir Arif, MPH
# Date:        March 2026
# Description: Query CDC NSSP BioSense ED visit data for COVID-19 in
#              Massachusetts. Uses the Socrata REST API (no key required
#              for queries ≤1,000 rows; optional Socrata token for larger
#              pulls). Saves raw data to data/raw_covid_nssp.csv.
# =============================================================================

# ---- Libraries ---------------------------------------------------------------
library(httr)        # HTTP GET requests
library(jsonlite)    # Parse JSON response
library(readr)       # write_csv
library(dplyr)       # Data manipulation
library(here)        # Relative paths

# ---- Configuration -----------------------------------------------------------

# Primary NSSP ED visit endpoint
NSSP_ENDPOINT <- "https://data.cdc.gov/resource/7xva-uux8.json"

# Supplemental: NSSP Trajectories endpoint (state-level surge classification)
NSSP_TRAJ_ENDPOINT <- "https://data.cdc.gov/resource/rdmq-nq56.json"

# Query parameters for COVID-19 / Massachusetts
QUERY_PARAMS <- list(
  geography = "Massachusetts",
  pathogen  = "COVID-19",
  "$limit"  = 5000,
  "$order"  = "week_end_date DESC"
)

# Trajectory query parameters
TRAJ_PARAMS <- list(
  geography = "Massachusetts",
  pathogen  = "COVID-19",
  "$limit"  = 2000,
  "$order"  = "week_end_date DESC"
)

# Output paths
RAW_OUTPUT  <- here("data", "raw_covid_nssp.csv")
TRAJ_OUTPUT <- here("data", "raw_covid_trajectories.csv")

# ---- Helper: Safe Socrata API call -------------------------------------------

#' Pull data from a Socrata REST API endpoint
#'
#' @param endpoint  Character. API URL.
#' @param params    Named list of Socrata query parameters.
#' @param token     Character. Optional Socrata app token (improves rate limits).
#' @return          Data frame.
pull_socrata <- function(endpoint, params, token = Sys.getenv("SOCRATA_TOKEN")) {
  message("Querying: ", endpoint)
  
  headers <- add_headers(
    "Accept"      = "application/json",
    "X-App-Token" = token   # Empty string if SOCRATA_TOKEN not set — still works
  )
  
  response <- GET(url = endpoint, query = params, headers, timeout(60))
  
  if (http_error(response)) {
    stop(
      "HTTP error ", status_code(response), "\n",
      "URL: ", response$url, "\n",
      "Body: ", content(response, as = "text", encoding = "UTF-8")
    )
  }
  
  raw_text <- content(response, as = "text", encoding = "UTF-8")
  df       <- fromJSON(raw_text, flatten = TRUE)
  
  message("  Retrieved ", nrow(df), " records, ", ncol(df), " columns")
  return(df)
}

# ---- Pull main COVID-19 ED visit data ----------------------------------------
message("=== Pulling COVID-19 ED visit data ===")
covid_raw <- pull_socrata(NSSP_ENDPOINT, QUERY_PARAMS)

# Validate: confirm we got real data
stopifnot("API returned 0 rows — check filter parameters" = nrow(covid_raw) > 0)

# Show what we got
message("\nFields in ED visit data:")
print(names(covid_raw))

message("\nSample of raw data (first 3 rows):")
print(head(covid_raw, 3))

# ---- Pull trajectory/trend classification data --------------------------------
message("\n=== Pulling NSSP trajectory/trend data ===")
traj_raw <- tryCatch(
  pull_socrata(NSSP_TRAJ_ENDPOINT, TRAJ_PARAMS),
  error = function(e) {
    warning("Trajectory pull failed: ", e$message,
            "\n  Continuing without trajectory data.")
    return(NULL)
  }
)

# ---- Save outputs ------------------------------------------------------------
write_csv(covid_raw, RAW_OUTPUT)
message("\nCOVID-19 ED visit data saved to: ", RAW_OUTPUT)

if (!is.null(traj_raw) && nrow(traj_raw) > 0) {
  write_csv(traj_raw, TRAJ_OUTPUT)
  message("Trajectory data saved to: ", TRAJ_OUTPUT)
} else {
  # Create empty placeholder so downstream scripts don't error
  write_csv(tibble(note = "Trajectory data unavailable"), TRAJ_OUTPUT)
  message("Trajectory data unavailable — placeholder saved to: ", TRAJ_OUTPUT)
}

message("\nDone. Proceed with 02_clean.R")
