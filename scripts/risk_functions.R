# risk_functions.R - Ship-strike risk calculation functions for Rice's whale analysis
# NRDC Contract: Further analysis of Rice's whale ship-strike risk

# Conn/Silber 2013 speed-based risk factors ----
RISK_FACTORS <- list(
  # speed bin definitions and associated risk factors
  bins = c(0, 10, 12, 15, Inf),
  factors = c(1.0, 1.6, 5.4, 10.8),
  labels = c("<=10 kt", ">10-12 kt", ">12-15 kt", ">15 kt")
)

#' Assign speed bin based on average speed
#' @param speed Numeric vector of speeds in knots
#' @return Factor of speed bins
assign_speed_bin <- function(speed) {
  cut(
    speed,
    breaks = RISK_FACTORS$bins,
    labels = RISK_FACTORS$labels,
    include.lowest = TRUE,
    right = TRUE
  )
}

#' Get risk factor for a speed bin
#' @param speed_bin Factor or character of speed bin labels
#' @return Numeric risk factor
get_risk_factor <- function(speed_bin) {
  idx <- match(as.character(speed_bin), RISK_FACTORS$labels)
  RISK_FACTORS$factors[idx]
}

#' Calculate base risk (whale density x traffic, no speed weighting)
#' @param whale_density Numeric whale density value
#' @param traffic_metric Numeric traffic metric (distance, hours, or count)
#' @return Numeric base risk value
calc_base_risk <- function(whale_density, traffic_metric) {
  whale_density * traffic_metric
}

#' Calculate speed-weighted risk using Conn/Silber 2013 factors
#' @param whale_density Numeric whale density value
#' @param traffic_metric Numeric traffic metric
#' @param speed_bin Speed bin (factor or character)
#' @return Numeric speed-weighted risk value
calc_speed_weighted_risk <- function(whale_density, traffic_metric, speed_bin) {
  risk_factor <- get_risk_factor(speed_bin)
  whale_density * traffic_metric * risk_factor
}

#' Calculate risk reduction from slowdown scenario
#' @param baseline_risk Numeric baseline risk value
#' @param slowdown_risk Numeric risk after slowdown (all speeds at 10kt)
#' @return List with absolute reduction and percentage reduction
calc_risk_reduction <- function(baseline_risk, slowdown_risk) {
  absolute <- baseline_risk - slowdown_risk
  pct <- ifelse(baseline_risk > 0, (absolute / baseline_risk) * 100, 0)
  list(
    absolute   = absolute,
    percentage = pct
  )
}

#' Summarize risk by geographic area
#' @param risk_data Data frame with columns: area_id, whale_density, traffic, speed_bin
#' @param area_col Name of area column
#' @param verbose Show debug messages
#' @return Data frame with risk summaries by area
summarize_risk_by_area <- function(risk_data, area_col = "area_id", verbose = interactive()) {

  if (verbose) message("[summarize_risk_by_area] Calculating risk for column: ", area_col)

  # add risk columns
  risk_data <- risk_data |>
    dplyr::mutate(
      base_risk = calc_base_risk(whale_density, traffic),
      speed_weighted_risk = calc_speed_weighted_risk(whale_density, traffic, speed_bin),
      # slowdown scenario: all at 10kt (factor = 1.0)
      slowdown_risk = calc_base_risk(whale_density, traffic)
    )

  # summarize by area
  risk_data |>
    dplyr::group_by(dplyr::across(dplyr::all_of(area_col))) |>
    dplyr::summarize(
      total_base_risk           = sum(base_risk, na.rm = TRUE),
      total_speed_weighted_risk = sum(speed_weighted_risk, na.rm = TRUE),
      total_slowdown_risk       = sum(slowdown_risk, na.rm = TRUE),
      risk_reduction_abs        = total_speed_weighted_risk - total_slowdown_risk,
      risk_reduction_pct        = ifelse(
        total_speed_weighted_risk > 0,
        (risk_reduction_abs / total_speed_weighted_risk) * 100,
        0
      ),
      .groups = "drop"
    )
}

#' Summarize risk by company
#' @param risk_data Data frame with columns: company, whale_density, traffic, speed_bin
#' @param verbose Show debug messages
#' @return Data frame with risk summaries by company
summarize_risk_by_company <- function(risk_data, verbose = interactive()) {

  if (verbose) message("[summarize_risk_by_company] Calculating risk by company")

  summarize_risk_by_area(risk_data, area_col = "company", verbose = verbose)
}

#' Calculate contribution of each area to total risk
#' @param area_risk Data frame from summarize_risk_by_area
#' @param risk_col Which risk column to use for percentages
#' @return Data frame with contribution percentages added
calc_area_contribution <- function(area_risk, risk_col = "total_speed_weighted_risk") {

  total <- sum(area_risk[[risk_col]], na.rm = TRUE)

  area_risk |>
    dplyr::mutate(
      contribution_pct = ifelse(
        total > 0,
        (.data[[risk_col]] / total) * 100,
        0
      )
    )
}

#' Get speed bin SQL expression for DuckDB
#' @return SQL CASE expression string
get_speed_bin_sql <- function() {
  "CASE
    WHEN avg_speed_knots <= 10 THEN '<=10 kt'
    WHEN avg_speed_knots <= 12 THEN '>10-12 kt'
    WHEN avg_speed_knots <= 15 THEN '>12-15 kt'
    ELSE '>15 kt'
  END"
}

#' Get risk factor SQL expression for DuckDB
#' @return SQL CASE expression string
get_risk_factor_sql <- function() {
  "CASE
    WHEN avg_speed_knots <= 10 THEN 1.0
    WHEN avg_speed_knots <= 12 THEN 1.6
    WHEN avg_speed_knots <= 15 THEN 5.4
    ELSE 10.8
  END"
}
