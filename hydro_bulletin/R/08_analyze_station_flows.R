# ============================================================
# Analyze station flows and thresholds
# ============================================================
#R/08_analyze_station_flows.R
analyze_station_flows <- function(df_plot) {
  
  df_plot |>
    dplyr::arrange(.data$SUBID, .data$dates) |>
    dplyr::group_by(.data$SUBID) |>
    dplyr::summarise(
      first_q = dplyr::first(.data$Q),
      last_q = dplyr::last(.data$Q),
      max_q = max(.data$Q, na.rm = TRUE),
      max_q_date = .data$dates[which.max(.data$Q)],
      Q1 = dplyr::first(.data$Q1),
      Q2 = dplyr::first(.data$Q2),
      Q3 = dplyr::first(.data$Q3),
      trend = dplyr::case_when(
        last_q > first_q ~ "increase",
        last_q < first_q ~ "decrease",
        TRUE ~ "stable"
      ),
      alert_level = dplyr::case_when(
        max_q >= Q3 ~ "red",
        max_q >= Q2 ~ "orange",
        max_q >= Q1 ~ "yellow",
        TRUE ~ "normal"
      ),
      .groups = "drop"
    )
}