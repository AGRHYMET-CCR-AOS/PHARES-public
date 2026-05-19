# ============================================================
# Generate station flow comments
# ============================================================
#R/09_generate_station_comments.R
generate_station_comments <- function(
    station_summary,
    station_name_col = NULL
) {
  
  # ----------------------------------------------------------
  # Station labels
  # ----------------------------------------------------------
  
  if (!is.null(station_name_col) &&
      station_name_col %in% names(station_summary)) {
    
    station_summary <- station_summary |>
      dplyr::mutate(
        station_label = as.character(.data[[station_name_col]])
      )
    
  } else {
    
    station_summary <- station_summary |>
      dplyr::mutate(
        station_label = as.character(.data$SUBID)
      )
  }
  
  # ----------------------------------------------------------
  # Split by alert level
  # ----------------------------------------------------------
  
  normal_stations <- station_summary |>
    dplyr::filter(.data$alert_level == "normal")
  
  yellow_stations <- station_summary |>
    dplyr::filter(.data$alert_level == "yellow")
  
  orange_stations <- station_summary |>
    dplyr::filter(.data$alert_level == "orange")
  
  red_stations <- station_summary |>
    dplyr::filter(.data$alert_level == "red")
  
  comments <- c()
  
  # ----------------------------------------------------------
  # Normal flow conditions
  # ----------------------------------------------------------
  
  if (nrow(normal_stations) > 0) {
    
    comments <- c(
      comments,
      "Les écoulements au niveau de la majorité des principales stations hydrométriques demeurent globalement normaux."
    )
  }
  
  # ----------------------------------------------------------
  # Yellow alert
  # ----------------------------------------------------------
  
  if (nrow(yellow_stations) > 0) {
    
    stations_txt <- paste(
      yellow_stations$station_label,
      collapse = ", "
    )
    
    comments <- c(
      comments,
      paste0(
        "Une légère hausse des débits est attendue au niveau des stations ",
        stations_txt,
        ", avec des niveaux pouvant atteindre le seuil de surveillance."
      )
    )
  }
  
  # ----------------------------------------------------------
  # Orange alert
  # ----------------------------------------------------------
  
  if (nrow(orange_stations) > 0) {
    
    stations_txt <- paste(
      orange_stations$station_label,
      collapse = ", "
    )
    
    comments <- c(
      comments,
      paste0(
        "Une augmentation des débits est prévue au niveau des stations ",
        stations_txt,
        ", avec des niveaux pouvant atteindre le seuil d’alerte orange dans les prochains jours."
      )
    )
  }
  
  # ----------------------------------------------------------
  # Red alert
  # ----------------------------------------------------------
  
  if (nrow(red_stations) > 0) {
    
    stations_txt <- paste(
      red_stations$station_label,
      collapse = ", "
    )
    
    comments <- c(
      comments,
      paste0(
        "Des débits élevés sont attendus au niveau des stations ",
        stations_txt,
        ", avec un risque d’atteinte du seuil d’alerte rouge."
      )
    )
  }
  
  # ----------------------------------------------------------
  # Return final text
  # ----------------------------------------------------------
  
  paste(comments, collapse = " ")
}
