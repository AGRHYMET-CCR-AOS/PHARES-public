# ============================================================
# Generate dynamic regional hydrological comments
# ============================================================
#R/07_generate_regional_comments.R
generate_regional_comments <- function(risk_data) {
  
  clean_data <- risk_data |>
    dplyr::filter(!.data$is_artifact)
  
  regional_summary <- clean_data |>
    sf::st_drop_geometry() |>
    dplyr::group_by(.data$bulletin_region) |>
    dplyr::summarise(
      max_risk = max(.data$risk_level, na.rm = TRUE),
      n_subbasins = dplyr::n(),
      n_risk_subbasins = sum(.data$risk_level >= 1, na.rm = TRUE),
      .groups = "drop"
    )
  
  if (all(regional_summary$max_risk == 0, na.rm = TRUE)) {
    return(
      "La situation hydrologique est globalement normale sur l’ensemble du territoire suivi, sans signal significatif de crue au cours des dix prochains jours."
    )
  }
  
  comments <- purrr::pmap_chr(
    regional_summary,
    function(bulletin_region, max_risk, n_subbasins, n_risk_subbasins) {
      
      if (max_risk == 0) {
        paste0(
          "Dans la partie ", bulletin_region,
          ", la situation hydrologique demeure globalement normale."
        )
      } else if (max_risk == 1) {
        paste0(
          "Dans la partie ", bulletin_region,
          ", des signaux faibles et localisés sont attendus, sans indication de crue significative."
        )
      } else if (max_risk == 2) {
        paste0(
          "Dans la partie ", bulletin_region,
          ", un risque modéré est attendu localement, pouvant nécessiter une vigilance hydrologique."
        )
      } else {
        paste0(
          "Dans la partie ", bulletin_region,
          ", des signaux de risque élevé sont attendus localement et nécessitent un suivi rapproché."
        )
      }
    }
  )
  
  paste(comments, collapse = " ")
}