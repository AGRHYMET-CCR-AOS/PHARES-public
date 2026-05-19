# ============================================================
# Generate key messages
# ============================================================
#R/06_generate_key_messages.R
generate_key_messages <- function(risk_data) {
  
  clean_data <- risk_data |>
    dplyr::filter(!.data$is_artifact)
  
  risk_zones <- clean_data |>
    dplyr::filter(.data$risk_level >= 1)
  
  if (nrow(risk_zones) == 0) {
    return(c(
      "La situation hydrologique devrait rester globalement normale sur l’ensemble du territoire suivi.",
      "Aucun risque de crue généralisée n’est attendu au cours des dix prochains jours.",
      "Le suivi hydrologique régulier reste recommandé."
    ))
  }
  
  max_risk <- max(risk_zones$risk_level, na.rm = TRUE)
  max_label <- risk_to_label_fr(max_risk)
  
  regions <- risk_zones |>
    dplyr::distinct(.data$bulletin_region) |>
    dplyr::pull(.data$bulletin_region) |>
    paste(collapse = ", ")
  
  c(
    "La situation hydrologique devrait rester globalement normale sur la majeure partie du territoire suivi.",
    "Aucun risque de crue généralisée n’est attendu au cours des dix prochains jours.",
    paste0(
      "Des signaux localisés de risque ", max_label,
      " sont attendus dans les zones suivantes : ", regions, "."
    ),
    dplyr::case_when(
      max_risk == 1 ~ "Une surveillance hydrologique régulière est recommandée dans les zones concernées.",
      max_risk == 2 ~ "Une vigilance est recommandée dans les zones concernées.",
      max_risk >= 3 ~ "Une vigilance renforcée est recommandée, avec un suivi rapproché de l’évolution des niveaux d’eau.",
      TRUE ~ "Le suivi hydrologique régulier reste recommandé."
    )
  )
}