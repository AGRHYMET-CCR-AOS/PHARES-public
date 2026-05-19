# ============================================================
# Classify FANFAR hydrological risk
# ============================================================
#R/04_classify_risk.R
risk_to_numeric <- function(risk) {
  dplyr::case_when(
    risk %in% c("Normal", "normal") ~ 0,
    risk %in% c("Low risk", "low", "Yellow") ~ 1,
    risk %in% c("Moderate risk", "moderate", "Orange") ~ 2,
    risk %in% c("High risk", "high", "Red") ~ 3,
    TRUE ~ NA_real_
  )
}

risk_to_label_fr <- function(risk_level) {
  dplyr::case_when(
    risk_level == 0 ~ "normale",
    risk_level == 1 ~ "faible",
    risk_level == 2 ~ "modéré",
    risk_level >= 3 ~ "élevé",
    TRUE ~ "non déterminé"
  )
}

classify_risk <- function(max_frcst, artifact_subids = NULL) {
  
  max_frcst |>
    dplyr::mutate(
      risk_level = risk_to_numeric(.data$risk),
      is_artifact = dplyr::if_else(
        .data$SUBID %in% artifact_subids,
        TRUE,
        FALSE,
        missing = FALSE
      ),
      risk_label_fr = risk_to_label_fr(.data$risk_level)
    )
}