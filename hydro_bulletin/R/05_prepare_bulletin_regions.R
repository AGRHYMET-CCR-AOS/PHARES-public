# ============================================================
# Prepare bulletin regions
# ============================================================
#R/05_prepare_bulletin_regions.R
prepare_bulletin_regions <- function(sf_data, region_col = NULL) {
  
  if (!is.null(region_col) && region_col %in% names(sf_data)) {
    return(
      sf_data |>
        dplyr::mutate(bulletin_region = as.character(.data[[region_col]]))
    )
  }
  
  centroids <- sf::st_centroid(sf::st_geometry(sf_data))
  coords <- sf::st_coordinates(centroids)
  
  x_mid <- stats::median(coords[, 1], na.rm = TRUE)
  y_mid <- stats::median(coords[, 2], na.rm = TRUE)
  
  sf_data |>
    dplyr::mutate(
      centroid_x = coords[, 1],
      centroid_y = coords[, 2],
      bulletin_region = dplyr::case_when(
        centroid_x >= x_mid & centroid_y >= y_mid ~ "Nord-Est",
        centroid_x <  x_mid & centroid_y >= y_mid ~ "Nord-Ouest",
        centroid_x >= x_mid & centroid_y <  y_mid ~ "Sud-Est",
        centroid_x <  x_mid & centroid_y <  y_mid ~ "Sud-Ouest",
        TRUE ~ "Zone non déterminée"
      )
    )
}