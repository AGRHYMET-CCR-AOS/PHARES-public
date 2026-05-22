# Load packages
library(tidyverse)
library(ncdf4)
library(ggspatial)
library(here)

config <- yaml::read_yaml(here("config", "bulletin_config.yml"))

# Functions

safe_read_sf<- function(path) {
  if (is.null(path)) return(NULL)
  if (!file.exists(path)) stop("File not found: ", path, call. = FALSE)
  sf::st_read(path, quiet = TRUE) %>%
    sf::st_make_valid()
}
safe_read_csv<- function(path) {
  if (is.null(path)) return(NULL)
  if (!file.exists(path)) stop("File not found: ", path, call. = FALSE)
  read.csv(path)
}



get_latest_nc_files <- function(dir_path) {
  
  # Lister tous les fichiers .nc
  files <- list.files(
    path = dir_path,
    pattern = "\\.nc$",
    full.names = TRUE
  )
  
  # Extraire les noms
  file_names <- basename(files)
  
  # Fonction interne pour extraire les dates
  extract_date <- function(x) {
    as.Date(
      sub(".*_(\\d{4}-\\d{2}-\\d{2})R1\\.nc", "\\1", x)
    )
  }
  
  # -----------------------------
  # Forecast
  # -----------------------------
  forecast_files <- files[
    grepl("^forecast_\\d{4}-\\d{2}-\\d{2}R1\\.nc$", file_names)
  ]
  
  latest_forecast <- NULL
  
  if (length(forecast_files) > 0) {
    
    forecast_dates <- extract_date(
      basename(forecast_files)
    )
    
    latest_forecast <- forecast_files[
      which.max(forecast_dates)
    ]
  }
  
  # -----------------------------
  # Analysis
  # -----------------------------
  analysis_files <- files[
    grepl("^analysis_\\d{4}-\\d{2}-\\d{2}R1\\.nc$", file_names)
  ]
  
  latest_analysis <- NULL
  
  if (length(analysis_files) > 0) {
    
    analysis_dates <- extract_date(
      basename(analysis_files)
    )
    
    latest_analysis <- analysis_files[
      which.max(analysis_dates)
    ]
  }
  
  # Retour
  list(
    forecast = latest_forecast,
    analysis = latest_analysis
  )
}
# ------------------------------------------------------------
# 2. Helper functions
# ------------------------------------------------------------

transform_to_crs <- function(x, ref) {
  if (!is.null(x) && sf::st_crs(x) != sf::st_crs(ref)) {
    x <- sf::st_transform(x, sf::st_crs(ref))
  }
  x
}


read_nc_dates <- function(nc) {
  time_raw <- ncdf4::ncvar_get(nc, "time")
  time_units <- ncdf4::ncatt_get(nc, "time", "units")$value
  
  origin <- sub("seconds since ", "", time_units)
  
  dates <- as.Date(
    as.POSIXct(time_raw, origin = origin, tz = "UTC")
  )
  
  format(dates, "%Y%m%d")
}


read_nc_variable_wide <- function(nc, var_name, id, dates_fmt) {
  ncdf4::ncvar_get(nc, var_name) |>
    tibble::as_tibble() |>
    dplyr::rename_with(~ dates_fmt) |>
    dplyr::mutate(SUBID = as.character(id)) |>
    dplyr::select(SUBID, dplyr::everything()) |>
    dplyr::ungroup()
}


wide_to_long_discharge <- function(df, category) {
  df |>
    tidyr::pivot_longer(
      cols = -SUBID,
      names_to = "dates",
      values_to = "Q"
    ) |>
    dplyr::mutate(
      dates = as.Date(as.character(dates), format = "%Y%m%d"),
      cat = category
    )
}


read_threshold <- function(nc, var_name, id, new_name) {
  ncdf4::ncvar_get(nc, var_name) |>
    tibble::as_tibble() |>
    dplyr::select(1) |>
    dplyr::rename_with(~ new_name) |>
    dplyr::mutate(SUBID = as.character(id)) |>
    dplyr::select(SUBID, dplyr::everything())
}


format_risk_level <- function(x) {
  factor(
    x,
    levels = c(0, 1, 2, 3),
    labels = RISK_LEVELS
  )
}

# build_fanfar_map <- function(data, country, rivers = NULL, stations = NULL,
#                              variable = "risk",
#                              map_type = c("risk", "discharge"),
#                              facet = FALSE,
#                              facet_var = "leadtime",
#                              nrow = NULL,
#                              annotation_scale_location = c("bl","br","tl","tr"),
#                              north_arrow_size = 1.5,
#                              title_size=12,
#                              legend_title = NULL) {
#   
#   map_type <- match.arg(map_type)
#   
#   if (is.null(legend_title)) {
#     legend_title <- ifelse(map_type == "risk", "Risk level", "Discharge (m³/s)")
#   }
#   
#   p <- ggplot2::ggplot() +
#     ggplot2::geom_sf(
#       data = data,
#       ggplot2::aes(fill = .data[[variable]]),
#       color = NA,
#       linewidth = 0
#     ) +
#     ggplot2::geom_sf(
#       data = country,
#       fill = NA,
#       linewidth = 0.2
#     ) +
#     ggplot2::xlab("Longitude") +
#     ggplot2::ylab("Latitude") +
#     ggplot2::theme_minimal() +
#     ggplot2::theme(
#       strip.text.x.top = element_text(size=title_size, face = "bold"),
#       text = ggplot2::element_text(size = 14),
#       axis.text = ggplot2::element_text(size = 14),
#       axis.title.x = ggplot2::element_text(size = 15, face = "bold"),
#       axis.title.y = ggplot2::element_text(size = 15, face = "bold"),
#       plot.title = ggplot2::element_text(face = "bold", size = 20),
#       legend.position = "bottom",
#       legend.key.width = grid::unit(2, "cm"),
#       panel.background = ggplot2::element_rect(fill = "white"),
#       panel.grid.major = ggplot2::element_line(color = "grey90", linewidth = 0.2),
#       plot.margin = ggplot2::margin(2.5, 2.5, 2.5, 2.5)
#     ) +
#     ggspatial::annotation_north_arrow(
#       location = "tr",
#       which_north = "true",
#       style = ggspatial::north_arrow_fancy_orienteering,
#       height = grid::unit(north_arrow_size, "cm"),
#       width = grid::unit(north_arrow_size, "cm"),
#       pad_x = grid::unit(0.1, "cm"),
#       pad_y = grid::unit(0.1, "cm")
#     ) +
#     ggspatial::annotation_scale(
#       location = annotation_scale_location,
#       width_hint = 0.3
#     )
#   
#   if (map_type == "risk") {
#     p <- p +
#       ggplot2::scale_fill_manual(
#         name = legend_title,
#         values = RISK_COLORS,
#         na.value = "white",
#         drop = FALSE
#       )
#   }
#   
#   if (map_type == "discharge") {
#     p <- p +
#       ggplot2::scale_fill_gradient(low = RColorBrewer::brewer.pal(name="Blues",9)[1],
#                                    high =RColorBrewer::brewer.pal(name="Blues",9)[4] )
# 
#   }
#   
#   if (facet) {
#     p <- p +
#       ggplot2::facet_wrap(
#         stats::as.formula(paste("~", facet_var)),
#         nrow = nrow
#       )
#   }
#   
#   if (!is.null(rivers)) {
#     p <- p +
#       ggplot2::geom_sf(
#         data = rivers,
#         color = "blue",
#         linewidth = 0.1,
#         inherit.aes = FALSE
#       )
#   }
#   
#   if (!is.null(stations)) {
#     p <- p +
#       ggplot2::geom_sf(
#         data = stations,
#         color = "black",
#         size = 1,
#         inherit.aes = FALSE
#       )
#   }
#   
#   p
# }


build_fanfar_map <- function(data, country, rivers = NULL, stations = NULL,
                             variable = "risk",
                             map_type = c("risk", "discharge"),
                             facet = FALSE,
                             facet_var = "leadtime",
                             nrow = NULL,
                             annotation_scale_location = c("bl","br","tl","tr"),
                             north_arrow_size = 1.5,
                             title = "",
                             title_size = 12,
                             legend_title = NULL,
                             source_text = NULL,
                             source_x = -Inf,
                             source_y = -Inf,
                             source_hjust = -0.1,
                             source_vjust = -0.5,
                             source_size = 3.5) {
  
  map_type <- match.arg(map_type)
  annotation_scale_location <- match.arg(annotation_scale_location)
  
  if (is.null(legend_title)) {
    legend_title <- ifelse(map_type == "risk", "Risk level", "Discharge (m³/s)")
  }
  
  p <- ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = data,
      ggplot2::aes(fill = .data[[variable]]),
      color = NA,
      linewidth = 0
    ) +
    ggplot2::geom_sf(
      data = country,
      fill = NA,
      linewidth = 0.2
    ) +
    ggplot2::labs(
      title = title
    )+
    ggplot2::xlab("Longitude") +
    ggplot2::ylab("Latitude") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      strip.text.x.top = ggplot2::element_text(size = title_size, face = "bold"),
      text = ggplot2::element_text(size = 14),
      axis.text = ggplot2::element_text(size = 14),
      axis.title.x = ggplot2::element_text(size = 15, face = "bold"),
      axis.title.y = ggplot2::element_text(size = 15, face = "bold"),
      plot.title = ggplot2::element_text(face = "bold",
                                         size = title_size, hjust = 0.5),
      legend.position = "bottom",
      legend.key.width = grid::unit(2, "cm"),
      panel.background = ggplot2::element_rect(fill = "white"),
      panel.grid.major = ggplot2::element_line(color = "grey90", linewidth = 0.2),
      plot.margin = ggplot2::margin(2.5, 2.5, 2.5, 2.5)
    ) +
    ggspatial::annotation_north_arrow(
      location = "tr",
      which_north = "true",
      style = ggspatial::north_arrow_fancy_orienteering,
      height = grid::unit(north_arrow_size, "cm"),
      width = grid::unit(north_arrow_size, "cm"),
      pad_x = grid::unit(0.1, "cm"),
      pad_y = grid::unit(0.1, "cm")
    ) +
    ggspatial::annotation_scale(
      location = annotation_scale_location,
      width_hint = 0.3
    )
  
  if (map_type == "risk") {
    p <- p +
      ggplot2::scale_fill_manual(
        name = legend_title,
        values = RISK_COLORS,
        na.value = "white",
        drop = TRUE
      )
  }
  
  if (map_type == "discharge") {
    p <- p +
      ggplot2::scale_fill_gradient(
        name = legend_title,
        low = RColorBrewer::brewer.pal(name = "Blues", 9)[1],
        high = RColorBrewer::brewer.pal(name = "Blues", 9)[4]
      )
  }
  
  if (!is.null(source_text)) {
    p <- p +
      ggplot2::annotate(
        "text",
        x = source_x,
        y = source_y,
        label = source_text,
        hjust = source_hjust,
        vjust = source_vjust,
        size = source_size
      )
  }
  
  if (facet) {
    p <- p +
      ggplot2::facet_wrap(
        stats::as.formula(paste("~", facet_var)),
        nrow = nrow
      )
  }
  
  if (!is.null(rivers)) {
    p <- p +
      ggplot2::geom_sf(
        data = rivers,
        color = "blue",
        linewidth = 0.1,
        inherit.aes = FALSE
      )
  }
  
  if (!is.null(stations)) {
    p <- p +
      ggplot2::geom_sf(
        data = stations,
        color = "black",
        size = 1,
        inherit.aes = FALSE
      )
  }
  
  p
}


number_to_french <- function(x) {
  
  numbers <- c(
    "0" = "zéro",
    "1" = "un",
    "2" = "deux",
    "3" = "trois",
    "4" = "quatre",
    "5" = "cinq",
    "6" = "six",
    "7" = "sept",
    "8" = "huit",
    "9" = "neuf",
    "10" = "dix"
  )
  
  unname(numbers[as.character(x)])
}
# ============================================================
# Compute dynamic map dimensions from spatial extent
# ============================================================

source(here::here("R", "02_process_fanfar.R"))
source(here::here("R", "04_classify_risk.R"))
source(here::here("R", "05_prepare_bulletin_regions.R"))
source(here::here("R", "06_generate_key_messages.R"))
source(here::here("R", "07_generate_regional_comments.R"))
source(here::here("R", "08_analyze_station_flows.R"))
source(here::here("R", "09_generate_station_comments.R"))