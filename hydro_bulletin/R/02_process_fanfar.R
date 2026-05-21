# ============================================================
# 02_process_fanfar.R
# FANFAR hydrological forecast processing
# ============================================================

# ------------------------------------------------------------
# 1. Configuration
# ------------------------------------------------------------
time_ref <- format(Sys.Date(),"%Y%m%d")
dir_path <- config$Data_Processing$nc_files_directory_path
COUNTRY_CODE <- config$Data_Processing$country_code
OUTPUT_DIR <- file.path(config$figures$output_path,time_ref)

if (!dir.exists(OUTPUT_DIR)) {
  dir.create(OUTPUT_DIR, recursive = TRUE)
}


lates_nc <- get_latest_nc_files(file.path(config$project$path,dir_path))

# Résultats
FORECAST_FILE <- here::here(lates_nc$forecast)
ANALYSIS_FILE <- here::here(lates_nc$analysis)



RISK_LEVELS <- c("Normal", "Low risk", "Moderate risk", "High risk")

RISK_COLORS <- c(
  "Normal" = "#C7E9C0",
  "Low risk" = "yellow",
  "Moderate risk" = "orange",
  "High risk" = "red"
)

# ------------------------------------------------------------
# 2. Spatial data processing
# ------------------------------------------------------------

subbasins <- safe_read_sf(config$Data_Processing$subbassins_shp)
countries <- safe_read_sf(config$Data_Processing$countries_shp)
rivers <- safe_read_sf(config$Data_Processing$rivers_shp)
stations <- safe_read_sf(config$Data_Processing$stations_shp)

countries <- transform_to_crs(countries, subbasins)
rivers <- transform_to_crs(rivers, subbasins)
stations <- transform_to_crs(stations, subbasins)

if (!is.null(COUNTRY_CODE)) {
  extracted_country <- countries |>
    dplyr::filter(.data$GMI_CNTRY == COUNTRY_CODE)
  
  if (nrow(extracted_country) == 0) {
    stop("No country found with GMI_CNTRY == ", COUNTRY_CODE)
  }
} else {
  extracted_country <- countries
}

sf_basins <- suppressWarnings(
  sf::st_intersection(subbasins, extracted_country)
) |>
  dplyr::mutate(SUBID = as.character(SUBID))

sub_ids <- unique(sf_basins$SUBID)

if (!is.null(rivers)) {
  rivers <- suppressWarnings(
    sf::st_intersection(rivers, extracted_country)
  )
}


# ------------------------------------------------------------
# 3. NetCDF processing
# ------------------------------------------------------------

frct <- ncdf4::nc_open(FORECAST_FILE)
analysis <- ncdf4::nc_open(ANALYSIS_FILE)
print(frct)

# ---- Analysis data ----

analysis_id <- ncdf4::ncvar_get(analysis, "id")
analysis_dates <- read_nc_dates(analysis)

analysis_cout_wide <- read_nc_variable_wide(
  nc = analysis,
  var_name = "cout",
  id = analysis_id,
  dates_fmt = analysis_dates
)

analysis_cout_long <- analysis_cout_wide |>
  wide_to_long_discharge(category = "Historic")


# ---- Forecast data ----

forecast_id <- ncdf4::ncvar_get(frct, "id")
forecast_dates <- read_nc_dates(frct)

forecast_cout_wide <- read_nc_variable_wide(
  nc = frct,
  var_name = "cout",
  id = forecast_id,
  dates_fmt = forecast_dates
) %>% 
  dplyr::select(all_of(c("SUBID",forecast_dates[1:config$bulletin$forecast_horizon_days])))

forecast_cout_long_ <- forecast_cout_wide |>
  wide_to_long_discharge(category = "Forecast") 

forecast_cout_long <- forecast_cout_long_%>% 
  dplyr::filter(SUBID %in% sub_ids)


# ---- Risk forecast ----

cout_al <- ncdf4::ncvar_get(frct, "cout_AL") |>
  tibble::as_tibble()

names(cout_al) <- forecast_dates
time_range <- forecast_dates[1:config$bulletin$forecast_horizon_days]
cout_al_wide <- cout_al |>
  dplyr::mutate(SUBID = as.character(forecast_id)) |>
  dplyr::filter(SUBID %in% sub_ids) |>
  dplyr::select(all_of(c("SUBID",time_range))) |>
  dplyr::rowwise() |>
  dplyr::mutate(max10d = max(dplyr::c_across(-SUBID), na.rm = TRUE)) |>
  dplyr::ungroup()


cout_al_long <- cout_al_wide |>
  tidyr::pivot_longer(
    cols = -SUBID,
    names_to = "leadtime",
    values_to = "risk"
  ) |>
  dplyr::mutate(risk = format_risk_level(risk))

cout_al_long_shp <- sf_basins |>
  dplyr::select(SUBID) |>
  dplyr::left_join(cout_al_long, by = "SUBID")


# ---- Thresholds ----

thresholds <- read_threshold(frct, "cout_AL_thresholds_1", forecast_id, "Q1") |>
  dplyr::left_join(
    read_threshold(frct, "cout_AL_thresholds_2", forecast_id, "Q2"),
    by = "SUBID"
  ) |>
  dplyr::left_join(
    read_threshold(frct, "cout_AL_thresholds_3", forecast_id, "Q3"),
    by = "SUBID"
  )


# ------------------------------------------------------------
# 4. Risk maps
# ------------------------------------------------------------
min_date <- as.Date(min(time_range),format = "%Y%m%d")
max_date <- as.Date(max(time_range),format = "%Y%m%d")
map_label1 <- paste0("Maximum discharge from ",min_date , " to ",max_date)

forecast_cout_long_shp <- sf_basins |>
  dplyr::select(SUBID) |>
  dplyr::left_join(forecast_cout_long, by = "SUBID")


# map_forecast_discharge <- build_fanfar_map(
#   data = forecast_cout_long_shp,
#   country = extracted_country,
#   rivers = rivers,
#   stations = stations,
#   variable = "Q",
#   map_type = "discharge",
#   facet = TRUE,
#   facet_var = "dates",
#   nrow = 2,
#   north_arrow_size = 1,
#   annotation_scale_location ="bl",
#   legend_title = "Discharge (m³/s)"
# )


map_forecast_discharge <- build_fanfar_map(
  data = forecast_cout_long_shp,
  country = extracted_country,
  rivers = rivers,
  stations = stations,
  variable = "Q",
  map_type = "discharge",
  facet = TRUE,
  facet_var = "dates",
  nrow = 2,
  north_arrow_size = 1,
  annotation_scale_location ="bl",
  legend_title = "Discharge (m³/s)"
)
ggplot2::ggsave(
  filename = file.path(OUTPUT_DIR, paste0("map_discharge_",time_ref,"_forecast.png")),
  plot = map_forecast_discharge,
  width = config$figures$max_risk_map$width,
  height = config$figures$max_risk_map$height,
  dpi = 300,
  bg = "white"
)


forecast_cout_long_shp_max <- forecast_cout_long_shp %>% 
  group_by(SUBID) %>% 
  summarize(Q=max(Q)) %>% 
  mutate(panel_name=map_label1)
map_max_discharge <- build_fanfar_map(
  data = forecast_cout_long_shp_max,
  country = extracted_country,
  rivers = rivers,
  stations = stations,
  variable = "Q",
  map_type = "discharge",
  facet = FALSE,
  facet_var = "panel_name",
  title = map_label1,
  nrow = 1,
  north_arrow_size = 1,
  annotation_scale_location ="bl",
  legend_title = "Q (m³/s)"
)
ggplot2::ggsave(
  filename = file.path(OUTPUT_DIR, paste0("map_discharge_",time_ref,"_forecast.png")),
  plot = map_forecast_discharge,
  width = config$figures$max_risk_map$width,
  height = config$figures$max_risk_map$height,
  dpi = 300,
  bg = "white"
)


map_label <- paste0("Maximum forecast flood hazard severity from ",min_date , " to ",max_date)
max_frcst <- cout_al_long_shp |>
  dplyr::filter(leadtime == "max10d") %>% 
  mutate(panel_name=map_label)

min(max_frcst$leadtime)
base_map_max_frcst <- build_fanfar_map(
  data = max_frcst,
  country = extracted_country,
  rivers = rivers,
  stations = stations,
  variable = "risk",
  map_type = "risk",
  facet_var ="panel_name" ,
  title_size =14,
  title = map_label,
  facet = FALSE,
  annotation_scale_location ="bl",
  north_arrow_size = 2
)

ggplot2::ggsave(
  filename = file.path(OUTPUT_DIR, paste0("map_max_risk_10days_",time_ref,"_forecast.png")),
  plot = base_map_max_frcst,
  width = config$figures$max_risk_map$width,
  height = config$figures$max_risk_map$height,
  dpi = 300,
  bg = "white"
)

d10_frcst <- cout_al_long_shp |>
  dplyr::filter(leadtime != "max10d")

base_map_d10_frcst <- build_fanfar_map(
  data = d10_frcst,
  country = extracted_country,
  rivers = rivers,
  stations = stations,
  variable = "risk",
  map_type = "risk",
  facet = TRUE,
  annotation_scale_location = "br",
  nrow = config$figures$daily_risk_map$facet_nrow,
  north_arrow_size = 1
)

# ---- Daily risk forecast maps ----

ggplot2::ggsave(
  filename = file.path(OUTPUT_DIR, paste0("map_daily_risk_",time_ref,"_forecast.png")),
  plot = base_map_d10_frcst,
  width = config$figures$daily_risk_map$width,
  height =config$figures$daily_risk_map$height,
  dpi = 300,
  bg = "white"
)


# ------------------------------------------------------------
# 5. Station processing
# ------------------------------------------------------------

stations_df <- safe_read_csv(config$Data_Processing$stations_csv)

# stations_df <- stations_df |>
#   dplyr::rename_with(toupper) |>
#   dplyr::rename(ID = SUBID) |>
#   dplyr::select(ID, LON, LAT)
# 
# stations_sf <- sf::st_as_sf(
#   stations_df,
#   coords = c("LON", "LAT"),
#   crs = 4326
# )
# 
# stations_sf <- transform_to_crs(stations_sf, subbasins)
# 
# stations_with_subbasin <- sf::st_join(
#   stations_sf,
#   subbasins,
#   join = sf::st_within
# )

#station_subids <- unique(as.character(stations_with_subbasin$ID))
station_subids <- unique(stations_df$SUBID)


# ------------------------------------------------------------
# 7. Hydrograph data preparation
# ------------------------------------------------------------

cout_long <- dplyr::bind_rows(
  analysis_cout_long,
  forecast_cout_long_
) |>
  dplyr::filter(SUBID %in% station_subids)

df_plot <- cout_long |>
  dplyr::left_join(thresholds, by = "SUBID") %>% 
  rename(frcst_type = cat)

thresholds_long <- thresholds |>
  dplyr::filter(SUBID %in% station_subids) |>
  dplyr::mutate(
    ymin1 = 0,
    ymax1 = Q1,
    ymin2 = Q1,
    ymax2 = Q2,
    ymin3 = Q2,
    ymax3 = Q3,
    ymin4 = Q3,
    ymax4 = Inf
  )


# ------------------------------------------------------------
# 7. Hydrograph plot
# ------------------------------------------------------------

hydrograph_plot <- ggplot2::ggplot() +
  
  ggplot2::geom_rect(
    data = thresholds_long,
    ggplot2::aes(xmin = -Inf, xmax = Inf, ymin = ymin1, ymax = ymax1),
    fill = "#A8E6A3",
    alpha = 0.6
  ) +
  
  ggplot2::geom_rect(
    data = thresholds_long,
    ggplot2::aes(xmin = -Inf, xmax = Inf, ymin = ymin2, ymax = ymax2),
    fill = "#FFD700",
    alpha = 0.6
  ) +
  
  ggplot2::geom_rect(
    data = thresholds_long,
    ggplot2::aes(xmin = -Inf, xmax = Inf, ymin = ymin3, ymax = ymax3),
    fill = "#FF8C00",
    alpha = 0.6
  ) +
  
  ggplot2::geom_rect(
    data = thresholds_long,
    ggplot2::aes(xmin = -Inf, xmax = Inf, ymin = ymin4, ymax = ymax4),
    fill = "#FF0000",
    alpha = 0.6
  ) +
  
  ggplot2::geom_line(
    data = df_plot,
    ggplot2::aes(x = dates, y = Q, group = SUBID,color = frcst_type)
    ,
    linewidth = 0.8
  ) +
  
  # ggplot2::geom_line(
  #   data = df_plot,
  #   ggplot2::aes(x = dates, y = Q, group = SUBID),
  #   color = "black",
  #   linewidth = 0.6
  # ) +
  
  ggplot2::geom_point(
    data = df_plot,
    ggplot2::aes(x = dates, y = Q),color = "black",
    size = 1.5
  ) +
  
  ggplot2::facet_wrap(~ SUBID, scales = "free_y", ncol = 2) +
  
  ggplot2::labs(
    x = "Date",
    y = expression("Discharge (m"^3*"/s)"),
    color = ""
  ) +
  scale_color_manual(values = c("blue","black"))+
  ggplot2::theme_classic() +
  ggplot2::theme(
    strip.text = ggplot2::element_text(face = "bold"),
    legend.position = "bottom"
  )


# ---- Hydrographs ----

ggplot2::ggsave(
  filename = file.path(OUTPUT_DIR, paste0("hydrographs_",time_ref,"_forecast.png")),
  plot = hydrograph_plot,
  width = config$figures$hydrograph$width,
  height = config$figures$hydrograph$height,
  dpi = 300,
  bg = "white"
)
ncdf4::nc_close(frct)
ncdf4::nc_close(analysis)
# ------------------------------------------------------------
# 9. Objects generated by the script
# ------------------------------------------------------------
# base_map_max_frcst : maximum risk map over 10 days
# base_map_d10_frcst : daily risk maps
# hydrograph_plot    : hydrographs with alert thresholds
# cout_al_long_shp   : spatial risk data
# df_plot            : discharge data for plotting
# thresholds         : alert thresholds by SUBID

