# ============================================================
# EXERCISE 4 — Map PHARES forecast flood risk
# Objective: Generate a flood risk map from PHARES NetCDF outputs
# ============================================================

library(ncdf4)
library(sf)
library(dplyr)
library(ggplot2)
library(ggspatial)

# -----------------------------
# 1. Input files
# -----------------------------

FORECAST_FILE <- "data/raw/fanfar/forecast_2026-05-06R1.nc"
SUBBASINS_FILE <- "data/statics/subbasins.shp"

# -----------------------------
# 2. Open NetCDF file
# -----------------------------

frct <- nc_open(FORECAST_FILE)

# Extract variables
cout_AL_QC <- ncvar_get(frct, "cout_AL_QC")

# Extract subbasin IDs from NetCDF dimension
subid <- ncvar_get(frct, "id")

nc_close(frct)

# -----------------------------
# 3. Prepare risk data
# -----------------------------

# Select forecast lead time
# Example: day 1 of the forecast
lead_time <- 1

risk_data <- data.frame(
  SUBID = subid,
  risk_value = cout_AL_QC[, lead_time]
)

risk_data <- risk_data %>%
  mutate(
    risk_level = case_when(
      risk_value == 0 ~ "Normal",
      risk_value == 1 ~ "Low risk",
      risk_value == 2 ~ "Moderate risk",
      risk_value == 3 ~ "High risk",
      TRUE ~ NA_character_
    ),
    risk_level = factor(
      risk_level,
      levels = c("Normal", "Low risk", "Moderate risk", "High risk")
    )
  )

# -----------------------------
# 4. Read subbasins shapefile
# -----------------------------

subbasins <- st_read(SUBBASINS_FILE)

# Make sure SUBID has the same type
subbasins <- subbasins %>%
  mutate(SUBID = as.numeric(SUBID))

risk_data <- risk_data %>%
  mutate(SUBID = as.numeric(SUBID))

# Join NetCDF risk values with shapefile
subbasins_risk <- subbasins %>%
  left_join(risk_data, by = "SUBID")

# -----------------------------
# 5. Plot risk map
# -----------------------------

risk_colors <- c(
  "Normal" = "#C7E9C0",
  "Low risk" = "#FFFFB2",
  "Moderate risk" = "#FE9929",
  "High risk" = "#DE2D26"
)

ggplot() +
  geom_sf(
    data = subbasins_risk,
    aes(fill = risk_level),
    color = NA,
    linewidth = 0
  ) +
  geom_sf(
    data = subbasins,
    fill = NA,
    color = "grey40",
    linewidth = 0.15
  ) +
  scale_fill_manual(
    name = "Risk level",
    values = risk_colors,
    drop = FALSE,
    na.value = "white"
  ) +
  annotation_scale(
    location = "bl",
    width_hint = 0.3
  ) +
  annotation_north_arrow(
    location = "tr",
    which_north = "true",
    style = north_arrow_fancy_orienteering
  ) +
  labs(
    title = "PHARES 10-day flood risk forecast",
    subtitle = paste("Forecast lead time: day", lead_time),
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_classic() +
  theme(
    text = element_text(size = 14),
    axis.title = element_text(face = "bold", size = 15),
    axis.text = element_text(size = 13),
    legend.position = "bottom",
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 12),
    panel.grid.major = element_line(color = "grey90", linewidth = 0.2),
    plot.title = element_text(face = "bold", size = 18),
    plot.subtitle = element_text(size = 13)
  )
