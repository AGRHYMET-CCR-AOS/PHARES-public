# ============================================================
# EXERCISE 1 — Explore PHARES NetCDF files
# Objective: Open forecast and analysis files and inspect their content
# ============================================================

library(ncdf4)

# Paths
FORECAST_FILE <- "data/raw/fanfar/forecast_2026-05-06R1.nc"
ANALYSIS_FILE <- "data/raw/fanfar/analysis_2026-05-06R1.nc"

# Open NetCDF files
frct <- nc_open(FORECAST_FILE)
analysis <- nc_open(ANALYSIS_FILE)

# Display general structure
names(frct)
names(analysis)

# List variables
names(frct$var)
names(analysis$var)

# List dimensions
names(frct$dim)
names(analysis$dim)

# Close files
nc_close(frct)
nc_close(analysis)
