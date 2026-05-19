# ============================================================
# EXERCISE 2 — Extract main hydrological variables
# Objective: Read discharge, precipitation, runoff and alert variables
# ============================================================

library(ncdf4)

FORECAST_FILE <- "data/raw/fanfar/forecast_2026-05-06R1.nc"

frct <- nc_open(FORECAST_FILE)

# Extract variables
cout <- ncvar_get(frct, "cout")
cprc <- ncvar_get(frct, "cprc")
upcprc <- ncvar_get(frct, "upcprc")
cros <- ncvar_get(frct, "cros")
evap <- ncvar_get(frct, "evap")

# Extract alert level variables
cout_AL <- ncvar_get(frct, "cout_AL")
cout_AL_QC <- ncvar_get(frct, "cout_AL_QC")

# Check dimensions
dim(cout)
dim(cprc)
dim(cout_AL)

# Basic summary
summary(as.vector(cout))
summary(as.vector(cprc))
summary(as.vector(cout_AL))

nc_close(frct)
