# ============================================================
# EXERCISE 3 — Derive flood risk levels from thresholds
# Objective: Reproduce alert levels using discharge and thresholds
# ============================================================

library(ncdf4)

FORECAST_FILE <- "data/raw/fanfar/forecast_2026-05-06R1.nc"

frct <- nc_open(FORECAST_FILE)

# Extract discharge
cout <- ncvar_get(frct, "cout")

# Extract thresholds
thr1 <- ncvar_get(frct, "cout_AL_thresholds_1")
thr2 <- ncvar_get(frct, "cout_AL_thresholds_2")
thr3 <- ncvar_get(frct, "cout_AL_thresholds_3")

# Extract official alert levels
cout_AL <- ncvar_get(frct, "cout_AL")

# Derive alert levels manually
risk_manual <- ifelse(
  cout < thr1, 0,
  ifelse(
    cout < thr2, 1,
    ifelse(
      cout < thr3, 2,
      3
    )
  )
)
View(risk_manual)

nc_close(frct)