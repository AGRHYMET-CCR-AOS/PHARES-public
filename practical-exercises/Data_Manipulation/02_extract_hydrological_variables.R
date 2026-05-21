# ============================================================
# EXERCISE 2 — Extract main hydrological variables
# Objective: Read discharge, precipitation, runoff and alert variables
# ============================================================

library(ncdf4)

FORECAST_FILE <- "data/raw/fanfar/forecast_2026-05-06R1.nc"

frct <- nc_open(FORECAST_FILE)

# Extract variables
cout <- as.data.frame(ncvar_get(frct, "cout"))
cprc <- as.data.frame(ncvar_get(frct, "cprc"))
upcprc <- as.data.frame(ncvar_get(frct, "upcprc"))
cros <- as.data.frame(ncvar_get(frct, "cros"))
evap <- as.data.frame(ncvar_get(frct, "evap"))


# Add colnames and rownames
subid <- ncvar_get(frct, "id")
time_raw <- ncdf4::ncvar_get(frct, "time")
dates <- as.Date(
  as.POSIXct(time_raw, origin = "1970-01-01", tz = "UTC")
)


colnames(cout) <- format(dates, "%Y%m%d")
cout$SUBID <- c(subid)


colnames(cprc) <- format(dates, "%Y%m%d")
cprc$SUBID <- c(subid)

colnames(upcprc) <- format(dates, "%Y%m%d")
upcprc$SUBID <- c(subid)


colnames(cros) <- format(dates, "%Y%m%d")
cros$SUBID <- c(subid)
# Extract alert level variables
cout_AL <- as.data.frame(ncvar_get(frct, "cout_AL"))
cout_AL_QC <- as.data.frame(ncvar_get(frct, "cout_AL_QC"))

# Export
write.table(x = cout,file = "cout.csv",append = FALSE,quote = FALSE,sep = ",",row.names = FALSE)

nc_close(frct)
