# ******************************************************************************
# Summarize FIA harvest records and owner type information
# 
# Author: Xiaojie Gao
# Date: 2026-04-02
# ******************************************************************************
rm(list = ls())
library(terra)
library(data.table)
library(magrittr)



pipedir <- "pipe/02_harvest_practices"
dir.create(pipedir, showWarnings = FALSE, recursive = TRUE)


fia_dt <- fread("data/fia_attr_dt_allyear_maine.csv")
fia_dt[!is.na(plotGroupBAm2ha_removed), .(plotGroupBAm2ha_removed)]

table(fia_dt[, .(MEASYEAR)])
unique(fia_dt[, .(PLT_CN)])


FindFIAOwner <- function(yr, fia_dt) {
    # ! Assuming 2014 is 2015, and 2024 is 2025
    if (yr == 2015) yr <- 2014
    if (yr == 2025) yr <- 2024
    owner_grid_yr <- vect(
        file.path(
            "pipe/timber_harvest/owner_grid",
            paste0("owner_grid_", yr, ".shp")
        )
    )

    fia_dt_yr <- fia_dt[
        nsubplotsForest == 4 & MEASYEAR %in% c((yr - 4):yr),
        .(
            concatPlot, PLT_CN, X, x, y,
            MEASYEAR,
            AGC_live_Mgha = plotGroupAGCMgha_live,
            BA_live_m2ha = plotGroupBAm2ha_live,
            BAremove_m2ha = plotGroupBAm2ha_removed,
            ifharv = ifelse(is.na(plotGroupBAm2ha_removed), 0, 1),
            pc_ba_removed = plotGroupBAm2ha_removed / (plotGroupBAm2ha_live + plotGroupBAm2ha_removed) * 100
        )
    ]

    fia_yr <- vect(fia_dt_yr, geom = c("x", "y"), crs = "epsg:4326") %>%
        project(owner_grid_yr)

    fia_yr_owner <- terra::extract(owner_grid_yr, fia_yr)
    fia_dt_yr[, OwnerType := fia_yr_owner$OwnerType]

    return(fia_dt_yr)
}


harv_per_owner_dt_file <- file.path(pipedir, "harv_per_owner_dt.csv")
if (!file.exists(harv_per_owner_dt_file)) {
    pb <- txtProgressBar(min = 0, max = 100, style = 3)
    # We don't bother extract prior 2000 because we don't have FIA measurements
    harv_per_owner_dt <- lapply(seq(2000, 2025), \(yr) {
        res <- FindFIAOwner(yr, fia_dt = fia_dt)
        setTxtProgressBar(pb, which(yr == yrs) * 100 / length(yrs)) # update progress
        return(res)
    }) %>%
        rbindlist()

    harv_per_owner_dt[is.na(OwnerType), OwnerType := "Unknown"]

    harv_per_owner_dt$MEASYEAR_aligned <- sapply(harv_per_owner_dt$MEASYEAR, \(yr) {
        yrs[which.min(abs(yrs - yr))]
    })
    
    # out: 
    fwrite(harv_per_owner_dt, file = harv_per_owner_dt_file)
}
harv_per_owner_dt <- fread(harv_per_owner_dt_file)

harv_per_owner_dt[, table(MEASYEAR)]
harv_per_owner_dt[, table(MEASYEAR_aligned)]


# Find measurement gap, initial basal area and initial owner type --------------
# plotid <- harv_per_owner_dt[, sample(concatPlot, 1)]
DoPlotHistory <- function(plotid, harv_per_owner_dt) {
    plotdt <- harv_per_owner_dt[concatPlot == plotid]
    # Sort by year
    setorder(plotdt, MEASYEAR)
    plotdt$meas_gap <- c(NA, plotdt[, diff(MEASYEAR)])
    plotdt$ownertype_init <- plotdt[, shift(OwnerType, 1)]
    setcolorder(plotdt, "meas_gap", after = "MEASYEAR")
    setcolorder(plotdt, "ownertype_init", before = "OwnerType")

    return(plotdt)
}

fia_harv_dt <- lapply(
    unique(harv_per_owner_dt$concatPlot), 
    DoPlotHistory, harv_per_owner_dt = harv_per_owner_dt
) %>%
    rbindlist()

uniqueN(fia_harv_dt$concatPlot)


# out: fia_harvest_per_owner.csv
fwrite(fia_harv_dt, file.path(pipedir, "fia_harvest_per_owner.csv"))
