# ******************************************************************************
# Get FIA data for all years for the state of Maine.
# 
# Author: Xiaojie Gao
# Date: 2025-01-23
# ******************************************************************************
rm(list = ls())
source("src/base.R")
library(data.table)
library(terra)



fia_true_vect <- vect(base$fia_loc)
# Project to WGS84, although NAD83 and WGS84 are almost identical in Eastern US
fia_true_vect <- terra::project(fia_true_vect, "epsg:4326")

# According to Danelle, only use the X column.
fia_true_vect <- fia_true_vect[, c("X")]

# Table from Danelle
crosswalk <- fread(base$imax_fia_proj_dir, "data/raw/xConcatPlotcw.csv")
# Merge crosswalk table to get the concatPlot column
fia_true_vect <- merge(fia_true_vect, crosswalk, by = "X")

# FIA plot attributes w/ fuzzed corrdinates
fia_attr <- fread(base$fia_fullwide_file)[, PLT_CN := paste0("X", PLT_CN)]

fia_attr <- fia_attr[STATE == "ME"]
fia_attr[, .N, keyby = MEASYEAR]



# Merge point locations with measurements
gm <- data.table(geom(fia_true_vect))[, .(x, y)]
fia_attr_dt <- merge(
    cbind(as.data.table(fia_true_vect)[, .(X, concatPlot)], gm),
    fia_attr,
    by = "concatPlot"
)
fia_attr_dt[, .N, keyby = "MEASYEAR"]


# out: fia_attr_dt_allyear_maine.csv
fwrite(fia_attr_dt, "data/fia_attr_dt_allyear_maine.csv")

