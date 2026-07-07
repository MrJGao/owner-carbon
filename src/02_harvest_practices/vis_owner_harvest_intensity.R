# ******************************************************************************
# Visualize harvest intensity per owner type.
# 
# Author: Xiaojie Gao
# Date: 2025-10-09
# ******************************************************************************
rm(list = ls())
source("src/vis_owner_color.R")
library(data.table)



pipedir <- "pipe/02_harvest_practices"

outdir <- "out/02_harvest_practices"
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)


hrvst_intensity_dt <- fread(file.path(pipedir, "owner_harvest_intensity_dt.csv"))

# ! Remove "Federal" and "Municipal" b/c we don't have enough FIA harvest events
# ! to calculate their intensity correctly.
hrvst_intensity_dt <- hrvst_intensity_dt[!OwnerType %in% c("Federal", "Municipal"), ]

# Remove "Other"
hrvst_intensity_dt <- hrvst_intensity_dt[OwnerType != "Other"]
# Not show "Unknow"
hrvst_intensity_dt <- hrvst_intensity_dt[OwnerType != "Unknown"]

setorder(hrvst_intensity_dt, harv_int)

# Reorder to make color consistent
cols_reorder <- col_dt[match(hrvst_intensity_dt$OwnerType, col_dt$OwnerType), color]

# Change REIT/TIMO to Investor
hrvst_intensity_dt[OwnerType == "REIT/TIMO", OwnerType := "Investor"]
col_dt[OwnerType == "REIT/TIMO", OwnerType := "Investor"]


{ # fig: harvest intensity
    svglite::svglite(
        file.path(outdir, "harvest_intensity_per_owner.svg"),
        width = 6, height = 5
    )
    par(mar = c(4, 10, 1, 1), mgp = c(2.5, 0.5, 0), cex = 1.4)
    coords <- barplot(
        hrvst_intensity_dt$harv_int,
        horiz = TRUE, names.arg = hrvst_intensity_dt$OwnerType,
        col = cols_reorder,
        border = NA,
        las = 1,
        xlim = c(0, 100),
        xlab = "Harvest Intensity \n(% basal area removal)"
    )
    segments(
        hrvst_intensity_dt[, harv_int - harv_int_sd], coords,
        hrvst_intensity_dt[, harv_int + harv_int_sd], coords
    )
    dev.off()
}

