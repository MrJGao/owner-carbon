# ******************************************************************************
# Visualize avg. harvest rate per owner type. Havest rates were calculated from
# FIA data (see `mod_owner_harvest_rates_fia.R`)
# 
# Author: Xiaojie Gao
# Date: 2025-12-24
# ******************************************************************************
rm(list = ls())
source("src/vis_owner_color.R")
library(data.table)



pipedir <- "pipe/02_harvest_practices"

outdir <- "out/02_harvest_practices"
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)


hrvst_rates_dt <- fread(file.path(pipedir, "owner_harvest_rates_dt.csv"))


# Federal and Municipal are not reliable
hrvst_rates_dt <- hrvst_rates_dt[!OwnerType %in% c("Federal", "Municipal")]

# Remove Other and Unknown from the figure
hrvst_rates_sub <- hrvst_rates_dt[!OwnerType %in% c("Other", "Unknown")]

hrvst_rates_sub[, harv_prob := harv_prob * 100]
hrvst_rates_sub[, harv_prob_std := harv_prob_std * 100]

# Reorder
setorder(hrvst_rates_sub, harv_prob)


# Reorder to make color consistent
cols_reorder <- col_dt[match(hrvst_rates_sub$OwnerType, col_dt$OwnerType), color]

# Change REIT/TIMO to Investor
hrvst_rates_sub[OwnerType == "REIT/TIMO", OwnerType := "Investor"]
col_dt[OwnerType == "REIT/TIMO", OwnerType := "Investor"]

{ # fig: harvest rate
    svglite::svglite(
        file.path(outdir, "harvest_rate_per_owner.svg"),
        width = 6, height = 5
    )
    par(mar = c(4, 10, 1, 1), mgp = c(2.5, 0.5, 0), cex = 1.4)
    coords <- barplot(
        hrvst_rates_sub$harv_prob,
        horiz = TRUE, names.arg = hrvst_rates_sub$OwnerType,
        col = cols_reorder,
        border = NA,
        las = 1,
        xlim = c(0, ceiling(max(hrvst_rates_sub[, harv_prob + harv_prob_std]))),
        xlab = "Annual Harvest Rate \n(% Area)"
    )
    segments(
        hrvst_rates_sub[, harv_prob - harv_prob_std], coords,
        hrvst_rates_sub[, harv_prob + harv_prob_std], coords
    )
    dev.off()
}


