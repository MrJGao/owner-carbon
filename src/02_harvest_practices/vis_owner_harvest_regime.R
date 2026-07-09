# ******************************************************************************
# Visualize ownership harvest rates and intensities using the table exported
# from Jonathan's analysis.
# 
# Author: Xiaojie Gao
# Date: 2026-05-29
# ******************************************************************************
library(data.table)
source("src/vis_owner_color.R")



# ~ Harvest Rates ####
# ~ ----------------------------------------------------------------------------
harv_rates <- fread("pipe/02_harvest_practices/owner_harvest_rate_control.csv")
setnames(harv_rates, "V1", "OwnerType")

# Make values annual
harv_rates[, est := est / 5]
harv_rates[, lo := lo / 5]
harv_rates[, hi := hi / 5]


setorder(harv_rates, est)

{
    svglite::svglite("out/02_harvest_practices/harv_rate.svg", width = 5, height = 5)
    par(mar = c(3, 10, 1, 1), mgp = c(1.5, 0.5, 0))
    plot(
        NA,
        xlim = c(-4.5, 1), ylim = c(1, 10),
        bty = "n", yaxt = "n", xlab = "Relative Annual harvest rate (%)", ylab = ""
    )
    axis(
        side = 2,
        at = 1:(nrow(harv_rates) + 1),
        labels = c(harv_rates$OwnerType, "Investor"),
        las = 2, lwd = 0,
        line = -1
    )
    abline(v = 0, lty = 2)

    points(
        0, nrow(harv_rates) + 1, 
        pch = 16, 
        col = col_dt[OwnerType == "REIT/TIMO", color],
        cex = 1.5
    )
    for (i in 1:nrow(harv_rates)) {
        segments(
            harv_rates[i, lo], i,
            harv_rates[i, hi], i,
        )
        points(
            harv_rates[i, est], i,
            pch = ifelse(harv_rates[i, bh_p] < 0.05, 16, 1),
            col = col_dt[OwnerType == harv_rates[i, OwnerType], color],
            cex = 1.5,
            lwd = 2
        )
    }

    dev.off()
}



# ~ Harvest Intensity ####
# ~ ----------------------------------------------------------------------------
harv_int <- fread("pipe/02_harvest_practices/owner_harvest_intensity_control.csv")
setnames(harv_int, "group", "OwnerType")

# Reorder to match harvest rates ordering
harv_int <- harv_int[match(harv_rates$OwnerType, harv_int$OwnerType),]
harv_int[, OwnerType := harv_rates$OwnerType]

{
    svglite::svglite("out/02_harvest_practices/harv_int.svg", width = 5, height = 5)
    par(mar = c(3, 10, 1, 1), mgp = c(1.5, 0.5, 0))
    plot(
        NA,
        xlim = c(-35, 20), ylim = c(1, 10),
        bty = "n", yaxt = "n", xlab = "Relative Harvest Intensity (%)", ylab = ""
    )
    axis(
        side = 2,
        at = 1:(nrow(harv_int) + 1),
        labels = c(harv_int$OwnerType, "Investor"),
        las = 2, lwd = 0,
        line = 0
    )
    abline(v = 0, lty = 2)

    points(
        0, nrow(harv_int) + 1,
        pch = 16,
        col = col_dt[OwnerType == "REIT/TIMO", color],
        cex = 1.5
    )
    for (i in 1:nrow(harv_int)) {
        segments(
            harv_int[i, lo], i,
            harv_int[i, hi], i,
        )
        points(
            harv_int[i, est], i,
            pch = ifelse(harv_int[i, bh_p] < 0.05, 16, 1),
            col = col_dt[OwnerType == harv_int[i, OwnerType], color],
            cex = 1.5,
            lwd = 2
        )
    }

    dev.off()
}



