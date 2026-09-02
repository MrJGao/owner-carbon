# ******************************************************************************
# Show LANDIS calibrated growth is similar to FIA growth
# 
# Author: Xiaojie Gao
# Date: 2026-08-17
# ******************************************************************************
rm(list = ls())
library(data.table)




PlotFigure <- function(dt, figfile) {
    fia_dt <- dt[source == "fia",]
    fia_dt[, year := seq(2000, 2020, by = 5) + 0.5]
    landis_dt <- dt[source == "landis" & type == "noharv",]
    landis_dt[, year := seq(1995, 2025, by = 5) - 0.5]


    png(figfile, width = 1000, height = 700, res = 200)
    par(bty = "L", mgp = c(1.5, 0.5, 0), mar = c(3, 3, 1, 1))
    plot(
        NA, xlim = c(1995, 2025), 
        ylim = c(0, max(dt$meanAGCMgha + 2 * dt$sd)),
        xlab = "Year", ylab = "Aboveground Carbon (Mg C / ha)"
    )
    # FIA
    points(
        fia_dt[, .(year, meanAGCMgha)], pch = 16, cex = 1.5, 
        col = "black"
    )
    segments(
        fia_dt$year, fia_dt[, meanAGCMgha - sd],
        fia_dt$year, fia_dt[, meanAGCMgha + sd],
        col = "black", lwd = 2
    )

    # LANDIS
    points(
        landis_dt[, .(year, meanAGCMgha)], 
        pch = 16, col = "blue", cex = 1.5
    )
    segments(
        landis_dt$year, landis_dt[, meanAGCMgha - sd],
        landis_dt$year, landis_dt[, meanAGCMgha + sd],
        col = "blue", lwd = 2
    )

    legend(
        bty = "n",
        "topleft", legend = c("FIA", "LANDIS"),
        pch = 16, col = c("black", "blue"), lwd = 2, lty = 1
    )

    dev.off()
}



# Danelle made this table for me
dt <- fread("pipe/03_landis_simulation/ownerCarbonFIAcompare_GutCheckAllRuns_2026-08-17.csv")
dt <- dt[simuID != "dynamicLowN"]
# Megagram/hectare and tons/hectare is 1:1
# Growth rate
# dt[, 
#     rate := (shift(meanAGCMgha, -1) - meanAGCMgha) / meanAGCMgha * 100, 
#     by = c("source", "type")
# ]
# dt[, 
#     ann_growth := (shift(meanAGCMgha, -1) - meanAGCMgha) / 5, 
#     by = c("source", "type")
# ]



outdir <- "out/03_landis_simulation"
PlotFigure(dt, file.path(outdir, "landis_fia_growth.png"))

