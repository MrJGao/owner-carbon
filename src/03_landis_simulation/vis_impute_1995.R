# ******************************************************************************
# Visualize the 1995 imputation.
# 
# Author: Xiaojie Gao
# Date: 2025-06-26
# ******************************************************************************
rm(list = ls())
library(terra)



imp_img <- rast("pipe/timber_harvest/maine_imp_1995_200m_stdage.tif")


outdir <- "out/03_landis_simulation"
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)


{
    png(
        file.path(outdir, "imp_1995.png"),
        width = 1000, height = 1400, res = 300
    )

    plotRGB(imp_img, r = 1, g = 2, b = 3)
    legend(
        grconvertX(0.6, "ndc"), grconvertY(0.85, "ndc"),
        bty = "n",
        legend = c("1st neighbor", "2nd neighbor", "3rd neighbor"),
        fill = c("red", "green", "blue"),
        border = NA,
        cex = 0.5, y.intersp = 0.8, x.intersp = 0.5
    )
    dev.off()
}

