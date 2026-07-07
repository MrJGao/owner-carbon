# ******************************************************************************
# Figure out the reasonable timber harvest patch sizes from the change detection
# time series maps.
# 
# Author: Xiaojie Gao
# Date: 2025-04-27
# ******************************************************************************
library(terra)
library(data.table)



pipedir <- "pipe/timber_harvest"
dir.create(pipedir, showWarnings = FALSE, recursive = TRUE)


pat_ncells_filename <- file.path(pipedir, "pat_ncells_dt.csv")
if (!file.exists(pat_ncells_filename)) {
    # Change detection maps
    hs <- rast("data/timber_harvest_summary.tif")

    pat <- patches(hs)
    pat_ncells <- freq(pat)

    pat_ncells_dt <- as.data.table(pat_ncells)
    fwrite(pat_ncells_dt, pat_ncells_filename)
}
pat_ncells_dt <- fread(pat_ncells_filename)


pat_ncells_dt[, .(min(count), max(count)), by = "layer"]

qt <- lapply(unique(pat_ncells_dt$layer), function(i) {
    quantile(pat_ncells_dt[layer == i]$count, c(0.025, 0.975)) * 0.09
})
qt <- do.call(rbind, qt)
max(qt[2])

hist(pat_ncells$count, xlim = c(0, 20), breaks = 1e4)
mean(pat_ncells$count)