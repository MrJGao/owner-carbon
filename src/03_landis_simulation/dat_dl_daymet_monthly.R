# ******************************************************************************
# Download Daymet monthly data for LANDIS run later.
# 
# Author: Xiaojie Gao
# Date: 2025-03-04
# ******************************************************************************
library(daymetr)
library(terra)
library(parallel)



# NE boundary
ne_bound <- vect("data/NewEngland.geojson")
ne_bound <- project(ne_bound, "epsg:4326")
ne_ext <- ext(ne_bound)

ne_ext_vector <- as.vector(ne_ext)


# Download Daymet tiles in this region
outdir <- "data/raw/NE_Daymet_month"
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

cl <- makeCluster(20)
calls <- clusterCall(cl, function() {
    suppressWarnings({
        library(daymetr)
    })
})
clusterExport(cl, c("outdir", "ne_ext_vector"))
null <- clusterApplyLB(cl, x = 1985:2024, fun = function(yr) {
    download_daymet_ncss(
        location = ne_ext_vector[c(4, 1, 3, 2)],
        start = yr,
        end = yr,
        frequency = "monthly",
        param = "ALL",
        path = outdir
    )
})
stopCluster(cl)


