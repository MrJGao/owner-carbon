# ******************************************************************************
# Due to the uncertainty created by the US government and thus NOAA, here we
# download Daymet monthly climate data for the entire region 9 for future use.
# 
# Author: Xiaojie Gao
# Date: 2025-04-28
# ******************************************************************************
rm(list = ls())
source("src/base.R")
library(daymetr)
library(terra)
library(parallel)



# R9 boundary
r9_bound <- vect(base$region9_shpfile)

state_names <- r9_bound$STUSPS

cl <- makeCluster(20)
calls <- clusterCall(cl, function() {
    suppressWarnings({
        library(daymetr)
    })
})

for (st in state_names) {
    message("Processing ", st, "...")

    st_bound <- r9_bound[r9_bound$STUSPS == st, ]
    st_bound <- project(st_bound, "epsg:4326")
    st_ext <- ext(st_bound)

    st_ext_vector <- as.vector(st_ext)


    # Download Daymet tiles in this region
    outdir <- paste0("data/raw/R9_Daymet_month/", st)
    dir.create(outdir, showWarnings = FALSE, recursive = TRUE)


    clusterExport(cl, c("outdir", "st_ext_vector"), envir = environment())
    
    null <- clusterApplyLB(cl, x = 1980:2023, fun = function(yr) {
        download_daymet_ncss(
            location = st_ext_vector[c(4, 1, 3, 2)],
            start = yr,
            end = yr,
            frequency = "monthly",
            param = "ALL",
            path = outdir
        )
    })
    message(st, " Done!")

    Sys.sleep(30)
}

stopCluster(cl)


