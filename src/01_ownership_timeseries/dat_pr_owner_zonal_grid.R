# ******************************************************************************
# Sumamrize ownership polygons into regular grids and output shpfiles
# 
# Author: Xiaojie Gao
# Date: 2025-05-13
# ******************************************************************************
rm(list = ls())
source("src/base.R")
source("src/vis_owner_color.R")
library(data.table)
library(magrittr)
library(terra)
library(parallel)
library(readxl)
library(googlesheets4)



pipedir <- "pipe/01_ownership_timeseries"
dir.create(pipedir, showWarnings = FALSE, recursive = TRUE)



# ~ Load shpfiles ####
# ~ ----------------------------------------------------------------------------
# Read from the google sheet directly
gsheet_url <- base$online_owner_table
owner_dt <- read_sheet(gsheet_url, sheet = "Clean")
owner_dt <- as.data.table(owner_dt)
owner_dt <- owner_dt[, .(FullName, OwnerType = OwnerType)]
owner_dt <- unique(na.omit(owner_dt))

possible_names <- read_sheet(gsheet_url, sheet = "Name_variety") %>%
    as.data.table() %>%
    na.omit()


# Merge back w/ the shpfiles
shpdir <- "data/maine_ownership_2024/clean"
shpoutdir <- "data/maine_ownership_2024/clean_with_types"
dir.create(shpoutdir, recursive = TRUE, showWarnings = FALSE)

yrs <- c(1995, 2000, 2005, 2010, 2014, 2018, 2019:2024)

if (length(list.files(shpoutdir)) == 0) {
    shplist <- lapply(yrs, function(yr) {
        shp <- vect(file.path(shpdir, paste0("meown_", yr, ".shp")))
        shp <- merge(shp, owner_dt, by = "FullName")
        # NOTE: Make names consistent
        for (i in 1:nrow(possible_names)) {
            pn <- possible_names[i, ]
            idx <- which(shp$FullName == pn$possible_names)
            if (length(idx) > 0) {
                shp$FullName[idx] <- pn$name
            }
        }
        # Use projection NAD83 / Conus Albers (EPSG:5070)
        shp <- project(shp, "EPSG:5070")
        shp$area <- expanse(shp)

        # out: Write out the shpfiles
        writeVector(
            shp,
            file.path(shpoutdir, paste0("meowntype_", yr, ".shp")),
            overwrite = TRUE
        )

        return(shp)
    })
} else {
    # Read the shpfiles
    shplist <- lapply(yrs, function(yr) {
        shp <- vect(file.path(shpoutdir, paste0("meowntype_", yr, ".shp")))
        return(shp)
    })
}


# Unique owner types
uni_owner <- sapply(shplist, function(shp) {
    unique(shp$OwnerType)
}) %>%
    c() %>%
    unique()



# ~ Summarize owner types into regular grids ####
# ~ ----------------------------------------------------------------------------
# What are the sizes of the owner polygons
owner_areas <- lapply(shplist, function(shp) shp$area)
sapply(owner_areas, quantile, c(0.05, 0.1, 0.15)) %>%
    sqrt()
# ^ B/c 90% of the owner polygons are greater than a ~120*120 m grid; 85% of the
# owner polygons are greater than a ~200*200 m grid. We chose 200*200 m grid as
# the regular grid to balance accracy and computational cost.


# B/c the extent of the shpfiles are not consistent, here I iterate and
# calculate an outer most boundary
outext <- lapply(shplist, function(shp) {
    as.polygons(ext(shp), crs = crs(shp))
}) %>%
    do.call(rbind, .) %>%
    ext() %>%
    as.vector()


outdir <- file.path(pipedir, "owner_grid")
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)


# Use parallel processing to summarize the major owner type for each zonal grid
cl <- makeCluster(length(yrs), outfile = "")
calls <- clusterCall(cl, function() {
    suppressMessages({
        library(data.table)
        library(magrittr)
        library(terra)
    })
})


#! The following takes ~15 min to finish
tstart <- Sys.time()
clusterExport(cl, c(
    "shpdir", "owner_dt", "outext", "pipedir", "possible_names",
    "outdir"
))
owner_grid <- clusterApplyLB(cl, x = yrs, fun = function(yr) {
    shp <- vect(file.path(shpdir, paste0("meown_", yr, ".shp")))
    shp <- merge(shp, owner_dt, by = "FullName")

    # NOTE: Make names consistent
    for (i in 1:nrow(possible_names)) {
        pn <- possible_names[i, ]
        idx <- which(shp$FullName == pn$possible_names)
        if (length(idx) > 0) {
            shp$FullName[idx] <- pn$name
        }
    }

    # Use projection NAD83 / Conus Albers (EPSG:5070)
    shp <- project(shp, "EPSG:5070")
    shp$area <- expanse(shp)

    # Use the extent of one of the shpfiles
    # Grid size is 200 meters
    hm <- rast(extent = ext(outext), res = 200, crs = crs(shp), vals = 1)

    hm_cells <- as.polygons(hm, dissolve = FALSE, na.rm = FALSE)
    hm_cells$ID <- 1:nrow(hm_cells)

    stat <- intersect(shp, hm_cells)
    hm_vals <- by(stat, stat$ID, function(x) {
        # Find the unique OBJECTID and area
        uni <- unique(x[, c("OBJECTID", "area")])

        # If the summed area does not occupy >50% of the cell area, assign NA
        geom_size <- sum(uni$area)
        cell_size <- res(hm)[1] * res(hm)[2]
        if (geom_size < 0.5 * cell_size) {
            return(NULL)
        }

        # Find the one w/ the biggest area
        max_obj <- x[x$OBJECTID == uni[which.max(uni$area), "OBJECTID"], ]
        if (nrow(max_obj) > 1) {
            max_obj <- max_obj[1, ]
        }
        return(max_obj)
    }) %>%
        do.call(rbind, .)

    shp_cells <- merge(
        hm_cells, hm_vals[, c("OwnerType", "FullName", "ID")],
        by = "ID",
        all.x = TRUE
    )

    # out: ownership in regular grids
    writeVector(
        shp_cells,
        file.path(outdir, paste0("owner_grid_", yr, ".shp")),
        overwrite = TRUE
    )
})
tend <- Sys.time()
ttake <- tend - tstart
message(round(ttake, 2), " ", units(ttake))

stopCluster(cl)
