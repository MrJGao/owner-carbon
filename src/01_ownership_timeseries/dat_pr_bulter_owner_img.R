# ******************************************************************************
# Cut Bulter's ownership data to Maine
# 
# Author: Xiaojie Gao
# Date: 2026-04-27
# ******************************************************************************
rm(list = ls())
library(terra)
library(magrittr)




# The following only needs to run once
# butler <- rast("data/raw/RDS-2025-0045/Data/US_forest_ownership.tif")
# # Crop by Maine boundary
# maine <- vect(base$region9_shpfile) %>%
#     subset(.$NAME == "Maine") %>%
#     project(butler)

# butler <- crop(butler, maine, mask = TRUE)

# writeRaster(butler, "data/Maine_forest_ownership_bulter.tif")


# ~ Append Bulter ownership layer ####
# ~ ----------------------------------------------------------------------------
# Use the Butler data to fill southern parts
butler <- rast("data/Maine_forest_ownership_bulter.tif")

# Read a template shpfile
shpoutdir <- "data/maine_ownership_2024/clean_with_types"
tempshp <- vect(file.path(shpoutdir, paste0("meowntype_", 1995, ".shp")))

butler <- project(butler, crs(tempshp), res = 200, method = "near")
# We only need Family Forests in the southern region and they are actually small
# parcel families
butler <- mask(butler, butler, maskvalues = 3, inverse = TRUE)

# Write to a new folder
shpoutdir_with_bulter <- file.path(
    "data/maine_ownership_2024",
    "clean_with_types_bulter"
)
dir.create(shpoutdir_with_bulter, showWarnings = FALSE, recursive = TRUE)


# Read the shpfiles
yrs <- c(1995, 2000, 2005, 2010, 2014, 2018, 2019:2024)
null <- lapply(yrs, function(yr) {
    shp <- vect(file.path(shpoutdir, paste0("meowntype_", yr, ".shp")))
    bulter_shp <- mask(butler, shp, inverse = TRUE)
    bulter_shp <- as.polygons(bulter_shp)
    names(bulter_shp) <- names(butler)
    bulter_shp$OwnerType <- "Small Family"

    shp <- rbind(shp, bulter_shp)

    # Here I need to recalculate area b/c Small Family was not calculated
    shp$area <- expanse(shp)

    # out:
    writeVector(
        shp,
        file.path(shpoutdir_with_bulter, paste0("meowntype_", yr, ".shp")),
        overwrite = TRUE
    )
})
