# ******************************************************************************
# Crop maine ecoregion map from the NE ecoregion map and align both ecoregion
# map and imputation map.
# 
# Author: Xiaojie Gao
# Date: 2025-08-06
# ******************************************************************************
rm(list = ls())
source("src/base.R")
library(terra)



# NE ecoregion map
ne_eco_map <- rast(base$ne_eco_map)
# Maine imputed map
maine_imp_map <- rast("pipe/timber_harvest/maine_imp_1995_200m_stdage.tif")

# Project and mask
maine_eco_map <- project(ne_eco_map, maine_imp_map, method = "near", mask = TRUE)
maine_eco_map <- mask(maine_eco_map, maine_imp_map[[1]])
# plot(maine_eco_map)
# plot(maine_imp_map[[1]], add = TRUE)

# Also mask the imputation map
maine_imp_map <- mask(maine_imp_map, maine_eco_map)

# Check whether the total forest area matches with expectation
forest_map <- maine_imp_map[[1]]
forest_map[forest_map != 0 & !is.na(forest_map)] <- 1
# Convert to ha
forest_map_ha <- forest_map * 200^2 * 0.0001
# Total number of cells that are not NA
(me_total_ha <- sum(values(forest_map_ha), na.rm = TRUE))


# out: 
writeRaster(
    maine_eco_map,
    file.path(
        "pipe/timber_harvest",
        "maine_eco_map_200_align_impmap_250812.img"
    ),
    datatype = "INT2S", filetype = "HFA",
    overwrite = TRUE, NAflag = 0
)

writeRaster(
    maine_imp_map,
    file.path(
        "pipe/timber_harvest", 
        "maine_imp_1995_200m_stdage_align_ecomap_250812.tif"
    ),
    overwrite = TRUE
)
# Write another imputation table file
fia_fea_dt <- fread(
    file.path("pipe/timber_harvest", "maine_imp_1995_200m_stdage.csv")
)
fwrite(
    fia_fea_dt,
    file.path(
        "pipe/timber_harvest",
        "maine_imp_1995_200m_stdage_align_ecomap_250812.csv"
    )
)
