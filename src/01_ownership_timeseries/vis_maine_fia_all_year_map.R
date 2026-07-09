# ******************************************************************************
# A map showing FIA plot locations during 1990s-present in Maine
# 
# Author: Xiaojie Gao
# Date: 2025-01-23
# ******************************************************************************
rm(list = ls())
source("src/base.R")
library(terra)
library(sf)
library(tmap)
library(data.table)




fia_dt <- fread("data/fia_attr_dt_allyear_maine.csv")[
    nsubplotsForest == 4, .(concatPlot, x, y, STATE)
]
fia_dt <- unique(fia_dt)

fia_sf <- st_as_sf(fia_dt, coords = c("x", "y"), crs = "epsg:4326")
# plot(fia_sf)

maine_county <- st_read(base$me_county_shp)

# Ecoregion map
eco_maine <- rast(base$me_eco_map)

eco_r9_file <- base$r9_eco_geojson
eco_r9 <- st_read(eco_r9_file)
unique(eco_r9$STUSPS)
eco_maine_shp <- subset(eco_r9, STUSPS == "ME")
eco_maine_shp <- st_transform(eco_maine_shp, "EPSG:5070")


# Ownership data covered region
owner_region <- st_read("data/maine_ownership_2024/clean_with_types/meowntype_1995.shp")
# owner_region <- st_union(owner_region, is_coverage = TRUE)


# Project all to EPSG:5070
fia_sf <- st_transform(fia_sf, "EPSG:5070")
maine_county <- st_transform(maine_county, "EPSG:5070")
owner_region <- st_transform(owner_region, "EPSG:5070")



{ #fig:
    maine_map <- tm_shape(eco_maine_shp) +
        tm_fill(
            fill = "NA_L2NAME",
            fill.scale = tm_scale_categorical(
                n.max = 2,
                c("#79cbd1", "#aacc66")
                ),
            fill.legend = tm_legend(
                title = "Ecological Regions of \nNorth America (Level II)",
                position = c("right", "top"),
                frame = FALSE,
                text.size = 0.8
            )
        ) +
        tm_shape(maine_county) +
            tm_borders(col = "white", lwd = 2) +
        tm_shape(fia_sf) +
            tm_dots(fill = "grey50", size = 0.2) +
        # tm_shape(owner_region) +
        #     tm_borders(col = "blue", lwd = 2) +
        tm_compass(north = 325, size = 2, text.size = 1, position = c("left", "top")) +
        tm_scalebar(
            width = 9, 
            position = c(0.67, 0.85),
            text.size = 1, lwd = 2
        ) +
        # tm_credits(
        #     paste(format(nrow(fia_sf), big.mark = ","), "sites"),
        #     position = c("left", "bottom"), size = 1.2
        # ) +
        tm_layout(
            frame = FALSE, bg.color = adjustcolor("white", 0)
        )

    png(
        "out/maine_fia_map_allyr.png", 
        width = 2200, height = 2800, res = 300,
        bg = NA
        
    )
    print(maine_map)
    dev.off()
}

