# ******************************************************************************
# Visualize the NE FIA sites used to impute Maine init community 1995.
# 
# Author: Xiaojie Gao
# Date: 2025-08-02
# ******************************************************************************
rm(list = ls())
source("src/base.R")
library(terra)
library(sf)
library(tmap)
library(data.table)
library(rnaturalearth)



outdir <- "out/timber_harvest"
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)



# FIA data from Danelle
fia_dt <- fread("Y:/FICE/MAINE/pre1999plotsActCoord.csv")
fia_dt[(!is.na(LAT_ACTUAL_NAD83) | !is.na(LON_ACTUAL_NAD83)) & STATE == "ME"]

# Remove PLT_CNs w/ no live trees
fia_true <- fread("data/raw/pre2000plotCbyTPAUNADJ.csv")
fia_dt <- fia_dt[PLT_CN %in% fia_true$PLT_CN, ]


# NOTE --------------------------------------------------------------------
# ! Here I use a table that was created by Danelle to re-impute the map
fia_sub <- fread(
    "pipe/timber_harvest/tempForXiaojie_dises_1995_impmap_STDAGEplots.csv"
)
fia_dt <- fia_dt[PLT_CN %in% fia_sub$PLT_CN]
# -------------------------------------------------------------------------


fia_sf <- st_as_sf(
    fia_dt, 
    coords = c("LON_ACTUAL_NAD83", "LAT_ACTUAL_NAD83"), 
    crs = "epsg:4326"
)

# Ecoregion map
eco_r9_file <- base$r9_eco_geojson
eco_r9 <- st_read(eco_r9_file)
eco_ne_shp <- subset(eco_r9, Group == "NE")
eco_ne_shp <- st_transform(eco_ne_shp, "EPSG:5070")


# Project all to EPSG:5070
fia_sf <- st_transform(fia_sf, "EPSG:5070")

# Overview
states <- ne_states("United States of America")
states <- states[!states$name %in% c("Alaska", "Hawaii"), ]
states <- st_transform(states, "EPSG:5070")

# USFS region 9
r9_shp <- st_read(base$region9_shpfile)
ne_shp <- r9_shp[r9_shp$Group == "NE",]



{ # fig:
    states_map <- tm_shape(states) +
        tm_polygons() +
        tm_shape(ne_shp) +
        tm_polygons(fill = "red") +
        tm_layout(frame = FALSE)

    map <- tm_shape(eco_ne_shp) +
        tm_fill(
            fill = "NA_L2NAME",
            fill.scale = tm_scale_categorical(
                n.max = 3,
                c("#79cbd1", "#aacc66", "#beda8d")
            ),
            fill.legend = tm_legend(
                title = "Ecological Regions of North America (Level II)",
                position = tm_pos_in(pos.h = 0.6, pos.v = 0.4),
                frame = FALSE,
                text.size = 0.8
            )
        ) +
        tm_shape(ne_shp) +
        tm_borders() +
        tm_shape(fia_sf) +
        tm_dots(fill = "grey50", size = 0.2) +
        tm_compass(north = 325, size = 2, text.size = 1, position = c("left", "top")) +
        tm_scalebar(
            width = 8,
            position = c("left", "top"),
            text.size = 1, lwd = 2
        ) +
        tm_credits(
            paste("Number of sites:", format(nrow(fia_sf), big.mark = ",")),
            position = c("right", "bottom"), size = 1.2
        ) +
        tm_layout(
            frame = FALSE, outer.margins = c(0, 0, 0, 0.4)
        )

    png(
        file.path(outdir, "ne_fia_1993-1997_map.png"),
        width = 2200, height = 2800, res = 300
    )
    print(map)
    print(
        states_map,
        vp = grid::viewport(
            x = 0.5, y = 0.9,
            just = c("left", "top"),
            width = 0.4, height = 0.2
        )
    )
    dev.off()
}


