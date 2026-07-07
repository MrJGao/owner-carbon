# ******************************************************************************
# Make a map to show Maine's location in the USA
# 
# Author: Xiaojie Gao
# Date: 2025-05-06
# ******************************************************************************
rm(list = ls())
source("src/base.R")
library(osmdata)
library(terra)
library(magrittr)
library(sf)



outdir <- "out"
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)



us_states <- vect(
    file.path(
        base$imax_fia_proj_dir, "data/raw/us-state-boundaries.geojson"
    )
) %>%
    project("EPSG:5070")


svglite::svglite(
    file.path(outdir, "maine_overview_map.svg"),
    width = 7, height = 5, bg = adjustcolor("white", 0)
)
plot(
    us_states[!us_states$name %in% c(
        "United States Virgin Islands", "Alaska",
        "Commonwealth of the Northern Mariana Islands", "Puerto Rico",
        "American Samoa", "Hawaii", "Guam"
    )],
    col = "grey", 
    axes = FALSE
)
polys(us_states[us_states$name == "Maine"], col = "red")

dev.off()

