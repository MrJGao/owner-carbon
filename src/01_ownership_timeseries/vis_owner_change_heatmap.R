# ******************************************************************************
# Heatmap summarizing change frequency
# 
# Author: Xiaojie Gao
# Date: 2025-05-10
# ******************************************************************************
rm(list = ls())
source("src/base.R")
source("src/vis_owner_color.R")
library(data.table)
library(magrittr)
library(terra)
library(parallel)
library(readxl)



pipedir <- "pipe/01_ownership_timeseries"
dir.create(pipedir, showWarnings = FALSE, recursive = TRUE)



# ~ Change heatmap ####
# ~ ----------------------------------------------------------------------------
change_freq_raster_file <- file.path(pipedir, "ownertype_change_freq_raster.tif")
if (!file.exists(change_freq_raster_file)) {
    yrs <- c(1995, 2000, 2005, 2010, 2014, 2018, 2019:2024)

    # Make a heatmap to summarize change frequency for a piece of land

    # Read owner grid shpfiles back in
    hm_change <- lapply(yrs, function(yr) {
        vect(file.path(pipedir, "owner_grid", paste0("owner_grid_", yr, ".shp")))
    })

    # Make a table
    ch_mat <- lapply(hm_change, function(ch) {
        ch$OwnerType
    }) %>%
        do.call(cbind, .)

    hm_cells <- copy(hm_change[[1]][, c("ID")])
    hm_cells$change_times <- apply(ch_mat, 1, function(x) {
        if (all(is.na(x))) {
            return(NA)
        } else {
            return(length(unique(x)) - 1)
        }
    })

    hm_cell_rast <- rasterize(
        hm_cells,
        rast(extent = ext(hm_cells), res = 200, crs = crs(hm_cells)),
        field = "change_times"
    )
    # hm_cell_rast[hm_cell_rast == 1] <- NA

    # out: ownertype_change_freq_raster.tif
    writeRaster(hm_cell_rast, change_freq_raster_file)
}


hm_cell_rast <- rast(change_freq_raster_file)

# Change times of land areas
change_area <- NULL
for (i in unlist(unique(hm_cell_rast))) {
    change_times <- i
    area_change <- length(hm_cell_rast[hm_cell_rast == change_times]) *
        cellSize(hm_cell_rast, unit = "km")[1]
    area_change <- unlist(area_change)

    change_area <- rbind(change_area, data.table(
        change_times,
        area = area_change
    ))
}

change_area[, totalarea := sum(area)]
change_area[, pct := area / totalarea]
change_area

# At least changed once
change_area[change_times >= 1, .(sum(area), sum(pct))]
# At least changed three times
change_area[change_times >= 3, .(sum(area), sum(pct))]


# Read Maine boundary
maine <- vect(base$region9_shpfile) %>%
    subset(.$NAME == "Maine") %>%
    project(hm_cell_rast)


{ # fig: Number of owners per land
    png(
        file.path(pipedir, "num_owner_per_land.png"),
        width = 1200, height = 1500, res = 300
    )
    par(mar = c(1, 1, 1, 1), bg = NA)
    plot(
        maine,
        col = "grey70", mar = c(0, 0, 1.5, 0),
        border = NA,
        axes = FALSE
    )
    plot(
        hm_cell_rast,
        col = hcl.colors(256, "plasma"), type = "continuous",
        axes = FALSE,
        plg = list(digits = 0),
        add = TRUE
    )
    north("topleft", angle = -13, xpd = NA)

    # Add change area stat
    par(
        fig = c(0.6, 0.85, 0.1, 0.25), new = TRUE, mar = c(0, 0, 0, 0),
        mgp = c(0, -0.3, 0)
    )
    xloc <- barplot(
        change_area$area / 1e3,
        names.arg = change_area$change_times,
        border = NA,
        space = 0.1,
        xpd = NA,
        xaxt = "n", yaxt = "n", xlab = "", ylab = "",
        col = hcl.colors(length(change_area$change_times), "plasma"),
        horiz = TRUE
    )
    axis(
        side = 2, at = xloc, labels = unique(change_area$change_times),
        cex.axis = 0.5, tick = FALSE, las = 1,
        line = 0.5
    )
    mtext(
        side = 2,
        text = "Change times",
        cex = 0.5, line = 0.5
    )
    axis(side = 1, tck = -0.02, cex.axis = 0.5, las = 1)
    mtext(
        side = 1,
        text = expression(Area ~ (1000 ~ km^2)),
        cex = 0.5, line = 0.5
    )

    dev.off()
}
