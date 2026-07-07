# ******************************************************************************
# Create a unique cell id raster from the management area map
# 
# Author: Xiaojie Gao
# Date: 2025-04-27
# ******************************************************************************
library(terra)


pipedir <- "pipe/03_landis_simulation"
dir.create(pipedir, showWarnings = FALSE, recursive = TRUE)


# Read the management map
ownermanage_img <- rast(file.path(pipedir, "owner_manage.tif"))


# Create a cell ID map
cellid_img <- rast(ownermanage_img)
values(cellid_img) <- 1:ncell(cellid_img)

# out: cell id map
writeRaster(
    cellid_img, 
    filename = file.path(pipedir, "cell_id.tif"), 
    datatype = "INT4U"
)

