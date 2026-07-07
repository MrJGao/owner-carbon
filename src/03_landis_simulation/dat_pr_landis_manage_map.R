# ******************************************************************************
# Make the management map based on the owenships for LANDIS simulation
# 
# Author: Xiaojie Gao
# Date: 2025-03-21
# ******************************************************************************
rm(list = ls())
library(terra)
library(data.table)
library(magrittr)
source("src/vis_owner_color.R")



pipedir <- "pipe/timber_harvest"
dir.create(pipedir, showWarnings = FALSE, recursive = TRUE)


# yrs <- c(1995, 2000, 2005, 2010, 2014, 2018, 2019:2024)
yrs <- c(1995, 2000, 2005, 2010, 2014, 2020, 2024)

owner_grid <- lapply(yrs, function(yr) {
    vect(file.path(pipedir, "owner_grid", paste0("owner_grid_", yr, ".shp")))
})


#! B/c now we have the Magic-Harvest succession, we may not need to collapse
# ! image stacks into a single-band image. I'll just output each owner type as a
# ! seperate image.


# Make a index table
owner_fact <- as.factor(col_dt$OwnerType)
mapcode_dt <- data.table(
    index = 1:length(owner_fact),
    levels = levels(owner_fact)
)
# out:
fwrite(mapcode_dt, file.path(pipedir, "owner_manage_mapcode.csv"))


null <- lapply(owner_grid, function(og) {
    # og$OwnerType
    # Rasterize the ownertype collapse index
    ot <- og$OwnerType
    ot[is.na(ot)] <- "Unknown"
    ind <- mapcode_dt[match(ot, levels), index]
    
    # Assign NA to Unknown, then use the ecoregion map to clip the raster
    owner_img <- rasterize(
        og,
        rast(
            res = 200,
            crs = crs(og),
            extent = ext(og)
        ),
        field = ind
    )

    imp_img <- rast(
        file.path(
            "pipe/timber_harvest",
            "maine_imp_1995_200m_stdage_align_ecomap_250812.tif"
        )
    )[[1]]
    rr <- project(owner_img, imp_img, method = "near")
    rr <- mask(rr, imp_img)

    yr <- substr(basename(sources(og)), 12, 15)

    # out: write out the cross-walk and the image
    writeRaster(
        rr, file.path(pipedir, paste0("owner_manage_", yr, ".img")),
        datatype = "INT2S", filetype = "HFA",
        overwrite = TRUE, NAflag = 0
    )
})



# # Make sure the images are correct
# pdf("zzz.pdf")
# null <- lapply(owner_grid, function(og) {
#     yr <- substr(basename(sources(og)), 12, 15)
#     rr <- rast(file.path(pipedir, paste0("owner_manage_", yr, ".img")))
#     plot(
#         rr,
#         col = col_dt[match(mapcode_dt$levels, OwnerType), color], 
#         sort = col_dt[match(mapcode_dt$levels, OwnerType), OwnerType],
#         main = yr
#     )
# })
# dev.off()



# Deprecated ------------------------------------------------------------------

# Owner type summary for all years
ownertype_collapse <- apply(ownertype_mat, 1, function(x) {
    if (all(is.na(x))) {
        return(NA)
    } else (
        return(paste(x, collapse = ","))
    )
})
# ownertype_collapse[is.na(ownertype_collapse) == FALSE]
uniqueN(ownertype_collapse)

# Make a cross-walk
ownertype_collapse_dt <- data.table(
    index = as.numeric(as.factor(ownertype_collapse)),
    ownertype_collapse = ownertype_collapse
)

# Rasterize the ownertype collapse index
owner_cells <- copy(owner_grid[[1]])
owner_cells$ownertype_collapse_index <- ownertype_collapse_dt$index
owner_img <- rasterize(
    owner_cells,
    rast(
        nrow = 1200, ncol = 800,
        crs = crs(owner_cells), extent = ext(owner_cells)
    ),
    field = "ownertype_collapse_index"
)

# fig:
pdf(file.path(pipedir, "owner_collapse_index.pdf"))
plot(owner_img, main = "owner_collapse_index")
dev.off()

# out: write out the cross-walk and the image
writeRaster(
    owner_img, file.path(pipedir, "owner_manage_6yrs.img"),
    datatype = "INT4S", filetype = "HFA",
    overwrite = TRUE
)
uni_ownertyp_collaspe <- unique(ownertype_collapse_dt) %>%
    setorder(index)
fwrite(uni_ownertyp_collaspe, file.path(pipedir, "owner_manage_6yrs.csv"))



# NO USE -----------------------------------------------------------------------
# Resample the ecoregion to match w/ current management map
rr <- rast(file.path(pipedir, "owner_manage_6yrs.img"))
dd <- rast("D:/LANDIS/inputFolders/SpatialData/eco_maine_imp_mode_200.img")

rr <- project(rr, dd, method = "near")
values(rr) <- 1

writeRaster(
    rr, file.path("D:/LANDIS/inputFolders/SpatialData/", "owner_manage_1value_200.img"),
    datatype = "INT2S", filetype = "HFA",
    overwrite = TRUE, NAflag = 0
)


# Create a cell ID map
cellid_img <- rast(rr)
values(cellid_img) <- 1:ncell(cellid_img)

# out: cell id map
writeRaster(
    cellid_img,
    filename = file.path("D:/LANDIS/inputFolders/SpatialData/", "cell_id.tif"),
    datatype = "INT4S", overwrite = TRUE
)
writeRaster(
    cellid_img, file.path("D:/LANDIS/inputFolders/SpatialData/", "cell_id.img"),
    overwrite = TRUE, datatype = "INT4S", filetype = "HFA"
)

