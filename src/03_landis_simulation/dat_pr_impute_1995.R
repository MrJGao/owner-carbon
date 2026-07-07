# ******************************************************************************
# Impute 1995 init community
# 
# Author: Xiaojie Gao
# Date: 2025-02-18
# ******************************************************************************
rm(list = ls())
source("src/base.R")
library(data.table)
library(terra)
library(magrittr)
library(parallel)


# Put all imto this directory
pipedir <- "pipe/NE_1995_predictors"
dir.create(pipedir, showWarnings = FALSE, recursive = TRUE)



# ~ Merge predictors into a big map ####
# ~ ----------------------------------------------------------------------------
# Align predictor images and merge them
# I use a 200 m resolution

# ~ Harnomic features ----------------------------------------

cl <- makeCluster(24)
calls <- clusterCall(cl, function() {
    suppressWarnings({
        library(terra)
    })
})

# The harmonic features
har_fea_dir <- "data/raw/NE_harmonic"
imgfiles <- list.files(har_fea_dir, full.names = TRUE)

clusterExport(cl, c("pipedir"))
null <- clusterApplyLB(cl, x = imgfiles, fun = function(imgfilename) {
    # Use NE's projection
    ne_map <- rast("out/imputed_maps/ne_imp_mode_200.tif")

    img <- rast(imgfilename)
    img <- project(img, ne_map, mask = TRUE)

    # out:
    writeRaster(
        img, 
        filename = file.path(pipedir, basename(imgfilename)), 
        overwrite = TRUE
    )
})
stopCluster(cl)



# Merge tiles
rast_li <- list.files(pipedir, "tcg*.*.tif$", full.names = TRUE)

# Make a vrt
vrtfile <- file.path(pipedir, "NE_tcg_vrtfile.vrt")
if (!file.exists(vrtfile)) {
    vrt(unlist(rast_li), filename = vrtfile, overwrite = TRUE)
}

# Convert VRT to tif using the gdal command line tool
gdalcommand <- paste(
    "gdal_translate", vrtfile, file.path(pipedir, "NE_tcg_1993-1997.tif")
)

system(gdalcommand)



# ~ Daymet ----------------------------------------
# Climate
fea_dir <- "data/raw/maine_1995_imputation_predictors"
fea_dir2 <- "data/raw/ne_no_maine_1995_imputation_predictors"

# Calculate annual mean values for the variables. 
vars <- c("dayl", "prcp", "srad", "swe", "tmax", "tmin", "vp")

# Daymet use mean values
cl <- makeCluster(length(vars))
calls <- clusterCall(cl, function() {
    suppressWarnings({
        library(terra)
    })
})

clusterExport(cl, c("pipedir", "fea_dir", "fea_dir2"))
null <- clusterApplyLB(cl, x = vars, fun = function(v) {
    # Use NE's projection
    ne_map <- rast("out/imputed_maps/ne_imp_mode_200.tif")

    # Maine
    dm_imgfiles <- list.files(
        fea_dir, 
        paste0("DAYMET.004_", v, "*.*.tif$"), 
        full.names = TRUE
    )

    img <- rast(dm_imgfiles)
    # Calculate mean
    img <- mean(img, na.rm = TRUE)
    img <- project(img, ne_map, mask = TRUE)

    # out:
    writeRaster(
        img,
        filename = file.path(pipedir, paste0("DAYMET_1995_maine_", v, ".tif")),
        overwrite = TRUE
    )
    
    # NE w/o Maine
    dm_imgfiles <- list.files(
        fea_dir2, 
        paste0("DAYMET.004_", v, "*.*.tif$"), 
        full.names = TRUE
    )

    img <- rast(dm_imgfiles)
    # Calculate mean
    img <- mean(img, na.rm = TRUE)
    img <- project(img, ne_map, mask = TRUE)

    # out:
    writeRaster(
        img,
        filename = file.path(pipedir, paste0("DAYMET_1995_nomaine_", v, ".tif")),
        overwrite = TRUE
    )
})
stopCluster(cl)


# The Maine and NE-no_maine should be merged together
for (v in vars) {
    dm_maine <- rast(
        file.path(pipedir, paste0("DAYMET_1995_maine_", v, ".tif"))
    )
    dm_nomaine <- rast(
        file.path(pipedir, paste0("DAYMET_1995_nomaine_", v, ".tif"))
    )
    dm <- merge(dm_maine, dm_nomaine)
    
    # out:
    writeRaster(dm, file.path(pipedir, paste0("DAYMET_1995_NE_", v, ".tif")))
}



# ~ DEM ----------------------------------------

# Use NE's projection
ne_map <- rast("out/imputed_maps/ne_imp_mode_200.tif")

srtmfile1 <- list.files(fea_dir, "SRTMGL1_NC.003_SRTMGL1_DEM", full.names = TRUE)
srtm1 <- rast(srtmfile1)
srtm1 <- project(srtm1, ne_map, mask = TRUE)

srtmfile2 <- list.files(fea_dir2, "SRTMGL1_NC.003_SRTMGL1_DEM", full.names = TRUE)
srtm2 <- rast(srtmfile2)
srtm2 <- project(srtm2, ne_map, mask = TRUE)

# Merge them together
srtm <- merge(srtm1, srtm2)

writeRaster(
    srtm,
    filename = file.path(pipedir, "SRTM_DEM_NE.tif"),
    overwrite = TRUE
)
plot(srtm)



# ~ Stack layers together ----------------------------------------
har_img <- rast(file.path(pipedir, "NE_tcg_1993-1997.tif"))
dm_img <- rast(list.files(pipedir, "DAYMET_1995_NE", full.names = TRUE))
srtm_img <- rast(file.path(pipedir, "SRTM_DEM_NE.tif"))

lyrs <- c(har_img, dm_img, srtm_img)

# out:
writeRaster(
    lyrs, 
    filename = file.path(pipedir, "NE_predictors.tif"), 
    overwrite = TRUE
)



# ~ Extract predictor values for the FIA sites ####
# ~ ----------------------------------------------------------------------------
# FIA data from Danelle
fia_dt <- fread("Y:/FICE/MAINE/pre1999plotsActCoord.csv")
fia_dt[(!is.na(LAT_ACTUAL_NAD83) | !is.na(LON_ACTUAL_NAD83)) & STATE == "ME"]

# Remove PLT_CNs w/ no live trees
# fia_true <- fread("data/raw/pre2000plotCbyTPAUNADJ.csv")
fia_true <- fread("data/raw/pre2000plotCbyTPAUNADJ2025-08-12.csv")
# I only need unique plot-level values
fia_true <- unique(fia_true[, .(PLT_CN, MEASYEAR, plotMgCha)])

fia_dt <- fia_dt[PLT_CN %in% fia_true$PLT_CN, ]

# NOTE --------------------------------------------------------------------
# ! Here I use a table that was created by Danelle to re-impute the map
fia_sub_dt <- fread(
    "pipe/timber_harvest/tempForXiaojie_dises_1995_impmap_STDAGEplots.csv"
)
fia_dt <- fia_dt[PLT_CN %in% fia_sub_dt$PLT_CN]
# -------------------------------------------------------------------------


fia_vect <- vect(
    fia_dt[!is.na(LAT_ACTUAL_NAD83) | !is.na(LON_ACTUAL_NAD83)], 
    geom = c("LON_ACTUAL_NAD83", "LAT_ACTUAL_NAD83"),
    crs = "epsg:4269"
)
# plot(fia_vect)

feaimg <- rast(file.path(pipedir, "NE_predictors.tif"))
fia_vect_reporj <- project(fia_vect, crs(feaimg))
fia_features <- terra::extract(feaimg, fia_vect_reporj)
fia_fea_dt <- cbind(fia_vect_reporj$PLT_CN, fia_features)
setDT(fia_fea_dt)
fia_fea_dt$ID <- NULL

vars <- c("dayl", "prcp", "srad", "swe", "tmax", "tmin", "vp")
colnames(fia_fea_dt) <- c("PLT_CN", paste0("b", 1:8), vars, "dem")




# ~ Impute the map ####
# ~ ----------------------------------------------------------------------------
# 200-m imputation map
imp_maine_200 <- rast(file.path(
    "D:/LANDIS/inputFolders/SpatialData/originals",
    paste0("maine_imp_mode_", 200, ".tif")
))

# Crop Maine out
feaimg <- rast(file.path(pipedir, "NE_predictors.tif"))
feaimg <- project(feaimg, imp_maine_200, mask = TRUE)
feaimg <- crop(feaimg, imp_maine_200, mask = TRUE)


# Make a index column and sort by it to make it easier to interpret the kNN
# neighbors b/c the KNN neighbors from the FNN package are based on row number.
fia_fea_dt[, index := as.numeric(as.factor(PLT_CN))]
setorder(fia_fea_dt, index)

img_dt <- as.data.frame(feaimg, na.rm = FALSE)
# Find NA rows
na_idx <- lapply(1:ncol(img_dt), function(i) {
    which(is.na(img_dt[, i]))
}) %>%
    do.call(c, .) %>%
    unique()

img_dt <- na.omit(img_dt)

# Number of neighbors
k <- 10

pred_val <- FNN::knn(
    train = fia_fea_dt[, -c("PLT_CN", "index")],
    test = img_dt,
    cl = fia_fea_dt$PLT_CN,
    k = k,
    algorithm = "brute"
)
ngbs <- attr(pred_val, "nn.index")
res_img <- rast(feaimg, nlyr = k)
for (i in 1:nlyr(res_img)) {
    tmp <- rep(NA, ncell(res_img))
    if (length(na_idx) > 0) {
        tmp[-na_idx] <- ngbs[, i]
    } else {
        tmp <- ngbs[, i]
    }
    values(res_img[[i]]) <- tmp
}

# out: 
# writeRaster(
#     res_img, 
#     file.path("pipe/timber_harvest", "maine_imp_1995_200m.tif"),
#     overwrite = TRUE
# )
# fwrite(
#     fia_fea_dt[, .(level = index, name = PLT_CN)],
#     file.path("pipe/timber_harvest", "maine_imp_1995_200m.csv")
# )

writeRaster(
    res_img, 
    file.path("pipe/timber_harvest", "maine_imp_1995_200m_stdage.tif"),
    overwrite = TRUE
)
fwrite(
    fia_fea_dt[, .(level = index, name = PLT_CN)],
    file.path("pipe/timber_harvest", "maine_imp_1995_200m_stdage.csv")
)
