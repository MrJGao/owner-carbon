# ******************************************************************************
# Extract Daymet climate for each ecoregion in Maine and save to a text file for
# LANDIS run later.
# 
# Author: Xiaojie Gao
# Date: 2025-03-04
# ******************************************************************************
rm(list = ls())
library(terra)
library(data.table)
library(parallel)



DoYear <- function(yr) {
    # Ecoregion image
    eco_img <- rast(eco_img_file)

    # Maine county
    ne_shp <- vect("Y:/DISES_ME/outputFolders/outputSpatial/impVactualMeanPlotAGCMghaNEWnewEngland.shp")
    maine_shp <- ne_shp[ne_shp$STATE == 23, ]


    # For each Daymet image, clip Maine region and summarize by each ecoregion
    vars <- c("tmax", "tmin", "prcp")
    eco_clim_dt <- lapply(vars, function(v) {
        var_file <- list.files(dm_dir, paste0(v, "_*.*", yr), full.names = TRUE)
        var_img <- rast(var_file)
        var_img <- project(var_img, eco_img)
        # plot(var_img[[1]])

        eco_clim <- zonal(var_img, eco_img, fun = "mean", na.rm = TRUE)
        setDT(eco_clim)
        eco_v_dt <- melt(
            eco_clim,
            id.vars = 1,
            measure.vars = patterns(paste0("^", v))
        )
        eco_v_dt[, Month := as.numeric(gsub(paste0(v, "_"), "", variable))]
        eco_v_dt <- eco_v_dt[, .(
            ecoregion_code = get(names(eco_v_dt)[1]),
            Year = yr,
            Month,
            value
        )]
        names(eco_v_dt)[which(names(eco_v_dt) == "value")] <- v
        
        return(eco_v_dt)
    })
    eco_clim_dt <- Reduce(
        f = function(x, y) { 
            merge(x, y, by = c("ecoregion_code", "Year", "Month")) 
        },
        x = eco_clim_dt
    )
    
    # out: 
    fwrite(
        eco_clim_dt, 
        file.path(pipedir, paste0("ecoclim_maine_", yr, ".csv"))
    )
}



# ~ Process ecoregions for Maine ####
# ~ ----------------------------------------------------------------------------
eco_img <- rast(
    file.path(
        "D:/LANDIS/inputFolders/SpatialData/originals",
        paste0(
            "landisEcoregions_Maine_notMasked2024-12-02_",
            200,
            ".tif"
        )
    )
)

# Copy this file into the folder for easier LANDIS use
# out:
writeRaster(
    eco_img, 
    file.path("pipe/timber_harvest", "Ecoregion_maine_200.tif"),
    overwrite = TRUE
)


# Drop the last two digits of the ecoregion code to get climate
eco_img_val <- values(eco_img)
eco_img_val <- sapply(eco_img_val, function(val) {
    if (is.na(val)) {
        return(NA)
    }
    substr(val, 1, nchar(val) - 2)
})
values(eco_img) <- eco_img_val
sort(unique(as.numeric(eco_img_val)))


# out: pipe/ecoregion_maine_clim_only_200.tif
eco_img_file <- "pipe/timber_harvest/ecoregion_maine_clim_only_200.tif"
writeRaster(eco_img, eco_img_file, overwrite = TRUE)



# ~ Process montly Daymet ####
# ~ ----------------------------------------------------------------------------
dm_dir <- "data/raw/NE_Daymet_month"

pipedir <- "pipe/ecoclimate_maine"
dir.create(pipedir, showWarnings = FALSE, recursive = TRUE)


cl <- makeCluster(20)
calls <- clusterCall(cl, function() {
    suppressWarnings({
        library(terra)
        library(data.table)
    })
})
clusterExport(cl, c("dm_dir", "pipedir", "eco_img_file"))
clusterApplyLB(cl, x = 1985:2023, fun = DoYear)
stopCluster(cl)



# ~ Output LANDIS format climate files ####
# ~ ----------------------------------------------------------------------------
# Use Danelle's PAR and CO2 values for now
maine_landis_clim_dir <- "D:/LANDIS/DISES_ME_agg200mMode/CCSM4_ecoregClimateFiles"

eco_clim_files <- list.files(pipedir, pattern = ".csv$", full.names = TRUE)
eco_clim_dt <- lapply(eco_clim_files, fread)
eco_clim_dt <- rbindlist(eco_clim_dt)


# Process and write out a file for each ecoregion
pipedir_me_owner <- "pipe/timber_harvest/ME_climate"
dir.create(pipedir_me_owner, showWarnings = FALSE, recursive = TRUE)
eco_clim_me_dt <- by(eco_clim_dt, eco_clim_dt$ecoregion_code, function(eco_dt) {
    eco_code <- unique(eco_dt$ecoregion_code)
    
    # In these climate files, PAR values are replicated each year, so I'll just
    # pick a year and replicate
    me_landis_clim_dt <- fread(file.path(
        maine_landis_clim_dir,
        paste0(eco_code, "_ecoreg_rcp85CCSM4_climate.txt")
    ))

    # Pend Daymet data to 1995-2023
    row1995 <- me_landis_clim_dt[Year == 1995, which = TRUE][1]
    me_landis_clim_dt_pre <- me_landis_clim_dt[1:(row1995 - 1), ]

    eco_dt <- merge(
        eco_dt, me_landis_clim_dt[Year == 1995, .(Month, PAR)], 
        by = c("Month")
    )

    # For CO2 values, I need both year and month
    eco_dt <- merge(
        eco_dt, 
        me_landis_clim_dt[
            Year %in% c(1995:2023), 
            .(Year = as.numeric(Year), Month, CO2)
        ],
        by = c("Year", "Month")
    )

    # Append a O3 column, values all 0, not use in LANDIS anyway
    eco_dt[, O3 := 0]

    me_landis_clim_dt_pre[, ecoregion_code := eco_code]
    setcolorder(eco_dt, c(1, 2, 4, 5, 7, 6, 8, 9, 3))
    colnames(me_landis_clim_dt_pre) <- colnames(eco_dt)
    eco_dt <- rbind(me_landis_clim_dt_pre, eco_dt)

    # HACK: Daymet data does not cover 2024 and 2025, so we simply replicate
    # 2023 to 2024 and 2025
    clim_2024 <- eco_dt[Year == 2023,]
    clim_2024$Year <- 2024
    eco_dt <- rbind(eco_dt, clim_2024)
    
    clim_2025 <- clim_2024
    clim_2025$Year <- 2025
    eco_dt <- rbind(eco_dt, clim_2025)


    # Rename columns and write out
    # out:
    write.table(
        eco_dt[, .(
            Year, Month, 
            Tmax = tmax, Tmin = tmin,
            PAR, Prec = prcp, CO2, O3
        )], 
        file.path(
            pipedir_me_owner, 
            paste0(eco_code, "_ecoreg_daymet_climate.txt")
        ),
        quote = FALSE,
        row.names = FALSE
    )

    return(eco_dt)
})
eco_clim_me_dt <- rbindlist(eco_clim_me_dt)



