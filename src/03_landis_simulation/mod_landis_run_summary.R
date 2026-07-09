# ******************************************************************************
# Summarize LANDIS run results
# 
# Author: Xiaojie Gao
# Date: 2025-07-22
# ******************************************************************************
source("src/base.R")
library(terra)
library(data.table)
library(magrittr)
library(tmap)



# Calculate FIA biomass per owner
CalFIABiomPerOwner <- function(fia_yr, manage_map_dir, manage_map_yr) {
    fia_maine <- fread("data/fia_attr_dt_allyear_maine.csv")
    manage_mapcode <- fread("pipe/timber_harvest/owner_manage_mapcode.csv")

    # No FIA out of this year range
    if (fia_yr < 1995 || fia_yr > 2020) {
        return(NULL)
    }

    fia_me_yr <- NULL
    if (fia_yr == 1995) {
        # FIA true measurements
        fia_true <- fread("data/raw/pre2000plotCbyTPAUNADJ.csv")
        # Remove tpaUnadjAdj==1, which is TPA_UNADJ=0 in the original dataset b/c we
        # don't know the plot designs.
        fia_true <- fia_true[tpaUnadjAdj != 1, ]

        fia_true <- unique(fia_true[, .(concatPlot, PLT_CN, MEASYEAR, plotMgCha)])

        # Subset by a table created by Danelle
        fia_fea_dt_sub <- fread(
            "pipe/timber_harvest/tempForXiaojie_dises_1995_impmap_STDAGEplots.csv"
        )
        fia_true <- fia_true[PLT_CN %in% fia_fea_dt_sub$PLT_CN]

        # FIA data from Danelle to get coordinates
        fia_dt <- fread("Y:/FICE/MAINE/pre1999plotsActCoord.csv")
        fia_true <- merge(
            fia_true,
            fia_dt[!is.na(LAT_ACTUAL_NAD83) | !is.na(LON_ACTUAL_NAD83)][
                ,
                .(PLT_CN, y = LAT_ACTUAL_NAD83, x = LON_ACTUAL_NAD83)
            ],
            by = "PLT_CN"
        )

        fia_vect <- vect(
            fia_dt[!is.na(LAT_ACTUAL_NAD83) | !is.na(LON_ACTUAL_NAD83)],
            geom = c("LON_ACTUAL_NAD83", "LAT_ACTUAL_NAD83"),
            crs = "epsg:4269"
        )
        fia_vect <- merge(
            fia_vect[, c("PLT_CN")], 
            fia_true, 
            by = "PLT_CN"
        )

        # Maine county
        maine_shp <- vect(base$me_county_shp)
        
        fia_vect_reporj <- project(fia_vect, maine_shp)
        fia_vect_extract <- terra::extract(maine_shp[, "NAME"], fia_vect_reporj)
        fia_vect_reporj$county_name <- fia_vect_extract$NAME
        fia_true_dt <- as.data.table(fia_vect_reporj)

        fia_true_me_dt <- fia_true_dt[!is.na(county_name), ]

        fia_true_me_dt[, agb_mgha := plotMgCha * 2]

        fia_me_yr <- fia_true_me_dt
    } else {
        period <- (fia_yr - 2):(fia_yr + 2)
        fia_me_yr <- fia_maine[
            MEASYEAR %in% period,
            .(
                MEASYEAR,
                PLT_CN, X, x, y,
                agb_mgha = (plotGroupAGCMgha_live) * 2
            )
        ]
    }
    
    avg_agb_mgha <- mean(fia_me_yr$agb_mgha, na.rm = TRUE)
    sd_agb_mgha <- sd(fia_me_yr$agb_mgha, na.rm = TRUE)
    med_agb_mgha <- median(fia_me_yr$agb_mgha, na.rm = TRUE)
    lwr_agb_mgha <- quantile(fia_me_yr$agb_mgha, 0.025, na.rm = TRUE)
    upr_agb_mgha <- quantile(fia_me_yr$agb_mgha, 0.975, na.rm = TRUE)
    fia_count <- nrow(fia_me_yr)

    tot_dt <- data.table(
        yr = fia_yr,
        avg_agb_mgha, sd_agb_mgha, med_agb_mgha, lwr_agb_mgha, upr_agb_mgha,
        fia_count,
        OwnerType = "Total"
    )

    # Read owner grid shpfiles
    # ! Assuming 2014 is 2015
    if (manage_map_yr == 2015) {
        manage_map <- rast(
            file.path(
                manage_map_dir,
                paste0("owner_manage_", 2014, ".img")
            )
        )
    } else {
        manage_map <- rast(
            file.path(
                manage_map_dir,
                paste0("owner_manage_", manage_map_yr, ".img")
            )
        )
    }

    fia_yr_vect <- vect(fia_me_yr, geom = c("x", "y"), crs = "epsg:4326") %>%
        project(manage_map)

    fia_yr_owner <- terra::extract(manage_map, fia_yr_vect)
    fia_me_yr[, index := fia_yr_owner$layer]
    fia_me_yr <- fia_me_yr[!is.na(index)]

    fia_yr_agb <- fia_me_yr[,
        .(
            yr = fia_yr,
            avg_agb_mgha = mean(agb_mgha, na.rm = TRUE),
            sd_agb_mgha = sd(agb_mgha, na.rm = TRUE),
            med_agb_mgha = median(agb_mgha, na.rm = TRUE),
            lwr_agb_mgha = quantile(agb_mgha, 0.025, na.rm = TRUE),
            upr_agb_mgha = quantile(agb_mgha, 0.975, na.rm = TRUE),
            fia_count = .N
        ),
        by = "index"
    ]

    fia_yr_agb <- merge(fia_yr_agb, manage_mapcode, by = "index")
    fia_yr_agb[, OwnerType := levels]

    fia_maine_biomass <- rbind(tot_dt, fia_yr_agb[, .SD, .SDcols = colnames(tot_dt)])


    # Imputed map to get total forest areas
    me_imp <- rast("D:/Laflower/inputFolders/SpatialData/maine_imp_1995_200m_stdage.tif")
    forest_map <- me_imp[[1]]
    forest_map[forest_map != 0 & !is.na(forest_map)] <- 1
    forest_map_ha <- forest_map * 200^2 * 0.0001
    # Total number of cells that are not NA
    me_total_ha <- sum(values(forest_map_ha), na.rm = TRUE)

    manage_map <- project(manage_map, forest_map_ha, method = "near", mask = TRUE)
    owner_forest <- zonal(forest_map_ha, manage_map, fun = "sum", na.rm = TRUE)
    setDT(owner_forest)
    owner_forest[, yr := fia_yr]
    colnames(owner_forest) <- c("index", "forest_area_ha", "yr")

    # Append total forest area
    owner_forest_area <- rbind(owner_forest, data.table(
        index = NA, forest_area_ha = me_total_ha, yr = fia_yr
    ))
    # ^^ The management map is 34732 ha less than the forest map, which was due
    # to the aggregation of the ownership map to 200 m resolution. But this
    # difference should not affect the overall comparison result.

    owner_forest_area <- merge(
        owner_forest_area,
        manage_mapcode,
        by = "index", all.x = TRUE
    )
    owner_forest_area[, OwnerType := levels]
    owner_forest_area[is.na(index), OwnerType := "Total"]

    # Multiply the two
    me_biomass <- merge(
        fia_maine_biomass, owner_forest_area,
        by = c("OwnerType", "yr")
    )
    me_biomass[, ":="(
        biomass_mn_sum_mg = avg_agb_mgha * forest_area_ha,
        biomass_mn_sd_mg = sd_agb_mgha * forest_area_ha,
        biomass_med_sum_mg = med_agb_mgha * forest_area_ha,
        biomass_upr_mg = upr_agb_mgha * forest_area_ha,
        biomass_lwr_mg = lwr_agb_mgha * forest_area_ha
    )]

    return(me_biomass)
}


# Calculate LANDIS out biomass per owner
CalBiomPerOwner <- function(
    biomass_dir, timestep, 
    manage_map_dir, manage_map_yr
) {
    biom_rast <- tryCatch({
        rast(
            file.path(
                biomass_dir, paste0("AGBiomass_AllSpecies_", timestep, ".img")
            )
        )
    }, error = function(e) {
        print(e)
        return(NULL)
    })
    if (is.null(biom_rast)) {
        return(NULL)
    }
    # Convert g/m2 to Mg
    biom_rast_val <- values(biom_rast) * 200^2 / 1e6
    biomass_sum_mg <- sum(biom_rast_val)

    # Convert g/m2 to Mg/ha
    biomass_mn_mgha <- unlist(global(biom_rast, "mean")) * 0.01

    res_dt <- data.table(
        timestep, 
        biomass_sum_mg, biomass_mn_mgha,
        OwnerType = "Total"
    )

    if (manage_map_yr == 1990) {
        # We don't have 1990 owner map
        res_dt[, yr := manage_map_yr]
        return(res_dt)
    }

    if (FALSE) {
    # if (manage_map_yr == 2015) {
        manage_map <- rast(
            file.path(
                manage_map_dir,
                paste0("owner_manage_", 2014, ".img")
            )
        )
    } else {
        manage_map <- rast(
            file.path(
                manage_map_dir,
                paste0("owner_manage_", manage_map_yr, ".img")
            )
        )
    }

    manage_mapcode <- fread("pipe/timber_harvest/owner_manage_mapcode.csv")

    
    crs(biom_rast) <- crs(manage_map)
    ext(biom_rast) <- ext(manage_map)
    owner_biomass_sum_mg <- zonal(biom_rast, manage_map, fun = "sum", na.rm = TRUE)
    owner_biomass_sum_mg[, 2] <- owner_biomass_sum_mg[, 2] * 200^2 / 1e6
    # formatC(owner_biomass_sum_mg[, 2], format = "e", digits = 2)
    owner_biomass_sum_mg <- merge(
        owner_biomass_sum_mg, manage_mapcode,
        by.x = "layer", by.y = "index"
    )
    setDT(owner_biomass_sum_mg)
    owner_biomass_sum_mg[, timestep := timestep]
    
    owner_biomass_mn_mgha <- zonal(biom_rast, manage_map, fun = "mean", na.rm = TRUE)
    owner_biomass_mn_mgha[, 2] <- owner_biomass_mn_mgha[, 2] * 0.01
    owner_biomass_mn_mgha <- merge(
        owner_biomass_mn_mgha, manage_mapcode,
        by.x = "layer", by.y = "index"
    )
    setDT(owner_biomass_mn_mgha)
    owner_biomass_mn_mgha[, timestep := timestep]
    
    com <- merge(
        owner_biomass_sum_mg[, .(timestep, biomass_sum_mg = Layer_1, OwnerType = levels)], 
        owner_biomass_mn_mgha[, .(timestep, biomass_mn_mgha = Layer_1, OwnerType = levels)], 
        by = c("timestep", "OwnerType")
    )

    res_dt <- rbind(res_dt, com)
    res_dt[, yr := manage_map_yr]

    return(res_dt)
}

# Calculate LANDIS harvest biomass per owner
CalHarvestPerOwner <- function(
    harvest_dir, timestep, 
    manage_map_dir, manage_map_yr
) {
    harv_rast <- tryCatch({
        rast(
            file.path(
                harvest_dir, paste0("harvest_biomass_removed_", timestep, ".img")
            )
        )
    }, error = function(e) {
        print(e)
        return(NULL)
    })
    if (is.null(harv_rast)) {
        return(NULL)
    }
    # Convert g/m2 to Mg
    harv_rast_val <- values(harv_rast) * 200^2 / 1e6
    harv_sum_mg <- sum(harv_rast_val)

    # Convert g/m2 to Mg/ha
    harv_mn_mgha <- unlist(global(harv_rast, "mean")) * 0.01

    res_dt <- data.table(
        timestep, 
        harv_sum_mg, harv_mn_mgha,
        OwnerType = "Total"
    )

    if (manage_map_yr == 1990) {
        # We don't have 1990 owner map
        res_dt[, yr := manage_map_yr]
        return(res_dt)
    }

    if (FALSE) {
    # if (manage_map_yr == 2015) {
        manage_map <- rast(
            file.path(
                manage_map_dir,
                paste0("owner_manage_", 2014, ".img")
            )
        )
    } else {
        manage_map <- rast(
            file.path(
                manage_map_dir,
                paste0("owner_manage_", manage_map_yr, ".img")
            )
        )
    }

    manage_mapcode <- fread("pipe/timber_harvest/owner_manage_mapcode.csv")

    
    crs(harv_rast) <- crs(manage_map)
    ext(harv_rast) <- ext(manage_map)
    owner_harv_sum_mg <- zonal(harv_rast, manage_map, fun = "sum", na.rm = TRUE)
    owner_harv_sum_mg[, 2] <- owner_harv_sum_mg[, 2] * 200^2 / 1e6
    # formatC(owner_harv_sum_mg[, 2], format = "e", digits = 2)
    owner_harv_sum_mg <- merge(
        owner_harv_sum_mg, manage_mapcode,
        by.x = "layer", by.y = "index"
    )
    setDT(owner_harv_sum_mg)
    owner_harv_sum_mg[, timestep := timestep]
    
    owner_harv_mn_mgha <- zonal(harv_rast, manage_map, fun = "mean", na.rm = TRUE)
    owner_harv_mn_mgha[, 2] <- owner_harv_mn_mgha[, 2] * 0.01
    owner_harv_mn_mgha <- merge(
        owner_harv_mn_mgha, manage_mapcode,
        by.x = "layer", by.y = "index"
    )
    setDT(owner_harv_mn_mgha)
    owner_harv_mn_mgha[, timestep := timestep]
    
    com <- merge(
        owner_harv_sum_mg[, .(timestep, harv_sum_mg = Layer_1, OwnerType = levels)], 
        owner_harv_mn_mgha[, .(timestep, harv_mn_mgha = Layer_1, OwnerType = levels)], 
        by = c("timestep", "OwnerType")
    )

    res_dt <- rbind(res_dt, com)
    res_dt[, yr := manage_map_yr]

    return(res_dt)
}



# ~ FIA-estimated biomass per owner ####
# ~ ----------------------------------------------------------------------------
fia_years <- seq(1995, 2020, by = 5)

# Orignal
fia_biom_dt <- lapply(fia_years, function(yr) {
    fia_biom_yr <- CalFIABiomPerOwner(
        fia_yr = yr, 
        manage_map_dir = "pipe/timber_harvest",
        manage_map_yr = yr 
    )
    return(fia_biom_yr)
}) %>%
    rbindlist()



# ~ LANDIS-simulated biomass per owner ####
# ~ ----------------------------------------------------------------------------
# LANDIS run time steps
timesteps <- seq(0, 30, by = 5)

landis_runs_dir <- "pipe/03_landis_simulation/landis_run/scenarios"

scen_names <- list.dirs(landis_runs_dir, full.names = FALSE, recursive = FALSE)
scen_runs <- lapply(scen_names, function(sn) {
    scen_dir <- file.path(landis_runs_dir, sn)

    scen_biomass <- lapply(timesteps, function(step) {
        owner_biomass <- CalBiomPerOwner(
            biomass_dir = file.path(scen_dir, "output/agbiomass"), 
            timestep = step,
            manage_map_dir = file.path(
                landis_runs_dir, sn, 
                "mgmtAreas"
            ),
            manage_map_yr = step + 1995
        )
        return(owner_biomass)
    }) %>%
        rbindlist()
    
    return(scen_biomass)
})

names(scen_runs) <- scen_names

scen_harvest <- lapply(scen_names, function(sn) {
    if (sn == "grow_only") {
        return(NULL)
    }
    scen_dir <- file.path(landis_runs_dir, sn)

    scen_harv <- lapply(timesteps[-1], function(step) {
        owner_harv <- CalHarvestPerOwner(
            harvest_dir = file.path(scen_dir, "output/harvest"), 
            timestep = step,
            manage_map_dir = file.path(
                landis_runs_dir, sn, 
                "mgmtAreas"
            ),
            manage_map_yr = step + 1995
        )
        return(owner_harv)
    }) %>%
        rbindlist()
    
    return(scen_harv)
})

names(scen_harvest) <- scen_names



# Biomass images -----------------
scen_imgs <- lapply(scen_names, function(sn) {
    scen_dir <- file.path(landis_runs_dir, sn)
    biomass_dir <- file.path(scen_dir, "output/agbiomass")
    biom_rast <- tryCatch(
        {
            final <- rast(
                file.path(
                    biomass_dir, paste0("AGBiomass_AllSpecies_", 30, ".img")
                )
            )
            NAflag(final) <- 0

            init <- rast(
                file.path(
                    biomass_dir, paste0("AGBiomass_AllSpecies_", 0, ".img")
                )
            )
            NAflag(init) <- 0

            change <- final - init
        },
        error = function(e) {
            print(e)
            return(NULL)
        }
    )
    if (is.null(biom_rast)) {
        return(NULL)
    }
    # Convert g/m2 to Mg/ha
    biom_rast <- biom_rast * 0.01

    biom_rast <- flip(biom_rast)
    
    # Mask out the small piece in the south
    mamg <- rast("pipe/timber_harvest/owner_manage_1995.img")
    crs(biom_rast) <- crs(mamg)
    ext(biom_rast) <- ext(mamg)
    biom_rast <- mask(biom_rast, mamg)

    return(biom_rast)
})
scen_imgs <- do.call(c, scen_imgs)
names(scen_imgs) <- scen_names


