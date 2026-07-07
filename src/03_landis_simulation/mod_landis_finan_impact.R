# ******************************************************************************
# The financialization impact on carbon. Compare the NoInd2TIMO scenario
# and the historical scenario. To put it into context, here we also compare
# other counter-factual scenarios including conservation.
# 
# Author: Xiaojie Gao
# Date: 2026-04-08
# ******************************************************************************
library(terra)
library(data.table)
library(magrittr)



ReadBiomassMap <- function(biomass_dir, timestep, crs, ext) {
    biom_rast <- tryCatch(
        {
            rast(
                file.path(
                    biomass_dir, paste0("AGBiomass_AllSpecies_", timestep, ".img")
                ),
                
            )
        },
        error = function(e) {
            print(e)
            return(NULL)
        }
    )
    if (is.null(biom_rast)) {
        return(NULL)
    }

    biom_rast <- flip(biom_rast)
    crs(biom_rast) <- crs
    ext(biom_rast) <- ext
    
    # Convert g/m2 to Mg/ha
    # biom_rast <- biom_rast * 0.01
    biom_rast <- biom_rast * 200^2 / 1e6

    return(biom_rast)
}


ReadOwnerMap <- function(manage_map_dir, manage_map_yr) {
    manage_map <- rast(
        file.path(
            manage_map_dir,
            paste0("owner_manage_", manage_map_yr, ".img")
        )
    )
    manage_mapcode <- fread("pipe/timber_harvest/owner_manage_mapcode.csv")
    levels(manage_map) <- manage_mapcode

    return(manage_map)
}


CompareScenToHistorical <- function(timesteps, scen_dir, hist_dir) {
    res_diff_carbon <- lapply(timesteps[-1], \(timestep) {
        # For each time step, find the region where Industrial -> TIMO
        scen_manage_rast <- ReadOwnerMap(
            manage_map_dir = file.path(scen_dir, "mgmtAreas"),
            timestep + 1995
        )
        hist_manage_rast <- ReadOwnerMap(
            manage_map_dir = file.path(hist_dir, "mgmtAreas"),
            timestep + 1995
        )

        # Make a mask
        diff_rast <- hist_manage_rast - scen_manage_rast

        # Total carbon across study region
        scen_biomass_rast <- ReadBiomassMap(
            biomass_dir = file.path(scen_dir, "output/agbiomass"),
            timestep = timestep,
            crs = crs(scen_manage_rast),
            ext = ext(scen_manage_rast)
        )
        scen_carbon_total <- global(scen_biomass_rast, "sum", na.rm = TRUE) / 2 / 1e6

        hist_biomass_rast <- ReadBiomassMap(
            biomass_dir = file.path(hist_dir, "output/agbiomass"),
            timestep = timestep,
            crs = crs(hist_manage_rast),
            ext = ext(hist_manage_rast)
        )
        hist_carbon_total <- global(hist_biomass_rast, "sum", na.rm = TRUE) / 2 / 1e6


        # Carbon difference
        diff_biom_rast <- hist_biomass_rast - scen_biomass_rast
        change_total <- global(diff_biom_rast, "sum", na.rm = TRUE) / 2 / 1e6
        delta_total_pct <- change_total / hist_carbon_total * 100

        # Mask affected regions
        scen_biomass_rast <- mask(scen_biomass_rast, diff_rast, maskvalues = 0)
        scen_biomass_rast[scen_biomass_rast == 0] <- NA

        hist_biomass_rast <- mask(hist_biomass_rast, diff_rast, maskvalues = 0)
        hist_biomass_rast[hist_biomass_rast == 0] <- NA

        change_biom_rast <- mask(diff_biom_rast, diff_rast, maskvalues = 0)
        change_biom_rast[change_biom_rast == 0] <- NA

        # Carbon in affected regions
        scen_carbon_direct <- global(scen_biomass_rast, "sum", na.rm = TRUE) / 2 / 1e6
        hist_carbon_direct <- global(hist_biomass_rast, "sum", na.rm = TRUE) / 2 / 1e6
        change_carbon_direct <- global(change_biom_rast, "sum", na.rm = TRUE) / 2 / 1e6
        delta_direct_pct <- change_carbon_direct / hist_carbon_direct * 100

        arow <- data.table(
            scen_carbon_total = as.numeric(scen_carbon_total), 
            hist_carbon_total = as.numeric(hist_carbon_total), 
            change_total = as.numeric(change_total), 
            delta_total_pct = as.numeric(delta_total_pct),
            scen_carbon_direct = as.numeric(scen_carbon_direct), 
            hist_carbon_direct = as.numeric(hist_carbon_direct), 
            change_carbon_direct = as.numeric(change_carbon_direct), 
            delta_direct_pct = as.numeric(delta_direct_pct)
        )

        return(arow)
    })

    res_diff_carbon_dt <- rbindlist(res_diff_carbon)

    res_diff_carbon_dt[, year := timesteps[-1] + 1995]
    setcolorder(res_diff_carbon_dt, "year")

    return(res_diff_carbon_dt)
}


landis_runs_dir <- "pipe/03_landis_simulation/landis_run/scenarios"

# LANDIS run time steps
timesteps <- seq(0, 30, by = 5)

noI2T_dir <- file.path(landis_runs_dir, "no_ind2timo")
hist_dir <- file.path(landis_runs_dir, "historical")
static_dir <- file.path(landis_runs_dir, "static_owner")
noT2NF_dir <- file.path(landis_runs_dir, "no_timo2newfamily")

conservNoHarv_dir <- file.path(landis_runs_dir, "ConservNoHarv")
conservParHarv_dir <- file.path(landis_runs_dir, "ConservParHarv")


# No Industrial->TIMO relative to historical
noI2T_carbon_diff_dt <- CompareScenToHistorical(timesteps, noI2T_dir, hist_dir)
# No ownership change relative to historical
static_carbon_diff_dt <- CompareScenToHistorical(timesteps, static_dir, hist_dir)
# No TIMO->New Family relative to historical
noT2NF_carbon_diff_dt <- CompareScenToHistorical(timesteps, noT2NF_dir, hist_dir)

# Conservation no harvest
conservNoHarv_carbon_diff_dt <- CompareScenToHistorical(timesteps, conservNoHarv_dir, hist_dir)
# Conservation partial harvest
conservParHarv_carbon_diff_dt <- CompareScenToHistorical(timesteps, conservParHarv_dir, hist_dir)



