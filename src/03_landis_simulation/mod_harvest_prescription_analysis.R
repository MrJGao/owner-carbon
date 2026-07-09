# ******************************************************************************
# Investigate the harvest intensity for each prescription in a landis simulation
# 
# Author: Xiaojie Gao
# Date: 2025-05-19
# ******************************************************************************
rm(list = ls())
library(terra)
library(data.table)
library(magrittr)



rootdir <- "D:/Laflower/"

scen_growonly <- "DISES_ME_agg200mPre1999noHarvest"
landis_growonly_dir <- file.path(rootdir, scen_growonly)

scen_harvest <- "DISES_ME_agg200mPre1999singleMgmtAreaHarvest"
landis_harvest_dir <- file.path(rootdir, scen_harvest)



# # B/c timber harvesting happened on time step 10, we use the harvested agb map
# # to subtract and divide the grow-only agb map to get the harvest intensity for
# # each pixel

# # ~ AGB maps ----------------------------------------
# agb_img_growonly <- list.files(
#     file.path(
#         landis_growonly_dir,
#         "output_Pre1999noHarvest", "agbiomass"
#     ), 
#     "AGBiomass_AllSpecies.*.img", 
#     full.names = TRUE
# )
# # Only use time step 10
# agb_img_growonly <- rast(agb_img_growonly[[2]])
# # plot(agb_img_growonly)
# agb_growonly <- as.data.frame(flip(agb_img_growonly))


# agb_img_harvest <- list.files(
#     file.path(
#         landis_harvest_dir,
#         "output_Pre1999singleMgmtAreaHarvest", "agbiomass"
#     ), 
#     "AGBiomass_AllSpecies.*.img", 
#     full.names = TRUE
# )
# # Only use time step 10
# agb_img_harvest <- rast(agb_img_harvest[[2]])
# # plot(agb_img_harvest)
# agb_harvest <- as.data.frame(flip(agb_img_harvest))


# cell_id <- rast(file.path(
#     rootdir, "inputFolders/SpatialData",
#     "cell_id.tif"
# )) %>%
#     as.data.frame()


# # Harvest img
# harv_img <- rast(file.path(
#     rootdir, "DISES_ME_agg200mPre1999singleMgmtAreaHarvest",
#     "output_Pre1999singleMgmtAreaHarvest", "harvest",
#     "harvest_biomass_removed_10.img"
# ))

# agb_harv <- as.data.frame(flip(harv_img))

# harv_log1 <- cbind(cell_id, agb_harv, agb_harvest, agb_growonly) %>%
#     setDT() %>%
#     set_colnames(c(
#         "Stand", "BiomassRemoved", "BiomassRemain", "BiomassGrowonly"
#     ))


# harv_log1[, totoal := BiomassRemoved + BiomassRemain]
# harv_log1[BiomassGrowonly > 0, ]
# #^^ Adding biomass remvoed and biomass remain does NOT equal to the grow only
# #^^ biomass, which means something is different in the two runs!



# ~ Try another way ####
# ~ ----------------------------------------------------------------------------
# Since we have harvest log table and the harvest image, we can just use them to
# calculate intensity

# AGB remain at time step 10
agb_10 <- rast(file.path(
    landis_harvest_dir, "output_Pre1999singleMgmtAreaHarvest", "agbiomass",
    "AGBiomass_AllSpecies_10.img"
))

# plot(agb_10)

# Cell ID
cell_id <- rast(file.path(
    rootdir, "inputFolders/SpatialData",
    "cell_id.tif"
))

# Harvest img
harv_img <- rast(file.path(
    rootdir, "DISES_ME_agg200mPre1999singleMgmtAreaHarvest",
    "output_Pre1999singleMgmtAreaHarvest", "harvest",
    "harvest_biomass_removed_10.img"
))


agb_10_dt <- data.table(
    Stand = values(cell_id), AGB_10 = values(flip(agb_10)),
    values(flip(harv_img))
) %>%
    set_colnames(c("Stand", "AGB_10", "AGB_Removed"))


# Harvest log table
harv_log <- fread(file.path(
    rootdir, "DISES_ME_agg200mPre1999singleMgmtAreaHarvest", 
    "output_Pre1999singleMgmtAreaHarvest", "harvest",
    "harvest_log.csv"
))
harv_log <- harv_log[, .(
    Time, ManagementArea, Prescription, Stand, StandAge, StandRank, 
    NumberOfSites, HarvestedSites, MgBiomassRemoved, MgBioRemovedPerDamagedHa,
    TotalCohortsPartialHarvest, TotalCohortsCompleteHarvest
)]
# Only need time step 10
harv_log <- harv_log[Time == 10, ]


harv_dt <- merge(harv_log, agb_10_dt, by = "Stand")

harv_dt[, harv_intensity := AGB_Removed / (AGB_Removed + AGB_10) * 100]

harv_dt[, .(
    avg_harv_intensity = mean(harv_intensity),
    med_harv_intensity = median(harv_intensity),
    min_harv_intensity = min(harv_intensity),
    max_harv_intensity = max(harv_intensity)
), by = "Prescription"]

harv_dt[
    harv_intensity > 90 & 
    !Prescription %in% c("ClearcutPlant", "ClearcutNatRegen"), 
]


