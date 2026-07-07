# ******************************************************************************
# Create a harvest prescription txt file for LANDIS
# 
# Author: Xiaojie Gao
# Date: 2025-04-27
# ******************************************************************************
rm(list = ls())
library(data.table)



pipedir <- "pipe/timber_harvest"
dir.create(pipedir, showWarnings = FALSE, recursive = TRUE)



# ~ Text file header ####
# ~ ----------------------------------------------------------------------------
fileheader <- c(
    'LandisData  \"Biomass Harvest\"',
    'Timestep    5', 
    'ManagementAreas \"../inputFolders/SpatialData/owner_manage_6yrs.img\"',
    'Stands \"../inputFolders/SpatialData/cell_id.tif\"'
)



# ~ Prescriptions from Adam ####
# ~ ----------------------------------------------------------------------------
CreatePrescription <- function(
    pres_name, stand_ranking, min_age, max_age, min_time, 
    site_selection, cohorts_removed, additional = "", 
    single_repeat = NULL, plant = NULL
) {
    pres_params <- list(
        Prescription = pres_name,
        StandRanking = stand_ranking,
        MinimumAge = min_age,
        MaximumAge = max_age,
        MinimumTimeSinceLastHarvest = min_time,
        SiteSelection = site_selection,
        CohortsRemoved = cohorts_removed,
        addtional = additional,
        SingleRepeat = single_repeat,
        Plant = plant
    )

    return(pres_params)
}

# Convert the prescription object to text for writing to LANDIS files
ConvertPresObj2Txt <- function(pres_obj) {
    required_params <- paste(names(pres_obj)[1:7], pres_obj[1:7], sep = " ")
    additional <- apply(pres_obj$addtional, 1, function(arow) {
        paste(arow, collapse = "    ")
    })
    option_params <- lapply(9:length(pres_obj), function(i) {
        arow <- pres_obj[i]
        if (!is.null(unlist(arow))) {
            # If SingleRepeat, copy the species list again
            if (names(arow) == "SingleRepeat") {
                opt_txt <- paste(
                    names(arow),
                    arow,
                    sep = " "
                )
                coh_txt <- paste(names(pres_obj)[7], pres_obj[7], sep = " ")
                speclist <- apply(pres_obj$addtional, 1, function(arow) {
                    paste(arow, collapse = "    ")
                })
                return(c(opt_txt, coh_txt, speclist))
            }
            return(paste(
                names(arow),
                arow,
                sep = " "
            ))
        } else {
            return(NULL)
        }
    })
    option_params <- do.call(c, option_params)

    return(c(required_params, additional, option_params))
}

specnames <- c(
    "acersacc", "betualle", "fraxamer", "pinustro", "prunsero", "querrubr", 
    "queralba", "abiebals", "acerrubr", "betulent", "betupapy", "betupopu", 
    "caryglab", "fagugran", "fraxnigr", "larilari", "ostrvirg", "piceglau", 
    "picemari", "picerube", "pinuresi", "pinurigi", "popubals", "popugran", 
    "poputrem", "quercocc", "querprin", "quervelu", "thujocci", "tiliamer", 
    "tsugcana", "ulmuamer"
)


# Partial Harvest
PartialHarvest_pres <- CreatePrescription(
    pres_name = "PartialHarvest",
    stand_ranking = "Random",
    min_age = 50, max_age = 300, min_time = 0,
    site_selection = "PartialStandSpread 1 8",
    cohorts_removed = "SpeciesList",
    additional = data.table(
        spec = specnames, 
        removal = "1-30(50%) 31-500(50%)"
    ),
    single_repeat = 50
)
PartialHarvest_pres_txt <- ConvertPresObj2Txt(PartialHarvest_pres)


# Extended Rotation
ExtendedRotation_pres <- CreatePrescription(
    pres_name = "ExtendedRotation",
    stand_ranking = "MaxCohortAge",
    min_age = 50, max_age = 300, min_time = 0,
    site_selection = "PartialStandSpread 1 8",
    cohorts_removed = "SpeciesList",
    additional = data.table(
        spec = specnames,
        removal = "1-30(50%) 31-500(50%)"
    ),
    single_repeat = 60
)
ExtendedRotation_pres_txt <- ConvertPresObj2Txt(ExtendedRotation_pres)


# Clearcut w/ natural regeneration
ClearcutNatRegen_pres <- CreatePrescription(
    pres_name = "ClearcutNatRegen",
    stand_ranking = "Random",
    min_age = 50, max_age = 300, min_time = 50,
    site_selection = "PartialStandSpread 1 8",
    cohorts_removed = "SpeciesList",
    additional = data.table(
        spec = specnames,
        removal = "1-30(50%) 31-500(50%)"
    )
)
ClearcutNatRegen_pres_txt <- ConvertPresObj2Txt(ClearcutNatRegen_pres)


# Commercial thinning clear cut w/ planting
CommThinClearcutPlant_pres <- CreatePrescription(
    pres_name = "CommThinClearcutPlant",
    stand_ranking = "Random",
    min_age = 50, max_age = 300, min_time = 50,
    site_selection = "PartialStandSpread 1 8",
    cohorts_removed = "SpeciesList",
    additional = data.table(
        spec = specnames,
        removal = "1-30(50%) 31-500(50%)"
    ),
    single_repeat = 50
)
CommThinClearcutPlant_pres_txt <- ConvertPresObj2Txt(CommThinClearcutPlant_pres)


# ClearcutPlant
ClearcutPlant_pres <- CreatePrescription(
    pres_name = "ClearcutPlant",
    stand_ranking = "Random",
    min_age = 50, max_age = 300, min_time = 50,
    site_selection = "PartialStandSpread 1 8",
    cohorts_removed = "SpeciesList",
    additional = data.table(
        spec = specnames,
        removal = "1-30(50%) 31-500(50%)"
    ),
    single_repeat = 50
)
ClearcutPlant_pres_txt <- ConvertPresObj2Txt(ClearcutPlant_pres)


# RegShelterwood
RegShelterwood_pres <- CreatePrescription(
    pres_name = "RegShelterwood",
    stand_ranking = "Random",
    min_age = 50, max_age = 300, min_time = 50,
    site_selection = "PartialStandSpread 1 8",
    cohorts_removed = "SpeciesList",
    additional = data.table(
        spec = specnames,
        removal = "1-30(50%) 31-500(50%)"
    ),
    single_repeat = 50
)
RegShelterwood_pres_txt <- ConvertPresObj2Txt(RegShelterwood_pres)


# ContinuousCover
ContinuousCover_pres <- CreatePrescription(
    pres_name = "ContinuousCover",
    stand_ranking = "Random",
    min_age = 50, max_age = 300, min_time = 20,
    site_selection = "PartialStandSpread 1 8",
    cohorts_removed = "SpeciesList",
    additional = data.table(
        spec = specnames,
        removal = "1-30(50%) 31-500(50%)"
    ),
    single_repeat = 50
)
ContinuousCover_pres_txt <- ConvertPresObj2Txt(ContinuousCover_pres)



# ~ Implementation ####
# ~ ----------------------------------------------------------------------------
# Read the management area table for time series of owner changes
owner_yr_dt <- fread("pipe/timber_harvest/owner_manage_6yrs.csv")
# owner_yrs <- c(1995, 2000, 2005, 2010, 2014, 2018, 2019:2024)
owner_yrs <- c(1995, 2000, 2005, 2010, 2014, 2020)

# Read the timber harvest rate table
owner_harv_rate <- fread("pipe/timber_harvest/harvest_per_owner.csv")

# Combine no owner harvest rates
no_owner_harv_rate <- fread("pipe/timber_harvest/noowner_hrvt_rate_dt.csv")
no_owner_harv_rate$OwnerType <- "NA"
setcolorder(no_owner_harv_rate, c(3, 1, 2))
colnames(no_owner_harv_rate) <- colnames(owner_harv_rate)

no_owner_harv_rate <- no_owner_harv_rate[Year %in% unique(owner_harv_rate$Year),]

owner_harv_rate <- rbind(owner_harv_rate, no_owner_harv_rate)


# Here, b/c we use 5-year steps, assume 2014 is 2015; and b/c change detection
# does not have years beyond 2019, I assume 2020 and 2025 are just 2019
owner_harv_rate[Year == 2014, Year := 2015]
owner_harv_rate[Year == 2019, Year := 2020]
owner_harv_rate_2025 <- owner_harv_rate[Year == 2020, ][, Year := 2025]
owner_harv_rate <- rbind(owner_harv_rate, owner_harv_rate_2025)


# For each ownership change time series, make a line for the implementation of
# the timber harvest
imp_dt <- data.table(
    Mgmt_area = integer(), Prescription = "", Harvest_area_percent = "",
    Begin_year = integer(), End_year = integer()
)
for (i in 2:nrow(owner_yr_dt)) {
    owner_ts <- unlist(owner_yr_dt[i, ])
    index <- owner_ts[1]
    owner_ts_names <- strsplit(as.character(owner_ts[2]), ",")[[1]]
    owner_ts_dt <- data.table(cbind(owner_yrs, owner_ts_names))
    
    # Here, b/c we use 5-year steps, assume 2014 is 2015, 2024 to 2025
    owner_ts_dt[owner_yrs == 2014, owner_yrs := 2015]
    # owner_ts_dt[owner_yrs == 2024, owner_yrs := 2025]

    mgmt_imp_dt <- NULL
    # landis_yrs <- seq(1995, 2025, by = 5)
    landis_yrs <- seq(1995, 2020, by = 5)
    for (j in seq_along(landis_yrs)) {
        owner_type <- owner_ts_dt[owner_yrs == landis_yrs[j]][[2]]
        
        harv_rate <- owner_harv_rate[
            OwnerType == owner_type & 
            Year == landis_yrs[j], 
        ]
        harv_pct <- paste0(
            round(as.numeric(harv_rate[[3]]), 2),
            "%"
        )

        # TODO: there should be a prescription for each owner type
        pres <- switch(owner_type, 
            "Industrial" = "PartialHarvest", 
            "REIT/TIMO" = "ExtendedRotation", 
            "Non-Profit Conservation" = "PartialHarvest", 
            "Non-Profit Other" = "PartialHarvest", 
            "Contractor" = "ExtendedRotation", 
            "Old Family" = "PartialHarvest", 
            "Family Forest" = "PartialHarvest", 
            "New Family" = "ExtendedRotation",
            "Municipal" = "PartialHarvest", 
            "State" = "ExtendedRotation", 
            "Federal" = "PartialHarvest", 
            "Tribal" = "ExtendedRotation", 
            "Other" = "PartialHarvest",
            "ClearcutPlant"
        )

        mgmt_imp_dt <- rbind(mgmt_imp_dt, data.table(
            Mgmt_area = as.integer(index), 
            Prescription = pres, 
            Harvest_area_percent = harv_pct,
            Begin_year = landis_yrs[j] - 5 - 1990, 
            End_year = landis_yrs[j] - 1990
        ))
    }

    imp_dt <- rbind(imp_dt, mgmt_imp_dt)
}



# ~ Create the txt file and write ####
# ~ ----------------------------------------------------------------------------
bioharvRT <- file(file.path(pipedir, "bioharvRT_J.txt"), open = "w")

# Header
writeLines(text = fileheader, con = bioharvRT)
writeLines(text = rep("\n", 1), con = bioharvRT)


# Prescriptions
pres_header <- c(
    ">> PRESCRIPTIONS",
    ">> -----------------------------------------------------------------------"
)

writeLines(text = pres_header, con = bioharvRT)

writeLines(text = PartialHarvest_pres_txt, con = bioharvRT)
writeLines(text = rep("\n", 1), con = bioharvRT)

writeLines(text = ExtendedRotation_pres_txt, con = bioharvRT)
writeLines(text = rep("\n", 1), con = bioharvRT)

writeLines(text = ClearcutNatRegen_pres_txt, con = bioharvRT)
writeLines(text = rep("\n", 1), con = bioharvRT)

writeLines(text = CommThinClearcutPlant_pres_txt, con = bioharvRT)
writeLines(text = rep("\n", 1), con = bioharvRT)

writeLines(text = ClearcutPlant_pres_txt, con = bioharvRT)
writeLines(text = rep("\n", 1), con = bioharvRT)

writeLines(text = RegShelterwood_pres_txt, con = bioharvRT)
writeLines(text = rep("\n", 1), con = bioharvRT)

writeLines(text = ContinuousCover_pres_txt, con = bioharvRT)
writeLines(text = rep("\n", 1), con = bioharvRT)

writeLines(text = rep("\n", 2), con = bioharvRT)


# Implementations
imp_header <- c(
    ">> PRESCRIPTION IMPLEMENTATION",
    ">> -----------------------------------------------------------------------",
    "HarvestImplementations"
)

writeLines(text = c(imp_header), con = bioharvRT)

writeLines(
    text = paste("<<", paste(names(imp_dt), collapse = "    ")), 
    con = bioharvRT
)

imp_txt <- apply(imp_dt, 1, function(arow) {
    paste(arow, collapse = "  ")
})
writeLines(text = imp_txt, con = bioharvRT)

writeLines(text = rep("\n", 2), con = bioharvRT)

# Harvest outputs
harv_out_header <- c(
    ">>Harvest Outputs",
    ">> -----------------------------------------------------------------------"
)

harv_out_txt <- c(
    "PrescriptionMaps    output/harvest/harvest_prescripts_{timestep}.img",
    "BiomassMaps    output/harvest/harvest_biomass_removed_{timestep}.img",
    "EventLog    output/harvest/harvest_log.csv",
    "SummaryLog    output/harvest/harvest_summary_log.csv"
)

writeLines(text = c(harv_out_header, harv_out_txt), con = bioharvRT)

close(bioharvRT)


