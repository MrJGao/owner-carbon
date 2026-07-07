# ******************************************************************************
# Build a ownership time series for each FIA plot.
# 
# Author: Xiaojie Gao
# Date: 2026-05-06
# ******************************************************************************
rm(list = ls())
library(terra)
library(data.table)
library(magrittr)



pipedir <- "pipe/02_harvest_practices"
dir.create(pipedir, showWarnings = FALSE, recursive = TRUE)


fia_dt <- fread("data/fia_attr_dt_allyear_maine.csv")
fia_dt[!is.na(plotGroupBAm2ha_removed), .(plotGroupBAm2ha_removed)]
fia_dt <- fia_dt[nsubplotsForest == 4, ]

uniqueN(fia_dt$concatPlot)
table(fia_dt$MEASYEAR)



yrs <- c(2000, 2005, 2010, 2015, 2020)


pb <- txtProgressBar(min = 0, max = 100, style = 3)
fia_owner_dt <- lapply(yrs, \(yr) {
    # Read owner grid shpfiles
    # ! Assuming 2014 is 2015
    if (yr == 2015) {
        owner_grid_yr <- vect(
            file.path(
                "pipe/timber_harvest/owner_grid",
                paste0("owner_grid_", 2014, ".shp")
            )
        )
    } else {
        owner_grid_yr <- vect(
            file.path(
                "pipe/timber_harvest/owner_grid",
                paste0("owner_grid_", yr, ".shp")
            )
        )
    }

    fia_dt_yr <- fia_dt[
        nsubplotsForest == 4 & MEASYEAR %in% c((yr - 4):yr),
        .(
            concatPlot, PLT_CN, X, x, y,
            MEASYEAR,
            AGC_live_Mgha = plotGroupAGCMgha_live,
            BA_live_m2ha = plotGroupBAm2ha_live,
            BAremove_m2ha = plotGroupBAm2ha_removed,
            ifharv = ifelse(is.na(plotGroupBAm2ha_removed), 0, 1),
            pc_ba_removed = plotGroupBAm2ha_removed / (plotGroupBAm2ha_live + plotGroupBAm2ha_removed) * 100
        )
    ]

    fia_yr <- vect(fia_dt_yr, geom = c("x", "y"), crs = "epsg:4326") %>%
        project(owner_grid_yr)

    fia_yr_owner <- terra::extract(owner_grid_yr, fia_yr)
    fia_dt_yr[, OwnerType := fia_yr_owner$OwnerType]

    setTxtProgressBar(pb, which(yr == yrs) * 100 / length(yrs)) # update progress

    return(fia_dt_yr)
}) %>%
    rbindlist()

fia_owner_dt[is.na(OwnerType), OwnerType := "Unknown"]


timestamps <- seq(2000, 2020, by = 5)
fia_owner_dt$MEASYEAR_aligned <- sapply(fia_owner_dt$MEASYEAR, \(yr) {
    timestamps[which.min(abs(timestamps - yr))]
})


count_dt <- NULL
for (tstamp in c(2005, 2010, 2015, 2020)) {
    # tstamp <- 2005
    num_ind <- intersect(
        fia_owner_dt[
            MEASYEAR_aligned == tstamp - 5 & OwnerType == "Industrial" & !is.na(AGC_live_Mgha), 
            concatPlot
        ],
        fia_owner_dt[
            MEASYEAR_aligned == tstamp & OwnerType == "Industrial" & !is.na(AGC_live_Mgha), 
            concatPlot
        ]
    ) %>%
        length()

    num_inv <- intersect(
        fia_owner_dt[
            MEASYEAR_aligned == tstamp - 5 & OwnerType == "REIT/TIMO" & !is.na(AGC_live_Mgha),
            concatPlot
        ],
        fia_owner_dt[
            MEASYEAR_aligned == tstamp & OwnerType == "REIT/TIMO" & !is.na(AGC_live_Mgha),
            concatPlot
        ]
    ) %>%
        length()

    num_finan <- intersect(
        fia_owner_dt[
            MEASYEAR_aligned == tstamp - 5 & OwnerType == "Industrial" & !is.na(AGC_live_Mgha),
            concatPlot
        ],
        fia_owner_dt[
            MEASYEAR_aligned == tstamp & OwnerType == "REIT/TIMO" & !is.na(AGC_live_Mgha),
            concatPlot
        ]
    ) %>%
        length()

    count_dt <- rbind(count_dt, data.table(
        period = paste(tstamp - 5, tstamp, sep = "_"),
        N_Industrial = num_ind,
        N_Investor = num_inv,
        N_Financial = num_finan
    ))
}
count_dt



ind_stable_plots <- Reduce(intersect, x = list(
    fia_owner_dt[
        MEASYEAR_aligned == 2000 & OwnerType == "Industrial" & !is.na(AGC_live_Mgha),
        concatPlot
    ],
    fia_owner_dt[
        MEASYEAR_aligned == 2005 & OwnerType == "Industrial" & !is.na(AGC_live_Mgha),
        concatPlot
    ],
    fia_owner_dt[
        MEASYEAR_aligned == 2010 & OwnerType == "Industrial" & !is.na(AGC_live_Mgha),
        concatPlot
    ],
    fia_owner_dt[
        MEASYEAR_aligned == 2015 & OwnerType == "Industrial" & !is.na(AGC_live_Mgha),
        concatPlot
    ],
    fia_owner_dt[
        MEASYEAR_aligned == 2020 & OwnerType == "Industrial" & !is.na(AGC_live_Mgha),
        concatPlot
    ]
))
fia_owner_dt[concatPlot %in% ind_stable_plots,]
