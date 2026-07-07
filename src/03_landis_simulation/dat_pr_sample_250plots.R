# ******************************************************************************
# Sample 250 plots w/ more than 20 years of measurements for LANDIS calibration.
# 
# Author: Xiaojie Gao
# Date: 2025-08-26
# ******************************************************************************
rm(list = ls())
library(data.table)
library(magrittr)
library(terra)



fia_maine <- fread("data/fia_attr_dt_allyear_maine.csv")
fia_maine <- fia_maine[, .(
    concatPlot, X, x, y, PLT_CN, INVYR, STATE, plotAreaForest,
    topDist,
    nsubplotsForest, MEASYEAR, plotGroupAGCMgha_live
)]

# Plots w/ more than 20 years of measurements
concatPlot_w_20 <- sapply(unique(fia_maine$concatPlot), function(cp) {
    subplot <- fia_maine[concatPlot == cp, ]
    rg <- max(subplot$MEASYEAR) - min(subplot$MEASYEAR)
    if (rg >= 20) {
        return(cp)
    } else {
        return(NULL)
    }
}) %>%
    do.call(c, .)

length(concatPlot_w_20)


set.seed(11)
sample_plots <- sample(concatPlot_w_20, 250)

fia_sub <- fia_maine[concatPlot %in% sample_plots,]

yrs <- seq(2000, 2020, by = 5)

mn_agb_mgha_allyrs_sub <- NULL
for (yr in yrs) {
    period <- (yr - 2):(yr + 2)
    # period <- yr
    biom_yr <- fia_sub[
        MEASYEAR %in% period,
        .(
            MEASYEAR,
            concatPlot,
            PLT_CN, X, x, y,
            agb_mgha = (plotGroupAGCMgha_live) * 2
        )
    ]

    mn_agb_mgha <- biom_yr[, mean(agb_mgha, na.rm = TRUE)]
    sd_agb_mgha <- biom_yr[, sd(agb_mgha, na.rm = TRUE)]
    sum_agb_mgha <- biom_yr[, sum(agb_mgha, na.rm = TRUE)]


    mn_agb_mgha_allyrs_sub <- rbind(mn_agb_mgha_allyrs_sub, data.table(
        yr, mn_agb_mgha, sd_agb_mgha, sum_agb_mgha,
        fia_count = nrow(biom_yr)
    ))
}
print(mn_agb_mgha_allyrs_sub)


# Append ecoregion code so that Danelle can calibrate LANDIS
fia_sub250 <- unique(fia_sub[, .(concatPlot, PLT_CN, x, y)])

eco_me <- rast(file.path(
    "pipe/timber_harvest",
    "maine_eco_map_200_align_impmap_250812.img"
))

fia_sub250_vect <- vect(fia_sub250, geom = c("x", "y"), crs = "EPSG:4326")
fia_sub250_vect <- project(fia_sub250_vect, eco_me)
fia_sub250_vect$ecoregion <- terra::extract(eco_me, fia_sub250_vect)[, 2]


# out: 
fwrite(
    as.data.table(fia_sub250_vect), 
    "pipe/timber_harvest/maine_250fia4landis_calib.csv"
)


fia_sub250 <- fread("pipe/timber_harvest/maine_250fia4landis_calib.csv")
fia_sub[MEASYEAR %in% c(1998:2002), ]

source("src/base.R")
fullwide <- fread(base$fia_fullwide_file)
fullwide <- fullwide[, .(concatPlot, PLT_CN, MEASYEAR, plotGroupAGCMgha_live)]

fullwide[concatPlot %in% fia_sub$concatPlot[1], ]
fia_sub[concatPlot == fia_sub$concatPlot[1], ]
fia_sub[concatPlot == c("23_5_21_665"),]


fwrite(fia_sub, file = "pipe/fia_sub250_with_attr.csv")
