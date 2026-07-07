# ******************************************************************************
# Match financilized plots with control
# 
# Author: Xiaojie Gao
# Date: 2026-05-20
# ******************************************************************************
rm(list = ls())
library(MatchIt)
library(data.table)
library(terra)
source("src/02_harvest_practices/hlp_fia_matching.R")



LoadFinanData <- function(verbose = TRUE) {
    fia_harv_dt <- fread(file.path(pipedir, "fia_harvest_per_owner.csv"))

    # To road distance, unit meters
    to_road_dist_img <- rast("Y:/Plisinski/Maine_Landtrendr_2022/Matchit_Variables/Mean_Variables/roads_dist.img")

    fia_vect <- vect(unique(fia_harv_dt[, .(concatPlot, x, y)]), geom = c("x", "y"), crs = "epsg:4326")
    dist <- terra::extract(to_road_dist_img, project(fia_vect, to_road_dist_img), ID = FALSE)
    fia_to_road_dist_dt <- cbind(as.data.table(fia_vect), to_road_dist = unlist(dist))

    fia_harv_dt <- merge(fia_harv_dt, fia_to_road_dist_dt, by = "concatPlot")

    # Filter financialized plots
    finan_dt <- FindFinanPlot(fia_harv_dt)
    finan_dt <- fia_harv_dt[concatPlot %in% unique(finan_dt$concatPlot),]

    # Stumpage prices
    fia_plot_stumpage <- fread(file.path(pipedir, "fia_plot_stumpage.csv"))

    # Road distance to mill
    plot2mill_dist <- fread(file.path(pipedir, "plot2mill_dist.csv"))
    # Align years
    timestamps <- seq(2000, 2020, by = 5)
    plot2mill_dist$MEASYEAR_aligned <- sapply(plot2mill_dist$MEASYEAR_aligned, \(yr) {
        timestamps[which.min(abs(timestamps - yr))]
    })

        
    # Financialized plots
    finan_dt <- merge(
        finan_dt, fia_plot_stumpage,
        by = c("concatPlot", "MEASYEAR_aligned")
    )
    finan_dt <- merge(
        finan_dt, plot2mill_dist,
        by = c("concatPlot", "MEASYEAR_aligned")
    )
    if (verbose) {
        message("# of financialized plots: ", uniqueN(finan_dt$concatPlot))
    }


    # Stable plots
    industrial_plots <- FindStablePlots("Industrial", fia_harv_dt)
    industrial_plots <- merge(
        industrial_plots, fia_plot_stumpage,
        by = c("concatPlot", "MEASYEAR_aligned")
    )
    industrial_plots <- merge(
        industrial_plots, plot2mill_dist,
        by = c("concatPlot", "MEASYEAR_aligned")
    )
    if (verbose) {
        message("# of stable industrial plots: ", uniqueN(industrial_plots$concatPlot))
    }

    finan_dt[, iffinan := 1]
    industrial_plots[, iffinan := 0]
    res_dt <- rbind(finan_dt, industrial_plots)

    return(res_dt)
}


pipedir <- "pipe/02_harvest_practices"
dir.create(pipedir, showWarnings = FALSE, recursive = TRUE)


finan_dt <- LoadFinanData(verbose = TRUE)

# toyuqi_dt <- finan_dt[, .(
#     concatPlot, MEASYEAR_aligned, meas_gap, 
#     AGC_live_Mgha, BA_live_m2ha, BAremove_m2ha, ifharv, pc_ba_removed,
#     init_ownertype = ownertype_init, current_ownertype = OwnerType, 
#     stumpage_value, to_mill_dist = shortest_dist, to_road_dist
# )]

# fwrite(toyuqi_dt, file.path(pipedir, "financial_causal_data_toyuqi.csv"))



# match_dt <- fread(file.path(pipedir, "stable_owner_period_dt.csv"))
# match_dt <- match_dt[OwnerType %in% c("REIT/TIMO", "Industrial")]

# match_dt[, iffinan := ]

# 1:1 nearest neighbor matching with replacement on 
# the Mahalanobis distance
m_out <- matchit(
    iffinan ~ stumpage_value + shortest_dist + MEASYEAR_aligned,
    data = finan_dt,
    distance = "mahalanobis",
    replace = TRUE
)

summary(m_out)
