# ******************************************************************************
# Use some variables to match FIA plots that changed from Industrial to TIMO
# with plots that stayed as Industrial before doing causal analysis.
# 
# Author: Xiaojie Gao
# Date: 2026-04-29
# ******************************************************************************
rm(list = ls())
library(terra)
library(data.table)
library(magrittr)
library(MatchIt)
source("src/02_harvest_practices/hlp_fia_matching.R")



pipedir <- "pipe/02_harvest_practices"
dir.create(pipedir, showWarnings = FALSE, recursive = TRUE)


fia_harv_dt <- fread(file.path(pipedir, "fia_harvest_per_owner.csv"))
uniqueN(fia_harv_dt$concatPlot)
unique(fia_harv_dt[, MEASYEAR])


fia_dt <- fread("data/fia_attr_dt_allyear_maine.csv")[
    nsubplotsForest == 4, .(
        concatPlot, PLT_CN, MEASYEAR,
        AGC_live_Mgha = plotGroupAGCMgha_live,
        topSTDAGE
    )
]
uniqueN(fia_dt$concatPlot)

timestamps <- seq(2000, 2020, by = 5)



# ~ Count ####
# ~ ----------------------------------------------------------------------------
# Count how many Industrial and Investor plots are there during each 5 year
# period
CountInterestPlots <- function(fia_harv_dt) {
    count_dt <- NULL
    for (tstamp in c(2005, 2010, 2015, 2020)) {
        num_ind <- uniqueN(
            fia_harv_dt[
                MEASYEAR_aligned == tstamp &
                    ownertype_init == "Industrial" &
                    OwnerType == "Industrial" &
                    !is.na(AGC_live_Mgha),
                concatPlot
            ]
        )
        num_inv <- uniqueN(
            fia_harv_dt[
                MEASYEAR_aligned == tstamp &
                    ownertype_init == "REIT/TIMO" &
                    OwnerType == "REIT/TIMO" &
                    !is.na(AGC_live_Mgha),
                concatPlot
            ]
        )
        num_finan <- uniqueN(
            fia_harv_dt[
                MEASYEAR_aligned == tstamp &
                    ownertype_init == "Industrial" &
                    OwnerType == "REIT/TIMO" &
                    !is.na(AGC_live_Mgha),
                concatPlot
            ]
        )

        count_dt <- rbind(count_dt, data.table(
            period = paste(tstamp - 5, tstamp, sep = "_"),
            N_Industrial = num_ind,
            N_Investor = num_inv,
            N_Financial = num_finan
        ))
    }
    return(count_dt)
}

count_dt <- CountInterestPlots(fia_harv_dt)



# ~ Find stable ownership plots ####
# ~ ----------------------------------------------------------------------------
industrial_plots <- FindStablePlots("Industrial", fia_harv_dt)
uniqueN(industrial_plots$concatPlot)

investor_plots <- FindStablePlots("REIT/TIMO", fia_harv_dt)
uniqueN(investor_plots$concatPlot)



# ~ Find plots that experienced financilization ####
# ~ ----------------------------------------------------------------------------
finan_dt <- FindFinanPlot(fia_harv_dt)
uniqueN(finan_dt$concatPlot)
finan_dt[change_yrs == 0, table(MEASYEAR)]


# Stumpage prices
fia_plot_stumpage <- fread(file.path(pipedir, "fia_plot_stumpage.csv"))

# Road distance to mill
plot2mill_dist <- fread(file.path(pipedir, "plot2mill_dist.csv"))
# Align years
plot2mill_dist$MEASYEAR_aligned <- sapply(plot2mill_dist$MEASYEAR_aligned, \(yr) {
    timestamps[which.min(abs(timestamps - yr))]
})



# ~ Extract other covariates ####
# ~ ----------------------------------------------------------------------------
# To road distance, unit meters
to_road_dist_img <- rast("Y:/Plisinski/Maine_Landtrendr_2022/Matchit_Variables/Mean_Variables/roads_dist.img")

fia_vect <- vect(unique(fia_harv_dt[, .(concatPlot, x, y)]), geom = c("x", "y"), crs = "epsg:4326")
dist <- terra::extract(to_road_dist_img, project(fia_vect, to_road_dist_img), ID = FALSE)
fia_to_road_dist_dt <- cbind(as.data.table(fia_vect), to_road_dist = unlist(dist))


fia_harv_dt <- merge(fia_harv_dt, fia_to_road_dist_dt, by = "concatPlot")


# Stand age
# me_plot_table <- fread("Y:/FIA/rawFIA/ME_PLOT.csv")
# me_plot_table[, PLT_CN := paste0("X", CN)]
# me_plot_table <- me_plot_table[, .(PLT_CN, STDAGE)]
fia_harv_dt <- merge(fia_harv_dt, fia_dt[, .(PLT_CN, topSTDAGE)], by = "PLT_CN")


# ~ Make the table ####
# ~ ----------------------------------------------------------------------------
# For each plot, we need current and previouly measured variable values if
# possible 
fia_stable_owner <- fia_harv_dt[ownertype_init == OwnerType, ]

ExtractPlotYear <- function(plotid, plotyr) {
    plotyr_dt <- fia_stable_owner[
        concatPlot == plotid & MEASYEAR_aligned == plotyr,
    ]

    if (nrow(plotyr_dt) > 1) {
        # We have 11 plots that have duplicates, no big deal
        # So, I'll just use MEASYEAR to assign previous values
        setorder(plotyr_dt, MEASYEAR)
        if (nrow(plotyr_dt) > 2) {
            stop("Cannot be more than 2 measurements!")
        }
        # If one is harvested and the other doesn't, drop this record;
        # otherwise, average their values
        if (uniqueN(plotyr_dt[, ifharv]) > 1) {
            return(NULL)
        } else {
            plotyr_dt <- plotyr_dt[, .(
                concatPlot = concatPlot[1],
                OwnerType = OwnerType[1],
                MEASYEAR_aligned = MEASYEAR_aligned[1],
                ifharv = ifharv[1],
                BAremove_m2ha = mean(BAremove_m2ha),
                pc_ba_removed = mean(pc_ba_removed),
                to_road_dist = mean(to_road_dist),
                topSTDAGE = mean(topSTDAGE)
            )]
        }
    }
    if (plotyr_dt$MEASYEAR_aligned == 2000) {
        stop("Shouldn't be here...")
    }

    # Find previous values
    arow <- plotyr_dt[, 
        .(
            concatPlot = concatPlot,
            OwnerType = OwnerType,
            MEASYEAR_aligned = MEASYEAR_aligned,
            ifharv = ifharv,
            BAremove_m2ha = mean(BAremove_m2ha),
            pc_ba_removed = mean(pc_ba_removed),
            to_road_dist = mean(to_road_dist),
            topSTDAGE = mean(topSTDAGE)
        )
    ]
    # Previous site conditions
    prev_fia <- fia_harv_dt[
        concatPlot == arow$concatPlot & MEASYEAR_aligned == arow$MEASYEAR_aligned - 5,
    ]
    if (nrow(prev_fia) > 1) {
        # Go with the closest one
        setorder(prev_fia, MEASYEAR)
        arow <- cbind(arow, prev_fia[
            2,
            .(
                AGC_live_Mgha_prev = AGC_live_Mgha, BA_live_m2ha_prev = BA_live_m2ha,
                to_road_dist_prev = to_road_dist, topSTDAGE_prev = topSTDAGE
            )
        ])
    } else {
        arow <- cbind(arow, fia_harv_dt[
            concatPlot == arow$concatPlot & MEASYEAR_aligned == arow$MEASYEAR_aligned - 5,
            .(
                AGC_live_Mgha_prev = AGC_live_Mgha, BA_live_m2ha_prev = BA_live_m2ha,
                to_road_dist_prev = to_road_dist, topSTDAGE_prev = topSTDAGE
            )
        ])
    }
    # Previous stumpage price
    arow <- cbind(arow, fia_plot_stumpage[
        concatPlot == arow$concatPlot & MEASYEAR_aligned == arow$MEASYEAR_aligned - 5,
        .(stumpage_value_prev = mean(stumpage_value))
    ])
    # Previous to mill distance
    arow <- cbind(arow, plot2mill_dist[
        concatPlot == arow$concatPlot & MEASYEAR_aligned == arow$MEASYEAR_aligned - 5,
        .(shortest_dist_prev = mean(shortest_dist))
    ])

    return(arow)
}

unique_plotmeas <- unique(fia_stable_owner[, .(concatPlot, MEASYEAR_aligned)])
pb <- txtProgressBar(min = 0, max = 100, style = 3)
res_dt <- lapply(1:nrow(unique_plotmeas), \(i) {
    arow <- ExtractPlotYear(
        unique_plotmeas[i, concatPlot], 
        unique_plotmeas[i, MEASYEAR_aligned]
    )
    setTxtProgressBar(pb, i * 100 / nrow(unique_plotmeas)) # update progress

    return(arow)
})
close(pb)
res_dt <- rbindlist(res_dt)

res_dt[, period := paste0(MEASYEAR_aligned - 5, "-", MEASYEAR_aligned)]

# out: stable_owner_period_dt.csv
fwrite(res_dt, file.path(pipedir, "stable_owner_period_dt.csv"))
