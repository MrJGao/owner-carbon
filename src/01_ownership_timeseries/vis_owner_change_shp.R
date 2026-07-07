# ******************************************************************************
# Investigate the ownership changes through time
# 
# Author: Xiaojie Gao
# Date: 2025-01-16
# ******************************************************************************
rm(list = ls())
source("src/base.R")
source("src/vis_owner_color.R")
library(data.table)
library(magrittr)
library(terra)
library(parallel)
library(readxl)


pipedir <- "pipe/01_ownership_timeseries"
dir.create(pipedir, showWarnings = FALSE, recursive = TRUE)



# ~ Process owner types ####
# ~ ----------------------------------------------------------------------------
# shpoutdir <- "data/maine_ownership_2024/clean_with_types"
shpoutdir <- "data/maine_ownership_2024/clean_with_types_bulter"
yrs <- c(1995, 2000, 2005, 2010, 2014, 2018, 2019:2024)
shplist <- lapply(yrs, function(yr) {
    shp <- vect(file.path(shpoutdir, paste0("meowntype_", yr, ".shp")))
    # Change REIT/TIMO to Investor
    shp[shp$OwnerType == "REIT/TIMO", ]$OwnerType <- "Investor"
    return(shp)
})


# Unique owner types
uni_owner <- sapply(shplist, function(shp) {
    unique(shp$OwnerType)
}) %>%
    c() %>%
    unique()



# ~ Make a time series map ####
# ~ ----------------------------------------------------------------------------
# Read Maine boundary
maine <- vect(base$region9_shpfile) %>%
    subset(.$NAME == "Maine") %>%
    project(shplist[[1]])

# Change REIT/TIMO to Investor
col_dt[OwnerType == "REIT/TIMO", OwnerType := "Investor"]


{ # fig: Ownership time series
    png(
        file.path(pipedir, "ownership_timeseries.png"),
        width = 2600, height = 1300, res = 200
    )
    par(mfrow = c(2, 6), oma = c(0, 0, 5, 0))
    for (i in 1:length(shplist)) {
        plot(maine,
            col = "grey70", mar = c(0, 0, 1.5, 0),
            axes = FALSE
        )
        plot(
            shplist[[i]],
            "OwnerType",
            border = NA,
            main = yrs[i],
            col = col_dt$color,
            sort = col_dt$OwnerType,
            add = TRUE,
            legend = FALSE
        )
    }
    legend(
        x = grconvertX(0.47, "ndc"), y = grconvertY(1, "ndc"),
        xjust = 0.45,
        bty = "n", xpd = NA,
        legend = col_dt$OwnerType,
        fill = col_dt$color,
        y.intersp = 0.8,
        ncol = 7,
        cex = 1.15
    )
    dev.off()
}


# Make a gif animation
animation::saveGIF(
    for (i in 1:length(shplist)) {
        plot(maine,
            col = "grey70", mar = c(0, 0, 2, 0),
            axes = FALSE
        )
        plot(
            shplist[[i]],
            "OwnerType",
            border = NA,
            col = col_dt$color,
            sort = col_dt$OwnerType,
            add = TRUE,
            legend = FALSE
        )
        # title(main = yrs[i], cex.main = 4, xpd = NA, line = -1.5)
        text(
            grconvertX(0.1, "ndc"), grconvertY(0.97, "ndc"),
            labels = yrs[i], cex = 4, xpd = NA, font = 2, pos = 4
        )
        legend(
            x = grconvertX(0.7, "ndc"), y = grconvertY(0.98, "ndc"),
            bty = "n", xpd = NA,
            legend = col_dt$OwnerType,
            fill = col_dt$color,
            cex = 2
        )
    },
    movie.name = "ownership_dynamics.gif", # It does not support folder path
    interval = 2,
    ani.width = 1200,
    ani.height = 1600,
    overwrite = TRUE
)



# ~ Make a stack barplot ####
# ~ ----------------------------------------------------------------------------
# Make the dataset
owner_areas_dt <- NULL
for (i in seq_along(uni_owner)) {
    owner <- uni_owner[i]
    # Find the owner polygons, sum their areas
    owner_areas <- sapply(shplist, function(shp) {
        owner_shp <- shp[shp$OwnerType == owner, ]
        owner_area <- sum(owner_shp$area)
        # Convert to km^2
        owner_area <- owner_area / 1e6

        return(owner_area)
    })
    owner_areas_dt <- rbind(owner_areas_dt, data.table(
        ownerType = owner,
        yr = yrs,
        area = owner_areas
    ))
}
# Maine total area in EPSG:5070
maine_forest_area <- base$maine_forest_ha * 0.01
# Calculate percentage
owner_areas_dt[, totalarea := maine_forest_area, by = yr]
owner_areas_dt[, pct := area / totalarea * 100, by = .(yr, ownerType)]

unknown_owner_dt <- owner_areas_dt[, .(pct = 100 - sum(pct)), by = yr]
unknown_owner_dt[, ":="(ownerType = "Unknown", area = 0, totalarea = 0)]
setcolorder(unknown_owner_dt, c(3, 1, 4, 5, 2))

owner_areas_dt <- rbind(owner_areas_dt, unknown_owner_dt)
setorder(owner_areas_dt, yr)


data_mat_dt <- dcast(owner_areas_dt, ownerType ~ yr, value.var = "pct")
data_mat <- as.matrix(data_mat_dt)[, -1]
rownames(data_mat) <- data_mat_dt[, ownerType]


cols <- col_dt[match(data_mat_dt[, ownerType], col_dt$OwnerType), color]

# Change REIT/TIMO to Investor
rownames(data_mat)[rownames(data_mat) == "REIT/TIMO"] <- "Investor"
col_dt[OwnerType == "REIT/TIMO", OwnerType := "Investor"]


# fig: Sum all areas for each onwer type
{
    svglite::svglite(
        file.path(pipedir, "owner_change_stack.svg"),
        width = 10, height = 5
    )
    par(mar = c(3, 3, 5, 1), mgp = c(1.5, 0.5, 0), cex = 1.2)
    barplot(
        data_mat,
        col = cols,
        border = "white",
        ylab = "Owner Type Area (%)"
    )
    legend(
        grconvertX(0.47, "ndc"), grconvertY(0.98, "ndc"),
        xjust = 0.4, x.intersp = 0.5,
        bty = "n", ncol = 4,
        legend = col_dt$OwnerType,
        fill = col_dt$color, border = NA,
        xpd = NA
    )
    dev.off()
}

