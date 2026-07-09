# ******************************************************************************
# Process owner grid time series for further analysis
# 
# Author: Xiaojie Gao
# Date: 2025-10-04
# ******************************************************************************
rm(list = ls())
source("src/base.R")
source("src/vis_owner_color.R")
library(data.table)
library(magrittr)
library(terra)
library(parallel)



pipedir <- "pipe/01_ownership_timeseries"
dir.create(pipedir, showWarnings = FALSE, recursive = TRUE)



# ~ Process data ####
# ~ ----------------------------------------------------------------------------
# Make a table to summarize the history of each grid ID
yrs <- c(1995, 2000, 2005, 2010, 2014, 2018, 2019:2024)

owner_grid_dt_file <- file.path(pipedir, "owner_grid_dt.csv")
if (!file.exists(owner_grid_dt_file)) {
    owner_grid_dt <- lapply(yrs, function(yr) {
        thegrid <- vect(
            file.path(
                pipedir, "owner_grid", 
                paste0("owner_grid_", yr, ".shp")
            )
        )
        thegrid_dt <- as.data.table(thegrid)[
            ,
            .(ID, Year = yr, OwnerType, FullName)
        ]

        return(thegrid_dt)
    }) %>%
        rbindlist()

    # out:
    fwrite(owner_grid_dt, file.path(pipedir, "owner_grid_dt.csv"))
}
owner_grid_dt <- fread(owner_grid_dt_file, na.strings = "")
owner_grid_dt <- setorder(owner_grid_dt, ID, Year)


# NOTE: there are NA values in some grids, I tried to fill them in w/ previous
# or post year records, however, I gave up b/c when it's NA, it's hard to
# determine whether it's b/c no data or the forest was just new due to land use
# change. So, instead, I change the table to wide format and removed rows that
# do not have a full record to quantify ownership changes.
ownergrid_wide_full <- dcast(
    owner_grid_dt, ID ~ Year,
    value.var = c("OwnerType", "FullName")
)
ownergrid_wide_nona <- na.omit(ownergrid_wide_full)

# So only use these IDs
owner_grid_dt <- owner_grid_dt[ID %in% ownergrid_wide_nona$ID, ]

ownertype_grid_nona <- ownergrid_wide_nona[, 
    .SD, 
    .SDcols = patterns("ID|OwnerType")
]


