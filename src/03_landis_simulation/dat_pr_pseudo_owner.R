# ******************************************************************************
# Create a series of sudo owner maps to simulate different use cases.
# 
# Author: Xiaojie Gao
# Date: 2025-07-22
# ******************************************************************************
rm(list = ls())
source("src/base.R")
source("src/vis_owner_color.R")
library(terra)
library(data.table)
library(magrittr)



pipedir <- "pipe/03_landis_simulation"


VisOwnerImgs <- function(imgdir, yrs, outpdf = "") {
    manage_mapcode <- fread("pipe/timber_harvest/owner_manage_mapcode.csv")
    # Reorder colors
    neworder_cols <- col_dt[match(manage_mapcode$levels, OwnerType), color]
    # Check whether this is effective
    pdf(outpdf, width = 10, height = 5)
    for (i in 2:length(yrs)) {
        prev_yr <- yrs[i - 1]
        this_yr <- yrs[i]

        # Assume 2014 is 2015, and 2024 is 2025
        # prev_yr <- ifelse(prev_yr == 2015, 2014, prev_yr)
        # prev_yr <- ifelse(prev_yr == 2025, 2024, prev_yr)
        this_yr <- ifelse(this_yr == 2015, 2014, this_yr)
        this_yr <- ifelse(this_yr == 2025, 2024, this_yr)

        prev_manage_map <- rast(file.path(
            imgdir, paste0("owner_manage_", prev_yr, ".img")
        ))

        this_manage_map <- rast(file.path(
            "pipe/timber_harvest", paste0("owner_manage_", this_yr, ".img")
        ))

        # Modified this year
        modified_this_manage_map <- rast(file.path(
            imgdir, paste0("owner_manage_", yrs[i], ".img")
        ))

        levels(prev_manage_map) <- manage_mapcode
        levels(this_manage_map) <- manage_mapcode
        levels(modified_this_manage_map) <- manage_mapcode

        par(mfrow = c(1, 3), mar = c(3, 3, 1, 1), bty = "n")
        plot(prev_manage_map, type = "classes", col = neworder_cols, main = prev_yr)
        plot(this_manage_map, type = "classes", col = neworder_cols, main = this_yr)

        plot(
            modified_this_manage_map, 
            type = "classes", col = neworder_cols, 
            main = this_yr
        )
    }
    dev.off()
}


MakePsudoOwnerMaps <- function(
    outdir, prev_code, this_code, psudo_code, still = FALSE,
    makeplot = TRUE
) {
    dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

    # The first year is just the same as the original management
    init_manage_map <- rast(file.path(
        "pipe/timber_harvest",
        paste0("owner_manage_", 1995, ".img")
    ))
    # out:
    writeRaster(
        init_manage_map,
        file.path(outdir, paste0("owner_manage_", 1995, ".img")),
        datatype = "INT2S", filetype = "HFA",
        overwrite = TRUE, NAflag = 0
    )


    # Starting from the second year, change prev_code->this_code to psudo_code
    yrs <- seq(1995, 2025, by = 5)
    for (i in 2:length(yrs)) {
        prev_yr <- yrs[i - 1]
        this_yr <- yrs[i]

        this_yr <- ifelse(this_yr == 2015, 2014, this_yr)
        this_yr <- ifelse(this_yr == 2025, 2024, this_yr)

        prev_manage_map <- rast(
            file.path(
                outdir,
                paste0("owner_manage_", prev_yr, ".img")
            )
        )

        this_manage_map <- rast(
            file.path(
                "pipe/timber_harvest",
                paste0("owner_manage_", this_yr, ".img")
            )
        )

        prev_manage_val <- values(prev_manage_map)
        this_manage_val <- values(this_manage_map)

        idx <- which(prev_manage_val == prev_code & this_manage_val == this_code)
        # Still means previous changed lands can change to other code but not this_code
        if (still) {
            idx <- c(
                idx, 
                which(prev_manage_val == psudo_code & this_manage_val == this_code)
            )
        }

        this_manage_val[idx] <- psudo_code
        values(this_manage_map) <- this_manage_val

        # out:
        writeRaster(
            this_manage_map,
            file.path(
                outdir,
                paste0("owner_manage_", yrs[i], ".img")
            ),
            datatype = "INT2S", filetype = "HFA",
            overwrite = TRUE, NAflag = 0
        )
    }


    if (makeplot) {
        VisOwnerImgs(
            outdir, yrs, 
            file.path(outdir, paste0(basename(outdir), ".pdf"))
        )
    }
}


(manage_mapcode <- fread("pipe/timber_harvest/owner_manage_mapcode.csv"))


# ~ What if there's no Industrial -> TIMO ####
# ~ ----------------------------------------------------------------------------
# Change TIMO pixels that are from Industrial back to Industrial
no_ind_timo_dir <- file.path(pipedir, "no_industrial_timo")
MakePsudoOwnerMaps(no_ind_timo_dir, 4, 11, 4, still = FALSE, makeplot = TRUE)



# ~ What if there's no TIMO -> New Family ####
# ~ ----------------------------------------------------------------------------
# Change New Family pixels that are from TIMO back to TIMO
no_timo2newfamily_dir <- file.path(pipedir, "no_timo2newfamily")
MakePsudoOwnerMaps(no_timo2newfamily_dir, 11, 6, 11, still = FALSE, makeplot = TRUE)



# ~ What if Industrial didn't change to TIMO but Tribal ####
# ~ ----------------------------------------------------------------------------
ind2tribal_dir <- file.path(pipedir, "ind2tribal")
# MakePsudoOwnerMaps(ind2tribal_dir, 4, 11, 13, still = TRUE, makeplot = TRUE)



# ~ What if Industrial didn't change to TIMO but Conservation and no harvest ####
# ~ ----------------------------------------------------------------------------
ind2connoharv <- file.path(pipedir, "ConservNoHarv")
MakePsudoOwnerMaps(ind2connoharv, 4, 11, 99, still = TRUE, makeplot = TRUE)



# ~ # ~ What if Industrial didn't change to TIMO but Non-Profit Conservation ####
# ~ ----------------------------------------------------------------------------
ind2npfc <- file.path(pipedir, "ConservParHarv")
MakePsudoOwnerMaps(ind2npfc, 4, 11, 7, still = TRUE, makeplot = TRUE)


