# ******************************************************************************
# Visualize LANDIS harvest per prescription per owner
# 
# Author: Xiaojie Gao
# Date: 2025-07-22
# ******************************************************************************
rm(list = ls())
library(data.table)
library(magrittr)
library(terra)



figdir <- "pipe/timber_harvest/landis_summary"
dir.create(figdir, showWarnings = FALSE, recursive = TRUE)

manage_mapcode <- fread("pipe/timber_harvest/owner_manage_mapcode.csv")


# Reproduce financialization
finan_harv <- file.path(
    "D:/Laflower/DISES_ME_agg200mPre1999magicHarvestSTDAGE",
    "output/harvest/harvest_log.csv"
) %>%
    fread() %>%
    .[, .(ManagementArea, Time, Prescription, MgBiomassRemoved, Stand)]

# Above ground biomass, after harvest
finan_dir <- file.path(
    "D:/Laflower/DISES_ME_agg200mPre1999magicHarvestSTDAGE",
    "output/agbiomass"
)

# For each time step, find the pixels' biomass after harvest
finan_harv_intensity <- lapply(unique(finan_harv$Time), function(yr) {
    biom_rast <- rast(
        file.path(finan_dir, paste0("AGBiomass_AllSpecies_", yr, ".img"))
    )
    # ! LANDIS output image is flipped 
    biom_rast <- flip(biom_rast)

    biom_rast_val <- values(biom_rast)
    # Convert to Mg
    biom_rast_val <- biom_rast_val / 1e6 * 200^2

    finan_harv[
        Time == yr, 
        after_harvest_biomass := biom_rast_val[finan_harv[Time == yr, Stand]]
    ]

    return(finan_harv)
}) %>%
    rbindlist()

# Calculate biomass harvest intensity
finan_harv_intensity[, intensity := MgBiomassRemoved / (MgBiomassRemoved + after_harvest_biomass) * 100]

finan_harv_scaled <- finan_harv_intensity[, .(
    meanIntensity = mean(intensity),
    sdIntensity = sd(intensity),
    medIntensity = median(intensity)
), by = .(ManagementArea, Prescription, Time)]

# Merge to get the ownership names
finan_harv_scaled <- finan_harv_scaled[manage_mapcode, on = "ManagementArea == index"]



# No finacialization, i.e., no owner change since 1995
stable_harv <- file.path(
    "D:/Laflower/DISES_ME_agg200mPre1999_static1995harvOwnersSTDAGE",
    "output/harvest/harvest_log.csv"
) %>%
    fread() %>%
    .[, .(ManagementArea, Time, Prescription, MgBiomassRemoved, Stand)]

    
stable_dir <- file.path(
    "D:/Laflower/DISES_ME_agg200mPre1999_static1995harvOwnersSTDAGE",
    "output/agbiomass"
)

stable_harv_intensity <- lapply(unique(stable_harv$Time), function(yr) {
    biom_rast <- rast(
        file.path(stable_dir, paste0("AGBiomass_AllSpecies_", yr, ".img"))
    )
    # ! LANDIS output image is flipped
    biom_rast <- flip(biom_rast)

    biom_rast_val <- values(biom_rast)
    # Convert to Mg
    biom_rast_val <- biom_rast_val / 1e6 * 200^2

    stable_harv[
        Time == yr,
        after_harvest_biomass := biom_rast_val[stable_harv[Time == yr, Stand]]
    ]

    return(stable_harv)
}) %>%
    rbindlist()

stable_harv_intensity[, intensity := MgBiomassRemoved / (MgBiomassRemoved + after_harvest_biomass) * 100]

stable_harv_scaled <- stable_harv_intensity[, .(
    meanIntensity = mean(intensity),
    sdIntensity = sd(intensity),
    medIntensity = median(intensity)
), by = .(ManagementArea, Prescription, Time)]

stable_harv_scaled <- stable_harv_scaled[manage_mapcode, on = "ManagementArea == index"]


pipedir <- "pipe/timber_harvest/landis_summary"
# out:
fwrite(finan_harv_scaled, file.path(pipedir, "finan_harv_scaled.csv"))
fwrite(stable_harv_scaled, file.path(pipedir, "stable_harv_scaled.csv"))



# ~ Make a figure ####
# ~ ----------------------------------------------------------------------------
{
    cols <- RColorBrewer::brewer.pal(8, "Set2")
    all_pres <- unique(finan_harv_scaled$Prescription)
    pres_col <- data.table(pres = all_pres, col = cols)

    svglite::svglite(
        file.path(figdir, "prescription_harvest_finan.svg")
    )
    par(mfrow = c(4, 4), mgp = c(1.5, 0.5, 0), mar = c(3, 3, 2, 3), bty = "L")

    owners <- unique(finan_harv_scaled$levels)
    for (ow in owners) {
        ow_harv <- finan_harv_scaled[levels == ow, ]
        pres <- unique(ow_harv$Prescription)
        plot(
            NA, xlim = c(5, 30), ylim = range(ow_harv$meanIntensity),
            xlab = "Time", ylab = "Biomass Harvested (Mg)", main = ow
        )
        for (i in seq_along(pres)) {
            lines(
                ow_harv[Prescription == pres[i], .(Time, meanIntensity)],
                type = "o", col = pres_col[pres[i] == pres, col]
            )
        }
    }

    legend(
        grconvertX(0.5, "ndc"), grconvertY(0.2, "ndc"),
        bty = "n",
        legend = pres_col$pres, col = pres_col$col, lty = 1,
        cex = 1.2,
        xpd = NA
    )

    dev.off()
}


{
    cols <- RColorBrewer::brewer.pal(11, "Set3")
    all_pres <- unique(stable_harv_scaled$Prescription)
    pres_col <- data.table(pres = all_pres, col = cols)

    svglite::svglite(
        file.path(figdir, "prescription_harvest_stable.svg")
    )
    par(mfrow = c(4, 4), mgp = c(1.5, 0.5, 0), mar = c(3, 3, 2, 3), bty = "L")

    owners <- unique(stable_harv_scaled$levels)
    for (ow in owners) {
        ow_harv <- stable_harv_scaled[levels == ow, ]
        pres <- unique(ow_harv$Prescription)
        plot(
            NA, xlim = c(5, 30), ylim = range(ow_harv$meanIntensity, na.rm = TRUE),
            xlab = "Time", ylab = "Biomass Harvested (Mg)", main = ow
        )
        for (i in seq_along(pres)) {
            lines(
                ow_harv[Prescription == pres[i], .(Time, meanIntensity)],
                type = "o", col = pres_col[pres[i] == pres, col]
            )
        }
    }

    legend(
        grconvertX(0.5, "ndc"), grconvertY(0.25, "ndc"),
        bty = "n",
        legend = pres_col$pres, col = pres_col$col, lty = 1,
        cex = 1.2,
        xpd = NA
    )

    dev.off()
}

