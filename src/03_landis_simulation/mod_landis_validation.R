# ******************************************************************************
# The goal is to match the LANDIS simulated biomass with the FIA-estimated values.
# 
# Author: Xiaojie Gao
# Date: 2025-11-04
# ******************************************************************************
rm(list = ls())
library(data.table)
library(magrittr)
library(terra)



CopyTemplateFiles <- function(
    scen_dir, manage_map_dir, landis_data_dir, 
    foln_adj = 1
) {
    # Clear existing dir
    unlink(scen_dir, recursive = TRUE, force = TRUE)
    # Create the directory
    dir.create(scen_dir, showWarnings = FALSE, recursive = TRUE)

    # Climate file
    file.copy(
        from = file.path(
            landis_data_dir, "inputFolders",
            "daymet_ecoregClimateFiles"
        ),
        to = scen_dir,
        recursive = TRUE
    )
    # Template files
    file.copy(
        from = list.files(
            file.path(landis_data_dir, "template_files"),
            full.names = TRUE
        ),
        to = scen_dir,
        recursive = TRUE
    )

    # Management maps
    mgmtarea_dir <- file.path(scen_dir, "mgmtAreas")
    dir.create(mgmtarea_dir, showWarnings = FALSE, recursive = TRUE)
    file.copy(
        from = list.files(manage_map_dir, full.names = TRUE),
        to = mgmtarea_dir,
        recursive = TRUE
    )

    # Note this `rootdir` is defined outside of the function
    workdir <- file.path(rootdir, scen_dir)

    # ----------------------------------------------------------------------------
    # Change nitrogen level
    dt <- fread(
        file.path(workdir, "species_PNET_ANP_20250812CalibrationCopyWithinRanges.txt"), 
        skip = 1
    )
    dt[, FolN := FolN * foln_adj]
    txt <- capture.output(write.table(dt, row.names = FALSE, sep = "\t", quote = FALSE))
    filecon <- file(file.path(workdir, "species_PNET_ANP_20250812CalibrationCopyWithinRanges.txt"))
    header <- readLines(filecon)[1]
    writeLines(c(header, txt), filecon)
    close(filecon)

    # Change the species param txt path
    filecon <- file(file.path(workdir, "PnET_succession_Land.txt"))
    txt <- readLines(filecon)
    theline <- grep("PnETSpeciesParameters", txt)
    txt[theline] <- 'PnETSpeciesParameters \"species_PNET_ANP_20250812CalibrationCopyWithinRanges.txt\"'
    writeLines(txt, filecon)
    close(filecon)
    
    # ----------------------------------------------------------------------------

    # Command to start LANDIS
    cmd <- paste0(
        'cmd /c start "', scen_name, '" /d ', normalizePath(workdir), " ; ",
        "RunIt.bat"
    )

    return(cmd)
}



# ------------------------------------------------------------------------------
# NOTE
rootdir <- "D:/Gao/projects/owner-carbon"
pipedir <- "pipe/03_landis_simulation/landis_run"
dir.create(pipedir, showWarnings = FALSE, recursive = TRUE)
# ------------------------------------------------------------------------------


# ~ Historical ####
# ~ ----------------------------------------------------------------------------
scen_name <- "N_orig"
# Copy essential files
cmd <- CopyTemplateFiles(
    scen_dir = file.path(pipedir, "historical_valid", scen_name),
    manage_map_dir = "pipe/03_landis_simulation/historical",
    landis_data_dir = pipedir,
    foln_adj = 1
)

system(cmd, wait = FALSE)



# ~ Reduce N by 10% ####
# ~ ----------------------------------------------------------------------------
scen_name <- "N_90"
cmd <- CopyTemplateFiles(
    scen_dir = file.path(pipedir, "historical_valid", scen_name),
    manage_map_dir = "pipe/03_landis_simulation/historical",
    landis_data_dir = pipedir,
    foln_adj = 0.9
)

system(cmd, wait = FALSE)



# ~ Reduce N by 20% ####
# ~ ----------------------------------------------------------------------------
scen_name <- "N_80"
cmd <- CopyTemplateFiles(
    scen_dir = file.path(pipedir, "historical_valid", scen_name),
    manage_map_dir = "pipe/03_landis_simulation/historical",
    landis_data_dir = pipedir,
    foln_adj = 0.8
)

system(cmd, wait = FALSE)



# ~ Reduce N by 30% ####
# ~ ----------------------------------------------------------------------------
scen_name <- "N_70"
cmd <- CopyTemplateFiles(
    scen_dir = file.path(pipedir, "historical_valid", scen_name),
    manage_map_dir = "pipe/03_landis_simulation/historical",
    landis_data_dir = pipedir,
    foln_adj = 0.7
)

system(cmd, wait = FALSE)




# ~ Vis ####
# ~ ----------------------------------------------------------------------------
scen_names <- list.dirs(
    file.path(pipedir, "historical_valid"), 
    recursive = FALSE, full.names = FALSE
)


scen_biom_dt <- lapply(scen_names, function(sn) {
    scen_dir <- file.path(pipedir, "historical_valid", sn)
    biomass_dir <- file.path(scen_dir, "output/agbiomass")
    
    biom_imgs <- list.files(biomass_dir, "AGBiomass_AllSpecies_")
    
    biom_dt <- lapply(biom_imgs, function(imgname) {
        biomimg <- rast(file.path(biomass_dir, imgname))
        # Convert g/m2 to Mg
        biom_rast_val <- values(biomimg) * 200^2 / 1e6
        biomass_sum_mg <- sum(biom_rast_val)

        timestep <- strsplit(
            tools::file_path_sans_ext(imgname), "_"
        )[[1]][3] %>%
            as.numeric()
        
        tmpdt <- data.table(
            scen = sn,
            timestep,
            yr = timestep + 1995,
            biomass_sum_mg, 
            OwnerType = "Total"
        )
    }) %>%
        rbindlist()

    setorder(biom_dt, "timestep")

    return(biom_dt)
}) %>%
    rbindlist()


# FIA biomass
source("src/base.R")
# Also need to source the `CalFIABiomPerOwner()` in
# `src/03_landis_simulation/mod_landis_run_summary.R` before running code below

fia_biom_dt <- lapply(seq(1995, 2020, by = 5), function(yr) {
    fia_biom_yr <- CalFIABiomPerOwner(
        fia_yr = yr,
        manage_map_dir = "pipe/timber_harvest",
        manage_map_yr = yr
    )
    return(fia_biom_yr)
}) %>%
    rbindlist()


# ~ Make the image ----------------------------------------
plot(
    NA, 
    xlim = range(scen_biom_dt$yr), 
    ylim = range(scen_biom_dt$biomass_sum_mg) / 1e6,
    mgp = c(1.5, 0.5, 0), bty = "L",
    xlab = "Timestep", ylab = "Biomass (1 Million Mg)"
)

snnames <- unique(scen_biom_dt$scen)
# snnames <- snnames[snnames != "N_orig"]
cols <- hcl.colors(length(snnames), "set 2")
for (i in seq_along(snnames)) {
    lines(
        scen_biom_dt[scen == snnames[i], .(yr, biomass_sum_mg / 1e6)], 
        type = "o", pch = 16, col = cols[i], lwd = 2
    )
}

# FIA biomass
lines(
    fia_biom_dt[OwnerType == "Total", .(yr, biomass_mn_sum_mg / 1e6)],
    type = "o", lwd = 2, pch = 16,
    col = "grey"
)

legend(
    "topleft", bty = "n",
    legend = c(snnames, "FIA biomass"), lty = 1, lwd = 2, col = c(cols, "grey")
)
