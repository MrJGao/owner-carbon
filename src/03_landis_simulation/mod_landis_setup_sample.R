# ******************************************************************************
# Setup and run LANDIS with sampled harvest rates
# 
# Author: Xiaojie Gao
# Date: 2026-07-10
# ******************************************************************************
rm(list = ls())
library(data.table)
source("src/03_landis_simulation/hlp_landis_apportion_rates.R")



# ------------------------------------------------------------------------------
# NOTE
rootdir <- "D:/Gao/projects/owner-carbon"
pipedir <- "pipe/03_landis_simulation/landis_run"
dir.create(pipedir, showWarnings = FALSE, recursive = TRUE)
# ------------------------------------------------------------------------------


ReadTxt <- function(txtpath) {
    filecon <- file(txtpath)
    txt <- readLines(filecon)
    close(filecon)

    return(txt)
}

WriteTxt <- function(txt, txtpath) {
    filecon <- file(txtpath)
    writeLines(txt, filecon)
    close(filecon)
}


SampleHarvestRates <- function() {
    # Read harvest rates and make samples from their distributions
    rates_dt <- fread("D:/Gao/projects/owner-carbon/pipe/02_harvest_practices/owner_harvest_rates_dt.csv")

    # Sample from the distribution
    rates_dt_sample <- rates_dt[,
        .(harv_prob = abs(rnorm(1, .SD$harv_prob, .SD$harv_prob_std))),
        by = "OwnerType"
    ]

    return(rates_dt_sample)
}


CopyTemplateFiles <- function(scen_dir, manage_map_dir, landis_data_dir, rates_dt_sample) {
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

    # Translate harvest rates to LANDIS format
    ConvRatesToPrescriptions(
        rates_dt_sample,
        outdir = file.path(scen_dir, "bioharvTxt")
    )
    # Also write the sampled rates out
    fwrite(rates_dt_sample, file.path(scen_dir, "harvest_rates_sample.csv"))

    # # Change the harvest txt paths
    # magic_harvest_txt <- ReadTxt(file.path(scen_dir, "MagicHarvest.txt"))
    # magic_harvest_txt <- gsub(
    #     "./bioharvTxt/", "./bioharvTxt_sample/", 
    #     magic_harvest_txt
    # )
    # WriteTxt(magic_harvest_txt, file.path(scen_dir, "MagicHarvest.txt"))


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

    # Command to start LANDIS
    cmd <- paste0(
        'cmd /c start "', scen_name, '" /d ', normalizePath(workdir), " ; ",
        "RunIt.bat"
    )

    return(cmd)
}



n_rep <- 97

for (i in n_rep:(n_rep + 3)) {
    scen_parent_dir <- file.path(pipedir, paste0("scenarios_sample_", i))
    dir.create(scen_parent_dir, showWarnings = FALSE, recursive = TRUE)
    # Sample rates
    rates_dt_sample <- SampleHarvestRates()


    # ~ Historical ####
    # ~ ----------------------------------------------------------------------------
    scen_name <- "historical"

    # Copy essential files
    cmd <- CopyTemplateFiles(
        scen_dir = file.path(scen_parent_dir, scen_name),
        manage_map_dir = "pipe/03_landis_simulation/historical",
        landis_data_dir = pipedir,
        rates_dt_sample = rates_dt_sample
    )

    system(cmd, wait = FALSE)


    # ~ Static owner ####
    # ~ ----------------------------------------------------------------------------
    scen_name <- "static_owner"

    # Copy essential files
    cmd <- CopyTemplateFiles(
        scen_dir = file.path(scen_parent_dir, scen_name),
        manage_map_dir = "pipe/03_landis_simulation/static_owner",
        landis_data_dir = pipedir,
        rates_dt_sample = rates_dt_sample
    )

    system(cmd, wait = FALSE)


    # ~ No Industrial -> TIMO ####
    # ~ ----------------------------------------------------------------------------
    scen_name <- "no_ind2timo"

    # Copy essential files
    cmd <- CopyTemplateFiles(
        scen_dir = file.path(scen_parent_dir, scen_name),
        manage_map_dir = "pipe/03_landis_simulation/no_ind2timo",
        landis_data_dir = pipedir,
        rates_dt_sample = rates_dt_sample
    )

    system(cmd, wait = FALSE)


    # ~ No TIMO -> New Family ####
    # ~ ----------------------------------------------------------------------------
    scen_name <- "no_timo2newfamily"

    # Copy essential files
    cmd <- CopyTemplateFiles(
        scen_dir = file.path(scen_parent_dir, scen_name),
        manage_map_dir = "pipe/03_landis_simulation/no_timo2newfamily",
        landis_data_dir = pipedir,
        rates_dt_sample = rates_dt_sample
    )

    system(cmd, wait = FALSE)

}