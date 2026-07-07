# ******************************************************************************
# Setup and run LANDIS simulations
# 
# Author: Xiaojie Gao
# Date: 2025-10-22
# ******************************************************************************
rm(list = ls())
library(data.table)


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


CopyTemplateFiles <- function(scen_dir, manage_map_dir, landis_data_dir) {
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
scen_name <- "historical"

# Copy essential files
cmd <- CopyTemplateFiles(
    scen_dir = file.path(pipedir, "scenarios", scen_name),
    manage_map_dir = "pipe/03_landis_simulation/historical",
    landis_data_dir = pipedir
)

system(cmd, wait = FALSE)



# ~ Static owner ####
# ~ ----------------------------------------------------------------------------
scen_name <- "static_owner"

# Copy essential files
cmd <- CopyTemplateFiles(
    scen_dir = file.path(pipedir, "scenarios", scen_name),
    manage_map_dir = "pipe/03_landis_simulation/static_owner",
    landis_data_dir = pipedir
)

system(cmd, wait = FALSE)



# ~ No Industrial -> TIMO ####
# ~ ----------------------------------------------------------------------------
scen_name <- "no_ind2timo"

# Copy essential files
cmd <- CopyTemplateFiles(
    scen_dir = file.path(pipedir, "scenarios", scen_name),
    manage_map_dir = "pipe/03_landis_simulation/no_ind2timo",
    landis_data_dir = pipedir
)

system(cmd, wait = FALSE)



# ~ No TIMO -> New Family ####
# ~ ----------------------------------------------------------------------------
scen_name <- "no_timo2newfamily"

# Copy essential files
cmd <- CopyTemplateFiles(
    scen_dir = file.path(pipedir, "scenarios", scen_name),
    manage_map_dir = "pipe/03_landis_simulation/no_timo2newfamily",
    landis_data_dir = pipedir
)

system(cmd, wait = FALSE)



# ~ Conservation No Harvest ####
# ~ ----------------------------------------------------------------------------
scen_name <- "ConservNoHarv"

# Copy essential files
cmd <- CopyTemplateFiles(
    scen_dir = file.path(pipedir, "scenarios", scen_name),
    manage_map_dir = "pipe/03_landis_simulation/ConservNoHarv",
    landis_data_dir = pipedir
)

system(cmd, wait = FALSE)



# ~ Conservation Partial Harvest ####
# ~ ----------------------------------------------------------------------------
scen_name <- "ConservParHarv"

# Copy essential files
cmd <- CopyTemplateFiles(
    scen_dir = file.path(pipedir, "scenarios", scen_name),
    manage_map_dir = "pipe/03_landis_simulation/ConservParHarv",
    landis_data_dir = pipedir
)

system(cmd, wait = FALSE)



# # ~ To Tribal ####
# # ~ ----------------------------------------------------------------------------
# scen_name <- "ind2tribal"

# # Copy essential files
# cmd <- CopyTemplateFiles(
#     scen_dir = file.path(pipedir, "scenarios", scen_name),
#     manage_map_dir = "pipe/03_landis_simulation/ind2tribal",
#     landis_data_dir = pipedir
# )

# system(cmd, wait = FALSE)

