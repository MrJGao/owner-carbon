# ******************************************************************************
# Base variables and functions
# 
# Author: Xiaojie Gao
# Date: 2025-07-21
# ******************************************************************************


# Github Gist for my common functions
source("https://gist.githubusercontent.com/MrJGao/7bd6f978771746fd27a999c9a5c808c6/raw/58d6f9b2fd052bcd5c824aa36450c907b4e912ba/gao_fun.R")

base <- list()

# The original big root project directory
base$imax_fia_proj_dir <- ""

base$fia_loc <- ""

# Maine ecoregion map
base$me_eco_map <- ""

# Maine ecoregion geojson
base$r9_eco_geojson <- file.path(base$imax_fia_proj_dir, "data/eco_r9.geojson")

# Maine county shpfile
base$me_county_shp <- file.path(
    "data/maine_county",
    "impVactualMeanPlotAGCMghaNEW.shp"
)

# Region 9 shpfile
base$region9_shpfile <- ""

# This is the server's path
base$fia_fullwide_file <- ""

# The unmasked NE ecoregion map
base$ne_eco_map <- ""


# Maine forest conservation
base$me_pos_shp <- ""


base$online_owner_table <- "https://docs.google.com/spreadsheets/d/1h_icRVHEQOjogbZmuJ28x2IrxHYq-DTa6Olpl_wJEIA/edit?gid=1524890947#gid=1524890947"