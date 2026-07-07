# ******************************************************************************
# Pull down FIA tree table. 
# 
# Author: Xiaojie Gao copied from Danelle Laflower.
# Date: 2026-05-06
# ******************************************************************************
rm(list = ls())
library(terra)
library(sf)
library(data.table)
library(tidyverse)


# B/c Danelle's scripts change working directory, I need to save mine
wd <- getwd()


# This code script needs to be run on server 1 or 2. 
source("Y:/FIA_EDGE/Rcode/sourceFunctions.R")
source("Y:/LANDIS/Rcode/landis_source.R")

fiafilteryear <- 1999 # this filters INVYR

stateCodesToInclude <- c(23)
statesToInclude <- c("ME") # STATES
statedf <- bind_cols(stateCodesToInclude, statesToInclude)
names(statedf) <- c("stateCodesToInclude", "statesToInclude")
statecdcw <- read.csv("Y:/FIA/stateCodes.csv")

source("Y:/FIA/Rcode/fia_importAndProcess_notTrees_StatesToInclude.R")
source("Y:/FIA/Rcode/fia_importAndProcess_trees_StatesToInclude.R")


trees1 <- as.data.table(trees1)
unique(trees1$STATUSCD)

live_trees <- trees1[STATUSCD == 1, ]


# out: Maine FIA tree table
fwrite(
    live_trees, 
    file = file.path(
        "D:/Gao/projects/owner-carbon/data/raw", 
        "maine_fia_livetreetable.csv"
    )
)



# Set my working directory back
setwd(wd)

# FIA tree table
tree_dt <- fread("data/raw/maine_fia_livetreetable.csv")
# tree_dt <- fread("Y:/FIA/rawFIA/ME_TREE.csv")
tree_dt[, concatPlot := paste(STATECD, UNITCD, COUNTYCD, PLOT, sep = "_")]


# Filter to our financialized and non-financialized plots
tree_dt <- tree_dt[
    concatPlot %in% c(
        unique(fia_harv_dt$concatPlot)
    ),
]


# out: Maine_tree_table.csv
fwrite(tree_dt, file = "pipe/Maine_tree_table.csv")

