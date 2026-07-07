# ******************************************************************************
# Process the Maine Forest Report data to get annual harvest rates.
# 
# Author: Xiaojie Gao
# Date: 2025-11-15
# ******************************************************************************
library(readxl)
library(data.table)



woodprocess_dt <- read_xlsx("data/raw/woodprocess_summary.xlsx")
woodprocess_dt <- as.data.table(woodprocess_dt)
# Rename for easier reference
names(woodprocess_dt) <- c("Year", "tot_harv_cords", "tot_harv_greentons")

# Convert from green tons to Mg
woodprocess_dt[, harv_mg := tot_harv_greentons * 0.907]


silviculture_dt <- read_xlsx("data/raw/silviculture_summary.xlsx")
silviculture_dt <- as.data.table(silviculture_dt)
# Rename for easier reference
names(silviculture_dt) <- c(
    "Year", "owner_type", "commercial_harv_arce", "precom_harv_acre"
)

# Convert from Acres to Hectares
silviculture_dt[, harv_ha := commercial_harv_arce * 0.404686]


fr_dt <- merge(
    woodprocess_dt[, .(Year, harv_mg)],
    silviculture_dt[owner_type == "Total", .(Year, harv_ha)],
    by = "Year"
)
fr_dt[, harv_int := harv_mg / harv_ha]


# This value was calculated and used in the RS harvest rate analysis
maine_forest_area_ha <- 8258189
fr_dt[, harv_rate := harv_ha / maine_forest_area_ha * 100]


