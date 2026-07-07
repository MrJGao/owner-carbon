# ******************************************************************************
# Check all ownership shapefiles and export a table w/ all names of companies
# and thier corresponding types.
# 
# Author: Xiaojie Gao
# Date: 2025-01-07
# ******************************************************************************
rm(list = ls())
library(data.table)
library(terra)



shpdir <- "data/maine_ownership_2024"
shpfiles <- list.files(shpdir, ".shp$", full.names = TRUE)

# Output dir
outdir <- file.path(shpdir, "clean")
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)



# ~ Clean the attributes ####
# ~ ----------------------------------------------------------------------------
# This table stores all unique company names and reference owner types
owner_dt <- NULL

# 1995 to 2010
for (yr in c(1995, 2000, 2005, 2010)) {
    owner_yr <- vect(file.path(shpdir, paste0("meown", substr(yr, 3, 4), ".shp")))
    
    owner_dt <- rbind(owner_dt, data.table(
        FullName = owner_yr$FULL_NAME,
        ref_class = owner_yr$CLASS,
        OwnerType = ""
    ))

    # Drop unused columns
    owner_yr <- owner_yr[, c(
        "OBJECTID", "FULL_NAME"
    )]
    names(owner_yr)[2] <- "FullName"

    writeVector(
        owner_yr, file.path(outdir, paste0("meown_", yr, ".shp")),
        overwrite = TRUE
    )
}

# 2014 and 2018
for (yr in c(2014, 2018)) {
    owner_yr <- vect(file.path(shpdir, paste0("ME", yr, ".shp")))
    owner_dt <- rbind(owner_dt, data.table(
        FullName = owner_yr$FULL_NAME,
        ref_class = "",
        OwnerType = ""
    ))
    if (!"OBJECTID" %in% names(owner_yr)) {
        owner_yr$OBJECTID <- 1:nrow(owner_yr)
    }
    # Drop unused columns
    owner_yr <- owner_yr[, c(
        "OBJECTID", "FULL_NAME"
    )]
    names(owner_yr)[2] <- "FullName"

    writeVector(
        owner_yr, file.path(outdir, paste0("meown_", yr, ".shp")),
        overwrite = TRUE
    )
}

# The 2019-2024 maps are in a different format
owner2019_2024 <- vect(file.path(shpdir, "meown24.shp"))

owner_dt <- rbind(owner_dt, data.table(
    FullName = owner2019_2024$FullName,
    ref_class = owner2019_2024$LandOwnCla,
    OwnerType = ""
))

for (yr in 19:24) {
    owner_yr <- owner2019_2024[, c(
        "OBJECTID", paste0("Owner", yr)
    )]
    names(owner_yr)[2] <- "FullName"

    writeVector(
        owner_yr, file.path(outdir, paste0("meown_20", yr, ".shp")),
        overwrite = TRUE
    )
}


# Remove duplicated values
owner_dt <- unique(owner_dt)


# ~ Fill out some init ownertypes for reference --------------------------------
# We have to check and categorize owners mannually!!

# But, according to Hagan 2005, these 14 owner types may serve as ref:
#   1. Contractor
#   2. Developer
#   3. Federal
#   4. Finacial Investors (e.g., Timber Investment Management Organizations)
#   5. Individual or Family
#   6. Industry
#   7. New Timber Baron
#   8. Non Profit
#   9. Old-line Family
#   10. Other
#   11. Public (state)
#   12. Real-estate Investment Trust (REIT)
#   13. Tribal
#   14. Various

# Here, I assign `ref_class` that's in the above categories to OwnerType
ownertypes <- c(
    "Contractor", "Developer", "Federal", "Finacial Investors",
    "Individual", "Industry", "New Timber Baron", "Non Profit",
    "Old-line Family", "Public", "Real-estate Investment Trust",
    "Tribal", "Various"
)
for (type in ownertypes) {
    owner_dt[
        grep(type, ref_class, ignore.case = TRUE),
        OwnerType := type
    ]
}


# out: owner_dt.csv
fwrite(owner_dt, file.path(outdir, "owner_dt.csv"))


