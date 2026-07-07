# This script translates harvest rates into LANDIS prescriptions.
# This script is copied and modified from Danelle.


rm(list = ls())
library(tidyverse)
library(dplyr)
library(tidyr)
library(purrr)
library(readxl)
library(stringr)


# source("D:/Laflower/source_MEagg200mPre1999stdage.R")
# last run/updated 11/17/2025
xiaojieHPPath <- "pipe/02_harvest_practices"

output_file <- "pipe/03_landis_simulation/landis_run/magicHarvestDynamicRunImpTable.csv"

# 1 pull in rates and fill in 2020 and 2025
hrlf <- list.files(path = xiaojieHPPath, pattern = "harvest_rate_.*.csv", full.names = TRUE)
hrlf <- hrlf[-6] # exclude 2019

wOwn <- map_dfr(.x = set_names(hrlf), .f = read.csv, .id = "source_file") %>%
    dplyr::select(source_file, OwnerType, harvest_rate_corr) %>%
    mutate(source_file_org = basename(source_file)) %>%
    separate(source_file_org, c(NA, NA, "yrs", NA)) %>%
    mutate(yrs = as.integer(yrs)) %>%
    # bin
    mutate(year = ifelse(yrs <= 1995, 1995,
        ifelse(yrs <= 2000, 2000,
            ifelse(yrs <= 2005, 2005,
                ifelse(yrs <= 2010, 2010,
                    ifelse(yrs <= 2015, 2015,
                        ifelse(yrs <= 2020, 2020)
                    )
                )
            )
        )
    )) %>%
    filter(year != 1995) %>%
    dplyr::select(-yrs) %>%
    rename(harvestRate5yr = harvest_rate_corr)
#
rate2020 <- wOwn %>%
    filter(year == 2015) %>%
    mutate(year = 2020)
rate2025 <- wOwn %>%
    filter(year == 2015) %>%
    mutate(year = 2025)
#
ownOut <- wOwn %>%
    bind_rows(rate2020, rate2025) %>%
    mutate(harvestRate5yr = round(harvestRate5yr, 2))


# DEBUG ========================================================================
ownOut <- read.csv("pipe/02_harvest_practices/owner_harvest_rates_fia.csv") %>%
    mutate(year = yr, harvestRate5yr = harv_rates)
ownOut[ownOut$OwnerType == "Federal",]$harvestRate5yr <- 2
ownOut[ownOut$OwnerType == "Municipal",]$harvestRate5yr <- 3

fake2000 <- ownOut[ownOut$year == 2005, ]
fake2000$year <- 2000
ownOut <- rbind(fake2000, ownOut)

fake2025 <- ownOut[ownOut$year == 2020, ]
fake2025$year <- 2025
ownOut <- rbind(ownOut, fake2025)

ownOut <- dplyr::select(ownOut, OwnerType, harvestRate5yr, year)
ownOut[ownOut$harvestRate5yr <= 0, ]$harvestRate5yr <- 0.01
# ============== delete above ==================================================



# 2 pull in intensities, join rates, and apportion rates to Rx.  Create csv to use with bioharv txt file
ownIntensityRxSuite <- read_excel("Y:/DISES_ME/harvestPrep/ownerIntensityRxSuites.xlsx", sheet = "intensityWorksheet") # row 2 is the mean Rx intensity
# ownOut are the owner and no owner rates calculated by Xiaojie
#
ownHarv5yrOut <- ownOut %>%
    mutate(
        annualHarvRate = round(harvestRate5yr / 5, 3),
        OwnerType = case_when(OwnerType == "Unknown" ~ "NoOwner", .default = OwnerType),
        ownerName = str_replace_all(OwnerType, " ", "")
    ) %>%
    dplyr::select(-OwnerType)

# write out implementation table to be added to bioharv text file

ownIntensityRxSuite %>% # this is the excel file with the rx proportions by management area
    filter(!is.na(owncd)) %>%
    select(owncd,
        ownerName = OwnerType,
        PartialHarvest,
        ExtendedRotation,
        CommThinClearcutPlant,
        ContinuousCover,
        RegShelterwood,
        ClearcutNatRegen,
        ClearcutPlant,
        repeatCommThin,
        Incidental
    ) %>%
    left_join(ownHarv5yrOut %>% select(ownerName, harvestRate5yr, year)) %>% # these are the management area rates
    pivot_longer(names_to = "Prescription", values_to = "pctOfTargArea", PartialHarvest:Incidental) %>%
    replace_na(list(pctOfTargArea = 0)) %>%
    filter(pctOfTargArea > 0) %>%
    mutate(rxPctOfTargArea = round((pctOfTargArea / 100) * harvestRate5yr, 2)) %>%
    mutate(pctRx5yrPct = paste0(rxPctOfTargArea, "%")) %>%
    mutate(
        beginYear = case_when(
            year == 2000 ~ 5,
            year == 2005 ~ 10,
            year == 2010 ~ 15,
            year == 2015 ~ 20,
            year == 2020 ~ 25,
            year == 2025 ~ 30
        ),
        endYear = ifelse(beginYear <= 25, beginYear + 4, beginYear)
    ) %>%
    select(">>MgmtArea" = owncd, Prescription, pctRx5yrPct, beginYear, endYear) %>% # beginYear endYear #leaving empty for historical runs
    group_by(`>>MgmtArea`) %>%
    arrange(`>>MgmtArea`, Prescription) %>%
    write.csv(paste0(output_file), row.names = FALSE)

# write.csv(paste0("Y:/DISES_ME/harvestPrep/magicHarvestDynamicRunImpTable",Sys.Date(),".csv"),row.names=FALSE)
#


# ~ Write to the bioharv txt files ####
# ~ ----------------------------------------------------------------------------
library(data.table)

for (yr in seq(1995, 2025, by = 5)) {
    filecon <- file(file.path(
            "pipe/03_landis_simulation/landis_run/template_files/bioharvTxt",
            paste0("bioharv_", yr, ".txt")
    ))
    biohrv_txt <- readLines(filecon)
    dt_line_start <- grep("HarvestImplementations", biohrv_txt) + 1
    dt_line_end <- grep(">>Harvest Outputs", biohrv_txt) - 1

    new_harv_rates <- fread(output_file)
    # HACK: here I use the table made by Danelle
    # new_harv_rates <- fread("D:/Laflower/magicHarvestDynamicRunImpTable.csv")

    # DEBUG ========================================================================
    new_harv_rates[new_harv_rates$pctRx5yrPct == "0%", ]
    new_harv_rates[new_harv_rates$pctRx5yrPct == "0%", ]$pctRx5yrPct <- "0.01%"
    # ============== delete above ==================================================

    txt <- capture.output(
        write.table(
            new_harv_rates[, .(`>>MgmtArea`, Prescription, pctRx5yrPct, beginYear, endYear)],
            col.names = FALSE,
            row.names = FALSE, sep = "\t",
            quote = FALSE
        )
    )
    new_biohrvt_txt <- c(
        biohrv_txt[1:dt_line_start],
        txt,
        biohrv_txt[(dt_line_end + 1):length(biohrv_txt)]
    )

    writeLines(new_biohrvt_txt, filecon)

    close(filecon)
}


