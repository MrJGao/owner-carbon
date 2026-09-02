# ******************************************************************************
# This script translates harvest rates into LANDIS prescriptions.
# 
# This script is copied and modified from Danelle.
# 
# Author: Xiaojie Gao
# Date: 2026-07-10
# ******************************************************************************
# rm(list = ls())
library(tidyverse)
library(dplyr)
library(tidyr)
library(purrr)
library(readxl)
library(stringr)
library(data.table)



# Harvest rates were owner-specific, here we convert them to owner- and
# prescription-specific. The owner-specific rates will be distributed to each
# prescription. 
ConvRatesToPrescriptions <- function(rates_dt, outdir) {
    # Assing Municipal and Federal
    munifedOverride <- data.frame(
        ownerName = c("Municipal", "Federal"),
        meanRate = c(2.4 * 5, 1 * 5)
    )
    ownHarv <- rates_dt %>%
        mutate(meanRate = harv_prob * 5 * 100) %>%
        mutate(
            OwnerType = case_when(OwnerType == "Unknown" ~ "NoOwner", .default = OwnerType),
            ownerName = str_replace_all(OwnerType, " ", "")
        ) %>%
        dplyr::select(-OwnerType) %>%
        filter(ownerName != "Federal") %>%
        bind_rows(munifedOverride)

    
    # copied from:  Y:/DISES_ME/Rcode/MethodologyWorkflowHistorical.Rmd
    # this table will need to be updated when the runs are long enough for repeat harvests
    ownIntensityRxSuite <- read_excel(
        "Y:/DISES_ME/harvestPrep/ownerIntensityRxSuites.xlsx", 
        sheet = "intensityWorksheet"
    ) # row 2 is the mean Rx intensity

    ownIntensityRxSuite <- ownIntensityRxSuite %>% 
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
        )
    
    ownIntensityRxSuite <- left_join(
        ownIntensityRxSuite, 
        ownHarv %>% select(ownerName, harvestRate5yr = meanRate)
    ) 
    ownIntensityRxSuite <- ownIntensityRxSuite %>%
        pivot_longer(
            names_to = "Prescription", 
            values_to = "pctOfTargArea", 
            PartialHarvest:Incidental
        ) %>%
        replace_na(list(pctOfTargArea = 0)) %>%
        filter(pctOfTargArea > 0) %>%
        mutate(rxPctOfTargArea = round((pctOfTargArea / 100) * harvestRate5yr, 2)) %>%
        mutate(pctRx5yrPct = paste0(rxPctOfTargArea, "%"))
    
    ownIntensityRxSuite <- ownIntensityRxSuite %>%
        select(">>MgmtArea" = owncd, Prescription, pctRx5yrPct) %>% 
        group_by(`>>MgmtArea`) %>%
        arrange(`>>MgmtArea`, Prescription)
    

    # Write out prescriptions
    dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
    file.copy(
        from = list.files(
            file.path(
                "pipe/03_landis_simulation/landis_run", 
                "template_files", 
                "bioharvTxt"
            ),
            full.names = TRUE
        ),
        to = outdir,
        recursive = TRUE
    )
    # Update file contents
    for (yr in seq(1995, 2025, by = 5)) {
        filecon <- file(file.path(
            outdir,
            paste0("bioharv_", yr, ".txt")
        ))
        biohrv_txt <- readLines(filecon)
        dt_line_start <- grep("HarvestImplementations", biohrv_txt) + 1
        dt_line_end <- grep(">>Harvest Outputs", biohrv_txt) - 1

        new_harv_rates <- as.data.table(ownIntensityRxSuite)
        # Reorder to match w/ Danelle's table, so easier to compare
        new_harv_rates[, 
            Prescription_fact := factor(
                Prescription, 
                levels = c(
                    "CommThinClearcutPlant", "ContinuousCover", 
                    "ClearcutNatRegen", "Incidental"
                )
            )
        ]
        setorder(new_harv_rates, `>>MgmtArea`, Prescription_fact)
        new_harv_rates <- new_harv_rates[, lapply(.SD, format, justify = "left")]

        txt <- capture.output(
            write.table(
                new_harv_rates[, .(`>>MgmtArea`, Prescription, pctRx5yrPct)],
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
}


# rates_dt <- read.csv("D:/Gao/projects/owner-carbon/pipe/02_harvest_practices/owner_harvest_rates_dt.csv")
# outdir <- "pipe/03_landis_simulation/landis_run/template_files/bioharvTxt_sample"

# ConvRatesToPrescriptions(rates_dt, outdir)



