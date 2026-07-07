# ******************************************************************************
# Helper functions for FIA matching and analyzing.
# 
# Author: Xiaojie Gao
# Date: 2026-05-14
# ******************************************************************************
library(data.table)
library(magrittr)


# Find stable plots during the entire period
FindStablePlots <- function(owner_type, fia_harv_dt) {
    stable_plots <- NULL
    for (plotid in unique(fia_harv_dt$concatPlot)) {
        iddt <- fia_harv_dt[
            concatPlot == plotid &
                (ownertype_init == owner_type & OwnerType == owner_type),
        ]
        if (nrow(iddt) == 0) next

        range_yrs <- range(iddt$MEASYEAR_aligned)
        if (range_yrs[1] == 2005 & range_yrs[2] == 2020) {
            # Add 2000 carbon and BA back
            iddt <- fia_harv_dt[concatPlot == plotid, ]
            stable_plots <- rbind(stable_plots, iddt)
        }
    }
    return(stable_plots)
}

# Find plots that experienced financialization
FindFinanPlot <- function(fia_harv_dt) {
    finan_dt <- lapply(fia_harv_dt[, unique(concatPlot)], \(plotid) {
        if (!plotid %in% fia_harv_dt$concatPlot) {
            return(NULL)
        }
        iddt <- fia_harv_dt[concatPlot == plotid, ]

        # Only need records with Industrial changed to Investor
        change_dt <- iddt[
            (ownertype_init == "Industrial" & OwnerType == "REIT/TIMO"),
        ]
        if (nrow(change_dt) == 0) {
            return(NULL)
        }

        change_yr <- change_dt[, MEASYEAR]

        # Before change
        rowindex <- which(change_yr == iddt$MEASYEAR)
        before <- iddt[
            rowindex - 1,
            .(MEASYEAR, change_yrs = MEASYEAR - change_yr, AGC_live_Mgha)
        ]
        current <- iddt[
            MEASYEAR == change_yr,
            .(MEASYEAR, change_yrs = MEASYEAR - change_yr, AGC_live_Mgha)
        ]
        after <- iddt[
            MEASYEAR > change_yr,
            .(MEASYEAR, change_yrs = MEASYEAR - change_yr, AGC_live_Mgha)
        ]

        arow <- data.table(concatPlot = plotid, rbind(before, current, after)) %>%
            na.omit()
        # # Average values for duplicated years
        # arow <- arow[,
        #     .(AGC_live_Mgha = mean(AGC_live_Mgha, na.rm = TRUE)),
        #     by = .(concatPlot, MEASYEAR, change_yrs)
        # ]

        return(arow)
    }) %>%
        rbindlist()

    return(finan_dt)
}



