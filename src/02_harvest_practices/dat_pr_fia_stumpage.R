# ******************************************************************************
# Calculate stumpage price for each FIA plot
# 
# Author: Xiaojie Gao
# Date: 2026-05-14
# ******************************************************************************
rm(list = ls())
library(data.table)
library(magrittr)



# For each plot, calculate a total stumpage price
CalStumpage <- function(plotid) {
    plot_measyr <- fia_harv_dt[concatPlot == plotid, MEASYEAR_aligned]
    if (length(plot_measyr) == 0) {
        stop("Error! no MEASYEAR for this plot!")
    }

    # Calculate the total value
    arow <- NULL
    for (measyr in plot_measyr) {
        val <- plot_value_dt[
            concatPlot == plotid & INVYR_aligned == measyr,
            sum(value_per_acre)
        ]
        arow <- rbind(arow, data.table(
            concatPlot = plotid,
            MEASYEAR_aligned = measyr,
            stumpage_value = val
        ))
    }

    return(arow)
}



pipedir <- "pipe/02_harvest_practices"
dir.create(pipedir, showWarnings = FALSE, recursive = TRUE)


fia_harv_dt <- fread(file.path(pipedir, "fia_harvest_per_owner.csv"))
plots <- unique(fia_harv_dt$concatPlot)


# Read in the necessary data
plot_value_dt <- fread("data/matching_covariates/ME_value_by_plot_inventory_product.csv")
timestamps <- seq(2000, 2020, by = 5)
plot_value_dt$INVYR_aligned <- sapply(plot_value_dt$INVYR, \(yr) {
    timestamps[which.min(abs(timestamps - yr))]
})


pb <- txtProgressBar(min = 0, max = 100, style = 3)
fia_plot_stumpage <- lapply(seq_along(plots), \(i) {
        setTxtProgressBar(pb, i * 100 / length(plots)) # update progress
        CalStumpage(plots[i])
    }
) %>%
    rbindlist()
close(pb)


# out: fia_plot_stumpage.csv
fwrite(fia_plot_stumpage, file.path(pipedir, "fia_plot_stumpage.csv"))

