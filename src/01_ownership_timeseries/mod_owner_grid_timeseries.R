# ******************************************************************************
# Investigate onwer change type grids to see if there are different change
# patterns per owner type.
# 
# Author: Xiaojie Gao
# Date: 2025-05-10
# ******************************************************************************
rm(list = ls())
source("src/base.R")
source("src/vis_owner_color.R")
library(data.table)
library(magrittr)
library(terra)
library(parallel)



pipedir <- "pipe/01_ownership_timeseries"
dir.create(pipedir, showWarnings = FALSE, recursive = TRUE)



# ~ Process data ####
# ~ ----------------------------------------------------------------------------
source("src/01_ownership_timeseries/dat_pr_owner_grid_timeseries.R")


 
# ~ Analyze owner change ####
# ~ ----------------------------------------------------------------------------
# For each grid, find its owner type, for which the next time step is compared
# with to see if it changed owner. For example, for an Industrial grid in 1995,
# compare the owner name with the same grid ID in 2000. If the owner name
# changed, count 1 change for Industrial owner type in 5 years. Do this for each
# owner type, we can then investigate which owner type had more frequent owner
# changes.

# Stores change information
owner_change_dt <- NULL
# Stores the longest year length w/o change per ID
owner_stable_dt <- NULL
pb <- txtProgressBar(min = 0, max = 100, style = 3)
for (i in 1:(length(yrs) - 1)) {
    yr <- yrs[i]
    next_yr <- yrs[i + 1]

    owner_this_yr <- owner_grid_dt[Year == yr,]
    owner_next_yr <- owner_grid_dt[Year == next_yr,]

    owner_name_change <- owner_this_yr$FullName != owner_next_yr$FullName
    this_yr_change <- owner_this_yr[owner_name_change, ]
    next_yr_change <- owner_next_yr[owner_name_change, ]
    # identical(this_yr_change$ID, next_yr_change$ID)

    this_change_dt <- data.table(
        ID = this_yr_change$ID,
        period = paste0("p", yr, "-", next_yr),
        before_OwnerType = this_yr_change$OwnerType,
        after_OwnerType = next_yr_change$OwnerType,
        before_FullName = this_yr_change$FullName,
        after_FullName = next_yr_change$FullName
    )
    owner_change_dt <- rbind(owner_change_dt, this_change_dt)
    

    owner_stable <- owner_this_yr[
        !owner_name_change,
        .(ID, period = paste0("p", yr, "-", next_yr), OwnerType, FullName)
    ]
    owner_stable_dt <- rbind(owner_stable_dt, owner_stable)

    setTxtProgressBar(pb, i * 100 / (length(yrs) - 1)) # update progress
}
close(pb)

owner_type_grid_count <- owner_grid_dt[, .(count = .N), by = c("OwnerType", "Year")]



# ~ Visualize results ####
# ~ ----------------------------------------------------------------------------

# ~ Q1 ----------------------------------------
# In each period, how many lands experienced owner name change per owner type?

owner_change_summary_dt <- NULL
for (i in 1:(length(yrs) - 1)) {
    yr <- yrs[i]
    next_yr <- yrs[i + 1]
    prd <- paste0("p", yr, "-", next_yr)

    # Proportion experienced change
    this_ch_dt <- owner_change_dt[period == prd, .N, by = before_OwnerType]
    this_ch_dt <- merge(
        this_ch_dt, owner_type_grid_count[Year == yr,], 
        by.x = "before_OwnerType", by.y = "OwnerType"
    )
    this_ch_dt[, name_ch_pct := N / count * 100]
    
    owner_change_summary_dt <- rbind(owner_change_summary_dt, this_ch_dt)
}
owner_change_summary_dt[, yr_count := sum(count), by = Year]


# Make a matrix
ch_pct_dt <- data.table(OwnerType = col_dt[OwnerType != "Unknown", OwnerType])
for (y in yrs[1:(length(yrs) - 1)]) {
    pct <- owner_change_summary_dt[
        Year == y,
        .(pct = sum(N) / yr_count * 100),
        by = before_OwnerType
    ]
    ch_pct_dt <- merge(
        ch_pct_dt, pct, 
        by.x = "OwnerType", by.y = "before_OwnerType",
        all.x = TRUE
    )
    colnames(ch_pct_dt)[ncol(ch_pct_dt)] <- y
}
ch_pct_mat <- as.matrix(ch_pct_dt[, -1])
rownames(ch_pct_mat) <- ch_pct_dt[,OwnerType]

ch_pct_mat[is.na(ch_pct_mat)] <- 0

# Remove "Other"
ch_pct_mat <- ch_pct_mat[rownames(ch_pct_mat) != "Other", ]

cols <- col_dt[match(rownames(ch_pct_mat), col_dt$OwnerType), color]

# Change REIT/TIMO to Investor
rownames(ch_pct_mat)[rownames(ch_pct_mat) == "REIT/TIMO"] <- "Investor"
col_dt[OwnerType == "REIT/TIMO", OwnerType := "Investor"]

{ # fig: total name changes
    svglite::svglite(
        file.path(pipedir, "owner_name_change_total.svg"),
        width = 7, height = 5
    )
    par(mar = c(3, 4, 1, 1), mgp = c(1.7, 0.5, 0))
    
    xloc <- barplot(
        ch_pct_mat,
        col = cols,
        border = NA,
        xaxt = "n",
        ylab = "Total Name Change (%)",
        cex.lab = 1.5,
        ylim = c(0, 50)
    )
    axis(
        side = 1, 
        at = c(xloc - 0.5, max(xloc) + 0.5), 
        labels = yrs,
        cex.axis = 1,
        gap.axis = 0.01,
        line = 0,
        lwd = 0,
        xpd = NA
    )
    legend(
        grconvertX(0.4, "ndc"), grconvertY(0.98, "ndc"),
        x.intersp = 0.5,
        bty = "n", ncol = 2,
        legend = col_dt[!OwnerType %in% c("Unknown", "Other"), OwnerType],
        fill = col_dt[!OwnerType %in% c("Unknown", "Other"), color], 
        border = NA,
        xpd = NA
    )
    
    dev.off()
}


# ----------------------------------------------------------------------------
# Investigate some changes shown in the above figure

owner_change_dt[
    period == "p2010-2014" & before_OwnerType == "REIT/TIMO" & after_OwnerType == "New Family"
] %>%
    .[, .N, by = c("before_FullName", "after_FullName")] %>%
    setorder(-N) %>%
    print()
# ^^ So the major transition in 2010-2014 from REIT/TIMO to New Family was
# dominated by GMO Renewable Resources -> BBC Land LLC

owner_change_dt[
    period == "p2014-2018" & before_OwnerType == "REIT/TIMO" & after_OwnerType == "REIT/TIMO",
    .(before_FullName, after_FullName),
] %>%
    .[, .N, by = c("before_FullName", "after_FullName")] %>%
    setorder(-N) %>%
    print()
# ^^ The major transition in 2014-2018 within REIT/TIMO is Plum Creek ->
# Weyerhaeuser and Wagner Timber Partners -> Sandy Gray.

# ----------------------------------------------------------------------------



# ~ Q2 ----------------------------------------
# What's the longest stable year length per owner type?

owner_stable_dt[, start_yr := as.numeric(substr(period, 2, 5))]
owner_stable_dt[, end_yr := as.numeric(substr(period, 7, 10))]


FindStableYrsForID <- function(id) {
    iddt <- owner_stable_dt[ID == id, ]
    setorder(iddt, start_yr)

    if (nrow(iddt) == 11) {
        # Means this land experienced no change during the entire study period
        return(data.table(
            ID = iddt$ID[1],
            OwnerType = iddt$OwnerType[1],
            FullName = iddt$FullName[1],
            start_yr = 1995,
            end_yr = 2024
        ))
    }

    if (nrow(iddt) == 1) {
        # Means this land experienced only one stable period
        return(data.table(
            ID = iddt$ID,
            OwnerType = iddt$OwnerType,
            FullName = iddt$FullName,
            start_yr = iddt$start_yr,
            end_yr = iddt$end_yr
        ))
    }


    # Find stable years
    tmp <- NULL
    st_yr <- 0
    ed_yr <- 0
    for (i in 1:(nrow(iddt) - 1)) {
        therow <- iddt[i, ]
        nextrow <- iddt[i + 1]

        if (st_yr == 0) {
            st_yr <- therow$start_yr
        }
        if (nextrow$start_yr == therow$end_yr) {
            # Means continuous
            if (i == (nrow(iddt) - 1)) { # next row is the last row
                ed_yr <- nextrow$end_yr
                tmp <- rbind(
                    tmp,
                    therow[, .(
                        ID,
                        OwnerType, FullName,
                        start_yr = st_yr, end_yr = ed_yr
                    )]
                )
                st_yr <- 0
            }
        } else if (i != (nrow(iddt) - 1)) {
            # Break ownership
            ed_yr <- therow$end_yr
            tmp <- rbind(
                tmp,
                therow[, .(
                    ID,
                    OwnerType, FullName,
                    start_yr = st_yr, end_yr = ed_yr
                )]
            )
            st_yr <- 0
        }
    }

    return(tmp)
}



owner_stable_summary_dt_file <- file.path(pipedir, "owner_stable_summary_dt.csv")
if (!file.exists(owner_stable_summary_dt_file)) {
    # ! The following cluster run takes ~60 min to finish ----------------------
    tstart <- Sys.time()

    cl <- makeCluster(40, outfile = "")
    calls <- clusterCall(cl, function() {
        suppressMessages({
            library(data.table)
            library(magrittr)
        })
    })
    clusterExport(cl, c("owner_stable_dt"))
    owner_stable_summary <- clusterApplyLB(
        cl,
        x = unique(owner_stable_dt$ID),
        fun = FindStableYrsForID
    )
    stopCluster(cl)

    tend <- Sys.time()
    ttake <- tend - tstart
    message(round(ttake, 2), " ", units(ttake))

    owner_stable_summary <- rbindlist(owner_stable_summary)
    owner_stable_summary[, stable_yrs := end_yr - start_yr]

    # out: for later analysis
    fwrite(owner_stable_summary, owner_stable_summary_dt_file)
    # ! ------------------------------------------------------------------------
}
owner_stable_summary <- fread(owner_stable_summary_dt_file)
# Remove start year after 2021 and end year is 2024, b/c there's no way to know
# whether they are stable or not after 2024.
owner_stable_summary <- owner_stable_summary[
    !(start_yr >= 2020 & end_yr == 2024)
]


owner_stable_summary[OwnerType == "New Family" & stable_yrs < 3]


# Account for land area per owner
stable_stat <- owner_stable_summary[,
        .(
            mn = mean(stable_yrs), sd = sd(stable_yrs),
            qt25 = quantile(stable_yrs, 0.25),
            qt500 = quantile(stable_yrs, 0.5),
            qt75 = quantile(stable_yrs, 0.75),
            min = min(stable_yrs), max = max(stable_yrs)
        ),
        by = c("OwnerType")
    ] %>%
    setorder(-qt500)

# Remove "Other"
stable_stat <- stable_stat[OwnerType != "Other"]

# ----------------------------------------------------------------------------
# Some examples

owner_stable_summary[OwnerType == "Other", ]
ownergrid_wide_nona[ID == 69135, ]

owner_stable_summary[OwnerType == "New Family" & stable_yrs < 3, ]
owner_stable_summary[OwnerType == "REIT/TIMO" & stable_yrs < 3, ]



owner_stable_summary[
    OwnerType == "New Family", 
    summary(stable_yrs), 
    by = "FullName"
]

# ----------------------------------------------------------------------------

setorder(stable_stat, -mn)

# Change REIT/TIMO to Investor
stable_stat[OwnerType == "REIT/TIMO", OwnerType := "Investor"]

cols_reorder <- col_dt[match(stable_stat$OwnerType, col_dt$OwnerType), color]


{ # fig:
    svglite::svglite(
        file.path(pipedir, "owner_stable_yrs.svg"),
        width = 6, height = 5, bg = "NA"
    )
    par(mar = c(4, 10, 1, 1), mgp = c(1.5, 0.5, 0))

    coords <- barplot(
        stable_stat$mn,
        horiz = TRUE,
        names.arg = stable_stat$OwnerType,
        col = cols_reorder,
        border = NA,
        las = 1,
        xlim = c(0, 35),
        xlab = "Number of Stable Years"
    )
    segments(
        stable_stat[, mn - sd], coords,
        stable_stat[, mn + sd], coords
    )

    dev.off()
}



