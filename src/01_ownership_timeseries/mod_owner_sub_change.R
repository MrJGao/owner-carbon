# ******************************************************************************
# Ownership sub group (i.e., each company) changes.
# 
# Author: Xiaojie Gao
# Date: 2025-02-07
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



# ~ Subgroup change heatmap ####
# ~ ----------------------------------------------------------------------------
yrs <- c(1995, 2000, 2005, 2010, 2014, 2018, 2019:2024)

# Read owner grid shpfiles back in
hm_change <- lapply(yrs, function(yr) {
    vect(
        file.path("pipe/timber_harvest/owner_grid", 
        paste0("owner_grid_", yr, ".shp"))
    )
})

uni_owner <- sapply(hm_change, function(shp) {
    unique(shp$OwnerType)
}) %>%
    c() %>%
    unique() %>%
    na.omit()


# Make a table
ch_mat <- lapply(hm_change, function(ch) {
    ch$FullName
}) %>%
    do.call(cbind, .)

hm_cells <- copy(hm_change[[1]][, c("ID")])
hm_cells$change_times <- apply(ch_mat, 1, function(x) {
    if (all(is.na(x))) {
        return(NA)
    } else {
        return(length(unique(x)) - 1)
    }
})

hm_cell_rast <- rasterize(
    hm_cells,
    rast(extent = ext(hm_cells), res = 200, crs = crs(hm_cells)),
    field = "change_times"
)

# Change times of land areas
change_area <- NULL
for (i in unlist(unique(hm_cell_rast))) {
    change_times <- i
    area_change <- length(hm_cell_rast[hm_cell_rast == change_times]) *
        cellSize(hm_cell_rast, unit = "km")[1]
    area_change <- unlist(area_change)

    change_area <- rbind(change_area, data.table(
        change_times,
        area = area_change
    ))
}

change_area[, totalarea := sum(area)]
change_area[, pct := area / totalarea]
change_area

# At least changed once
change_area[change_times >= 1, .(sum(area), sum(pct))]
# At least changed three times
change_area[change_times >= 3, .(sum(area), sum(pct))]

change_area[change_times >= 5, .(sum(area), sum(pct))]


# change_area$totalarea[1] / (base$maine_forest_ha * 0.01) # km2

# Read Maine boundary
maine <- vect(base$region9_shpfile) %>%
    subset(.$NAME == "Maine") %>%
    project(hm_change[[1]])



{ # fig: Number of sub owners per land
    png(
        file.path(pipedir, "num_owner_sub_per_land.png"),
        width = 1200, height = 1500, res = 300
    )
    par(mar = c(1, 1, 1, 1))
    plot(
        maine,
        col = "grey70", mar = c(0, 0, 1.5, 0),
        border = NA,
        axes = FALSE
    )
    plot(
        hm_cell_rast,
        col = hcl.colors(256, "plasma"), type = "continuous",
        axes = FALSE,
        plg = list(digits = 0),
        add = TRUE
    )
    north("topleft", angle = -13, xpd = NA)

    # Add change area stat
    par(
        fig = c(0.6, 0.85, 0.1, 0.25), new = TRUE, mar = c(0, 0, 0, 0),
        mgp = c(0, -0.3, 0)
    )
    xloc <- barplot(
        change_area$area / 1e3,
        names.arg = change_area$change_times,
        border = NA,
        space = 0.1,
        xpd = NA,
        xaxt = "n", yaxt = "n", xlab = "", ylab = "",
        col = hcl.colors(length(change_area$change_times), "plasma"),
        horiz = TRUE
    )
    axis(
        side = 2, at = xloc, labels = unique(change_area$change_times),
        cex.axis = 0.5, tick = FALSE, las = 1,
        line = 0.5
    )
    mtext(
        side = 2, 
        text = "Change times", 
        cex = 0.5, line = 0.5
    )
    axis(side = 1, tck = -0.02, cex.axis = 0.5, las = 1)
    mtext(
        side = 1, 
        text = expression(Area ~ (1000 ~ km^2)), 
        cex = 0.5, line = 0.5
    )

    dev.off()
}



# ~ Get subgroup change per time period ####
# ~ ----------------------------------------------------------------------------
cl <- makeCluster(length(yrs), outfile = "")
calls <- clusterCall(cl, function() {
    suppressWarnings({
        library(data.table)
        library(magrittr)
        library(terra)
    })
})

tstart <- Sys.time()
clusterExport(cl, c("yrs", "pipedir"))
ch_dt <- clusterApplyLB(cl, x = yrs, fun = function(yr) {
    shp <- vect(
        file.path(
            "pipe/timber_harvest/owner_grid",
            paste0("owner_grid_", yr, ".shp")
        )
    )

    # Make a table
    ch_dt <- data.table(cbind(
        shp$ID, shp$FullName, shp$OwnerType
    ))
    colnames(ch_dt) <- c(
        "ID",
        paste(c("FullName", "OwnerType"), yr, sep = "_")
    )

    return(ch_dt)
})
tend <- Sys.time()
ttake <- tend - tstart
message(round(ttake, 2), " ", units(ttake))
stopCluster(cl)

ch_dt <- Reduce(
    f = function(x, y) {
        merge(x, y, by = "ID")
    }, 
    x = ch_dt
)
ch_dt[, ID := as.numeric(ID)]
setorder(ch_dt, ID)


subchange_dt <- lapply(2:length(yrs), function(i) {
    prev_yr <- yrs[i - 1]
    cur_yr <- yrs[i]

    # Total change per owner type
    tot <- ch_dt[,
        sum(
            get(paste0("FullName_", prev_yr)) != get(paste0("FullName_", cur_yr)),
            na.rm = TRUE
        ) / .N * 100,
        by = get(paste0("OwnerType_", prev_yr))
    ]
    colnames(tot) <- c("OwnerType", paste0("Tot_", prev_yr))
    # Change to the same owner type
    tosame <- ch_dt[
        get(paste0("OwnerType_", prev_yr)) == get(paste0("OwnerType_", cur_yr)),
        sum(
            get(paste0("FullName_", prev_yr)) != get(paste0("FullName_", cur_yr)),
            na.rm = TRUE
        ) / .N * 100,
        by = get(paste0("OwnerType_", prev_yr))
    ]
    colnames(tosame) <- c("OwnerType", paste0("ToSame_", prev_yr))

    sub <- merge(tot, tosame, by = "OwnerType")
    
    return(sub)
})
subchange_dt <- Reduce(
    f = function(x, y) {
        merge(x, y, by = "OwnerType")
    },
    x = subchange_dt
)

subchange_dt <- merge(subchange_dt, col_dt, by = "OwnerType")
# Reorder rows
subchange_dt <- subchange_dt[match(col_dt$OwnerType, OwnerType), ]


{ # fig: Make a barplot
    svglite::svglite(
        file.path(pipedir, "owner_sub_change.svg"),
        width = 10, height = 10
    )
    par(
        mfrow = c(nrow(subchange_dt), 1), 
        mar = c(1, 5, 0, 0), oma = c(4, 0, 1, 0),
        las = 1, cex.axis = 1.2, tck = -0.05, mgp = c(1.5, 0.5, 0)
    )
    for (i in 1:nrow(subchange_dt)) {
        tot <- as.numeric(subchange_dt[i, .SD, .SDcols = patterns("Tot_")])
        tosame <- as.numeric(subchange_dt[i, .SD, .SDcols = patterns("ToSame_")])

        mat <- cbind(tot, tosame)
        rownames(mat) <- yrs[1:(length(yrs) - 1)]
        xloc <- barplot(
            tot,
            col = subchange_dt[i, color],
            border = NA
        )
        barplot(
            tosame,
            density = 20, angle = 45, 
            add = TRUE
        )
        legend(
            "topright", bty = "n",
            legend = subchange_dt[i, OwnerType], cex = 1.3
        )
    }
    axis(
        side = 1, at = c(xloc - 0.5, max(xloc) + 0.5), labels = yrs,
        lwd = 0,
        xpd = NA
    )
    text(
        grconvertX(0.03, "ndc"), grconvertY(0.5, "ndc"),
        pos = 3,
        labels = "Change (%)",
        srt = 90,
        cex = 1.5,
        xpd = NA
    )

    legend(
        grconvertX(0.5, "ndc"), grconvertY(1, "ndc"),
        bty = "n", xpd = NA,
        legend = c("Total change", "To same type"),
        density = c(NA, 20),
        angle = c(NA, 45),
        ncol = 2,
        cex = 2
    )

    dev.off()
}


