# ******************************************************************************
# Validate Maine imputation in 1995
# 
# Author: Xiaojie Gao
# Date: 2025-02-24
# ******************************************************************************
rm(list = ls())
source("src/base.R")
library(terra)
library(data.table)
library(magrittr)



ImpAttrMap <- function(impmap, fia_attr_dt, attrname, k = 1,
    fea_dt = NULL, outfile = "", overwrite = FALSE
) {
    if (class(impmap) == "character") {
        impmap <- rast(impmap)
    }

    pltcn_dt <- data.table(cats(impmap)[[1]])
    if (nrow(pltcn_dt) == 0) {
        pltcn_dt <- fea_dt[, .(level = index, name = PLT_CN)]
    }

    res_img <- rast(impmap[[k]])
    for (i in k) {
        pltcn <- pltcn_dt[values(impmap[[i]]), name]
        attr_val <- fia_attr_dt[match(pltcn, PLT_CN), get(attrname)]
        values(res_img[[i]]) <- attr_val
    }
    res_img <- app(res_img, "mean", na.rm = TRUE)
    
    if (outfile == "") {
        return(res_img)
    } else {
        writeRaster(res_img, outfile, overwrite = overwrite)
    }
}



# Maine county
maine_shp <- vect(base$me_county_shp)


# FIA true measurements
fia_true <- fread("data/raw/pre2000plotCbyTPAUNADJ.csv")
# Remove tpaUnadjAdj==1, which is TPA_UNADJ=0 in the original dataset b/c we
# don't know the plot designs.
fia_true <- fia_true[tpaUnadjAdj != 1, ]

fia_true <- unique(fia_true[, .(concatPlot, PLT_CN, MEASYEAR, plotMgCha)])

# NOTE --------------------------------------------------------------------
# ! Here I use a table that was created by Danelle to re-imputed the map
fia_sub <- fread(
    "pipe/timber_harvest/tempForXiaojie_dises_1995_impmap_STDAGEplots.csv"
)
fia_true <- fia_true[PLT_CN %in% fia_sub$PLT_CN]
# -------------------------------------------------------------------------


# FIA data from Danelle to get coordinates
fia_dt <- fread("Y:/FICE/MAINE/pre1999plotsActCoord.csv")
fia_vect <- vect(
    fia_dt[!is.na(LAT_ACTUAL_NAD83) | !is.na(LON_ACTUAL_NAD83)],
    geom = c("LON_ACTUAL_NAD83", "LAT_ACTUAL_NAD83"),
    crs = "epsg:4269"
)

fia_vect <- merge(fia_vect, fia_true, by = "PLT_CN")
fia_vect_reporj <- project(fia_vect, maine_shp)
# plot(maine_shp)
# text(maine_shp, "NAME")
# points(fia_vect_reporj)

fia_vect_extract <- terra::extract(maine_shp[, "NAME"], fia_vect_reporj)
unique(fia_vect_extract$NAME)
fia_vect_reporj$county_name <- fia_vect_extract$NAME

fia_true_dt <- as.data.table(
    fia_vect_reporj[, c( "PLT_CN", "county_name", "plotMgCha")]
)
unique(fia_true_dt$county_name)

fia_true_dt[, .N, by = "county_name"]

fia_true_mn <- fia_true_dt[, 
    .(
        meanC = mean(plotMgCha, na.rm = TRUE), 
        sdC = sd(plotMgCha, na.rm = TRUE)
    ), 
    by = "county_name"
]


# The main imputation 1995
me_imp <- rast("pipe/timber_harvest/maine_imp_1995_200m_stdage.tif")
me_fea_dt <- fread("pipe/timber_harvest/maine_imp_1995_200m_stdage.csv")[, .(
    index = level, PLT_CN = name
)]

me_c_map <- ImpAttrMap(
    impmap = me_imp, fia_attr_dt = fia_true, attrname = "plotMgCha",
    fea_dt = me_fea_dt
)


maine_shp$mean_c_county <- zonal(me_c_map, maine_shp, fun = "mean", na.rm = TRUE)
maine_shp$sd_c_county <- zonal(me_c_map, maine_shp, fun = "sd", na.rm = TRUE)

com_dt <- merge(
    as.data.table(maine_shp), fia_true_mn, 
    by.x = "NAME", by.y = "county_name"
)

com_dt[abs(meanC - mean_c_county) > 14,]



outdir <- "out/timber_harvest"
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)


imp_vals <- values(me_imp[[1]])
uni_imp_vals <- sort(unique(imp_vals))


{ #fig:
    svglite::svglite(
        file.path(outdir, "maine_1995_imp_validation.svg"),
        width = 12, height = 6
    )
    par(
        mfrow = c(1, 2), mgp = c(1.7, 0.5, 0), 
        cex.lab = 1.5, cex.axis = 1.2, cex.main = 1.5
    )
    plot(
        com_dt[, .(mean_c_county, meanC)],
        xlim = c(0, 80), ylim = c(0, 80),
        pch = 16,
        xlab = "Imputed mean carbon (Mg C / ha)", 
        ylab = "Measured mean carbon (Mg C / ha)",
        main = "Maine 1995 imputation validation \n(county level)",
        cex = 1.5
    )
    abline(0, 1, lty = 2)
    # impute sd
    segments(
        com_dt[, mean_c_county - sd_c_county], com_dt[, meanC],
        com_dt[, mean_c_county + sd_c_county], com_dt[, meanC],
        col = "grey70"
    )
    segments(
        com_dt[, mean_c_county], com_dt[, meanC - sdC],
        com_dt[, mean_c_county], com_dt[, meanC + sdC],
        col = "grey70"
    )

    # The imputed FIA frequency
    hist(
        imp_vals,
        breaks = uni_imp_vals, freq = TRUE,
        xlab = "Imputed FIA index",
        main = "Maine 1995 imputation FIA index"
    )
    box(bty = "L")

    dev.off()
}



# ~ What about total carbon? ####
# ~ ----------------------------------------------------------------------------
# Convert Mg/ha to Mg
me_c_map_val <- values(me_c_map) * 200^2 * 0.0001
# Total number of cells that are not NA
me_c_total_ha <- sum(!is.na(me_c_map_val)) * 200^2 * 0.0001

map_c_sum <- sum(me_c_map_val, na.rm = TRUE)
map_c_mean <- mean(me_c_map_val, na.rm = TRUE)
map_c_med <- median(me_c_map_val, na.rm = TRUE)
map_c_sd <- sd(me_c_map_val, na.rm = TRUE)

fia_true_me_dt <- fia_true_dt[!is.na(county_name),]
fia_all_mn <- fia_true_me_dt[,
    .(
        meanC = mean(plotMgCha, na.rm = TRUE),
        sdC = sd(plotMgCha, na.rm = TRUE)
    )
]
fia_all_mn_sum <- fia_all_mn$meanC * me_c_total_ha
fia_all_sd <- fia_all_mn$sdC * me_c_total_ha


{ #fig: total C
    svglite::svglite(
        file.path(outdir, "maine_1995_imp_validation_total.svg"),
        width = 5, height = 5
    )
    rg <- range(fia_all_mn_sum + fia_all_sd, fia_all_mn_sum - fia_all_sd) / 1e6
    plot(
        map_c_sum / 1e6, fia_all_mn_sum / 1e6,
        pch = 16, ylim = rg, xlim = rg, cex = 1.5,
        xlab = "Imputed Total Carbon (Million Mg)",
        ylab = "FIA-estimated Total Carbon (Million Mg)",
        mgp = c(1.5, 0.5, 0)
    )
    abline(0, 1, lty = 2)
    segments(
        map_c_sum / 1e6, (fia_all_mn_sum + fia_all_sd) / 1e6,
        map_c_sum / 1e6, (fia_all_mn_sum - fia_all_sd) / 1e6
    )
    dev.off()
}



# ~ What about total carbon per owner ####
# ~ ----------------------------------------------------------------------------
manage_map <- rast(
    file.path(
        "pipe/timber_harvest",
        paste0("owner_manage_", 1995, ".img")
    )
)

# From the imputed map
# Convert Mg/ha to Mg
me_c_map_mg <- me_c_map * 200^2 * 0.0001
manage_map <- project(manage_map, me_c_map_mg, method = "near")
owner_c_sum <- zonal(me_c_map_mg, manage_map, fun = "sum", na.rm = TRUE)
colnames(owner_c_sum) <- c("index", "total_C")

# From FIA
fia_all_vect <- project(fia_vect_reporj, manage_map)
fia_yr_owner <- terra::extract(manage_map, fia_all_vect)
fia_true_dt[, index := fia_yr_owner$layer]
fia_true_me_dt <- fia_true_dt[!is.na(county_name)]

fia_me_agb <- fia_true_me_dt[,
    .(
        avg_agc_mgha = mean(plotMgCha, na.rm = TRUE),
        sd_agc_mgha = sd(plotMgCha, na.rm = TRUE),
        med_agc_mgha = median(plotMgCha, na.rm = TRUE),
        lwr_agc_mgha = quantile(plotMgCha, 0.025, na.rm = TRUE),
        upr_agc_mgha = quantile(plotMgCha, 0.975, na.rm = TRUE),
        fia_count = .N
    ),
    by = "index"
]

# Management area per owner
land_ha_map <- me_c_map_mg
land_ha_map[!is.na(land_ha_map)] <- 1 * 200^2 * 0.0001
manage_owner_ha <- zonal(land_ha_map, manage_map, fun = "sum", na.rm = TRUE)
colnames(manage_owner_ha) <- c("index", "manage_owner_ha")

fia_me_agb <- merge(fia_me_agb, manage_owner_ha, by = "index")

fia_me_agb[, ":=" (
    avg_agc_sum = avg_agc_mgha * manage_owner_ha,
    sd_agc_sum = sd_agc_mgha * manage_owner_ha,
    med_agc_sum = med_agc_mgha * manage_owner_ha,
    lwr_agc_sum = lwr_agc_mgha * manage_owner_ha,
    upr_agc_sum = upr_agc_mgha * manage_owner_ha
)]



# Merge FIA estimated C and imputed C tables
manage_mapcode <- fread("pipe/timber_harvest/owner_manage_mapcode.csv")
owner_c_sum <- merge(owner_c_sum, manage_mapcode, by = "index")

com_c_dt <- merge(owner_c_sum, fia_me_agb, by = "index", all.x = TRUE)
setDT(com_c_dt)


{ #fig: total C per owner
    svglite::svglite(
        file.path(outdir, "maine_1995_imp_validation_total_per_owner.svg"),
        width = 5, height = 5
    )
    par(oma = c(5, 0, 0, 0), mgp = c(1.5, 0.5, 0), mar = c(5, 3, 1, 1))

    y_rg <- range(com_c_dt[, .(total_C, lwr_agc_sum, upr_agc_sum)], na.rm = TRUE)
    plot(
        com_c_dt[, .(index, total_C)],
        pch = 16, col = "blue",
        ylim = y_rg, ylab = "Total Carbon (Mg)",
        xaxt = "n", xlab = "",
        bty = "L"
    )
    axis(side = 1, at = com_c_dt$index, labels = com_c_dt$levels, las = 2)
    
    points(com_c_dt[, .(index + 0.1, avg_agc_sum)], col = "grey", pch = 16)
    segments(
        com_c_dt[, index + 0.1], com_c_dt[, avg_agc_sum + sd_agc_sum],
        com_c_dt[, index + 0.1], com_c_dt[, avg_agc_sum - sd_agc_sum],
        col = "grey"
    )

    points(com_c_dt[, .(index - 0.1, med_agc_sum)], col = "grey30", pch = 16)
    segments(
        com_c_dt[, index - 0.1], com_c_dt[, lwr_agc_sum],
        com_c_dt[, index - 0.1], com_c_dt[, upr_agc_sum],
        col = "grey30"
    )
    
    legend(
        grconvertX(0, "npc"), grconvertY(1, "npc"), 
        bty = "n",
        legend = c(
            "Imputed",
            "FIA estimated (mean & sd)", 
            "FIA estimated (median & 95% interval)"
        ),
        pch = 16, lty = c(NA, 1, 1),
        col = c("blue", "grey", "grey30")
    )

    dev.off()
}

