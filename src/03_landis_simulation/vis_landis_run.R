# ******************************************************************************
# Visualize LANDIS simulation of different scenarios.
# 
# Author: Xiaojie Gao
# Date: 2025-07-15
# ******************************************************************************
rm(list = ls())
library(tmap)
library(grid)
library(gridExtra)



source("src/03_landis_simulation/mod_landis_run_summary.R")


figdir <- "out/03_landis_simulation/vis_run"
dir.create(figdir, showWarnings = FALSE, recursive = TRUE)

colors <- RColorBrewer::brewer.pal(9, "Set1")

colors <- adjustcolor(colors, 0.5)


# Remove conservation
scen_runs <- scen_runs[!names(scen_runs) %in% c("ConservNoHarv", "ConservParHarv")]



# ~ Total biomass ####
# ~ ----------------------------------------------------------------------------
{ # fig: Total biomass
    png(
        file.path(figdir, "total_biomass_runs.png"),
        width = 1200, height = 1200, res = 300
    )
    par(mar = c(3, 3, 1, 1), mgp = c(1.5, 0.5, 0), tck = -0.01)

    scen_bioms <- unlist(sapply(scen_runs, "[[", "biomass_sum_mg"))
    y_rg <- range(c(
        scen_bioms,
        fia_biom_dt[OwnerType == "Total", biomass_mn_sum_mg + biomass_mn_sd_mg],
        fia_biom_dt[OwnerType == "Total", biomass_mn_sum_mg - biomass_mn_sd_mg],
        na.rm = TRUE
    ))

    color_reorder <- c(
        colors[c(9, 5, 1, 4, 2)]
    )

    plot(
        NA,
        xlim = c(1995, 2025),
        # ylim = y_rg / 1e6,
        ylim = c(550, 1300) / 2,
        xaxt = "n", xlab = "Year", ylab = "Forest Carbon (Tg C)",
        bty = "L"
    )
    axis(
        side = 1,
        at = seq(1990, 2025, by = 5),
        labels = seq(1990, 2025, by = 5),
        gap.axis = 0.001
    )
    for (i in 1:length(scen_runs)) {
        lines(
            scen_runs[[i]][OwnerType == "Total", .(yr, biomass_sum_mg / 1e6 / 2)],
            type = "o", pch = 16, col = color_reorder[i],
            lwd = 2
        )
    }

    # FIA biomass - mean ± sd
    lines(
        fia_biom_dt[OwnerType == "Total", .(yr, biomass_mn_sum_mg / 1e6 / 2)],
        type = "o", lwd = 2, pch = 16,
        col = "black"
    )

    legend(
        "topleft",
        bty = "n",
        legend = c(
            "Biophysical Potential",
            "Historical",
            "No Financialization", "No Investor to New Family",
            "No Onwership Change",
            "FIA Carbon"
        ),
        lty = 1, lwd = 2, col = c(color_reorder, "black"),
        cex = 0.9
    )

    dev.off()
}


{ # fig: Total biomass
    png(
        file.path(figdir, "total_biomass_runs_barplot.png"),
        width = 1700, height = 1200, res = 300
    )
    par(mar = c(3, 3, 1, 1), mgp = c(1.5, 0.5, 0), tck = -0.01)

    scen_bioms <- unlist(sapply(scen_runs, "[[", "biomass_sum_mg"))
    y_rg <- range(c(
        scen_bioms,
        fia_biom_dt[OwnerType == "Total", biomass_mn_sum_mg + biomass_mn_sd_mg],
        fia_biom_dt[OwnerType == "Total", biomass_mn_sum_mg - biomass_mn_sd_mg],
        na.rm = TRUE
    ))

    total_carbon_mat <- lapply(scen_runs, \(sn) {
        sn[OwnerType == "Total", biomass_sum_mg / 1e6 / 2]
    })
    total_carbon_mat <- do.call(rbind, total_carbon_mat)
    
    # Reorder
    # total_carbon_mat <- total_carbon_mat[c(3:7, 1, 2), ]
    # color_reorder <- c(
    #     colors[c(9, 5, 1, 4, 2)],
    #     hcl.colors(5, "Greens 2")[c(1, 3)]
    # )
    color_reorder <- c(
        colors[c(9, 5, 1, 4, 2)]
    )

    x <- barplot(
        total_carbon_mat[, -1],
        beside = TRUE,
        width = 0.5,
        names.arg = seq(2000, 2025, by = 5),
        xlim = c(1, 18),
        ylim = c(260, 650),
        ylab = "Total Carbon (1 Tg C)",
        border = NA,
        col = color_reorder,
        xpd = FALSE
    )
    legend(
        "topleft",
        bty = "n",
        # legend = rownames(total_carbon_mat),
        # legend = c(
        #     "Biophysical Potential", "Historical", "No Financialization",
        #     "No Investor to New Family", "No Onwership Change", 
        #     "Conservation No Harvest", "Conservation Partial Harvest"
        # ),
        legend = c(
            "Biophysical Potential", "Historical", "No Financialization",
            "No Investor to New Family", "No Onwership Change"
        ),
        fill = color_reorder, border = NA
    )

    dev.off()
}





scen_runs[["historical"]][OwnerType == "Total", .(2025, biomass_sum_mg / 1e6 / 2)]
scen_runs[["no_timo2newfamily"]][OwnerType == "Total", .(2025, biomass_sum_mg / 1e6 / 2)]
scen_runs[["no_ind2timo"]][OwnerType == "Total", .(2025, biomass_sum_mg / 1e6 / 2)]
scen_runs[["static_owner"]][OwnerType == "Total", .(2025, biomass_sum_mg / 1e6 / 2)]

scen_harvest[["historical"]][OwnerType == "Total", .(2025, harv_sum_mg / 1e6)]
scen_harvest[["no_timo2newfamily"]][OwnerType == "Total", .(2025, harv_sum_mg / 1e6)]
scen_harvest[["no_ind2timo"]][OwnerType == "Total", .(2025, harv_sum_mg / 1e6)]
scen_harvest[["static_owner"]][OwnerType == "Total", .(2025, harv_sum_mg / 1e6)]


# ~ Total harvest ####
# ~ ----------------------------------------------------------------------------
{
    png(
        file.path(figdir, "total_harvest_runs.png"),
        width = 1200, height = 1200, res = 300
    )
    par(mar = c(3, 3, 1, 1), mgp = c(1.5, 0.5, 0), tck = -0.01)

    scen_harv <- unlist(sapply(scen_harvest, "[[", "harv_sum_mg"))
    y_rg <- range(c(
        scen_harv,
        na.rm = TRUE
    ))

    plot(
        NA,
        xlim = c(1995, 2025),
        ylim = c(23, 80),
        xaxt = "n", xlab = "Year", ylab = "Harvest (1 Tg C)",
        bty = "L"
    )
    axis(
        side = 1,
        at = seq(1990, 2025, by = 5),
        labels = seq(1990, 2025, by = 5),
        gap.axis = 0.001
    )
    for (i in 1:length(scen_harvest)) {
        if (is.null(scen_harvest[[i]])) {
            next
        }
        lines(
            scen_harvest[[i]][OwnerType == "Total", .(yr, harv_sum_mg / 1e6)],
            type = "o", pch = 16, col = colors[i],
            lwd = 2
        )
    }

    # Maine Forest Report harvest
    # lines(
    #     mfr_dt[, .(Year, harv_mg / 1e6)],
    #     type = "o", lwd = 2, pch = 16,
    #     col = "grey"
    # )

    legend(
        "topleft",
        bty = "n",
        legend = c(
            names(scen_harvest),
            "MFR harvest"
        ),
        lty = 1, lwd = 2, col = c(colors[1:length(scen_harvest)], "grey")
    )

    dev.off()
}


# ~ Biomass maps ####
# ~ ----------------------------------------------------------------------------
# Remove grow only scen
scen_imgs <- scen_imgs[[scen_names != "grow_only"]]
qt <- quantile(scen_imgs[], probs = c(0.025, 0.975), na.rm = TRUE)

for (i in 1:nlyr(scen_imgs)) {
    ss <- scen_imgs[[i]]

    png(
        file.path(figdir, paste0(names(ss), ".png")),
        width = 1200, height = 1600, res = 300
    )
    p <- tm_shape(ss) +
        tm_raster(
            col.scale = tm_scale_continuous(
                limits = qt,
                outliers.trunc = c(TRUE, TRUE),
                values = "hcl.purple_green",
                midpoint = 0
            ),
            col.legend = tm_legend(
                show = ifelse(names(ss) == "historical", TRUE, FALSE),
                frame = FALSE, title = "",
                position = tm_pos_in(
                    pos.h = "RIGHT", pos.v = "center"
                ),
                height = 25,
                reverse = TRUE
            )
        ) +
        tm_title(
            names(ss), position = tm_pos_in(), size = 1
        ) +
        tm_layout(
            frame = FALSE
        )
    print(p)
    dev.off()
}



# ~ Merge figures together ####
# ~ ----------------------------------------------------------------------------
png(
    "out/landis_simulation_result.png",
    width = 2000, height = 1600, res = 300
)
grobs <- list(
    rasterGrob(
        png::readPNG(file.path(figdir, "total_biomass_runs.png")), 
        interpolate = TRUE
    ),
    rasterGrob(
        png::readPNG(file.path(figdir, "historical.png")), 
        interpolate = TRUE
    ),
    rasterGrob(
        png::readPNG(file.path(figdir, "no_ind2timo.png")), 
        interpolate = TRUE
    ),
    rasterGrob(
        png::readPNG(file.path(figdir, "static_owner.png")), 
        interpolate = TRUE
    ),
    rasterGrob(
        png::readPNG(file.path(figdir, "conservation.png")), 
        interpolate = TRUE
    ),
    rasterGrob(
        png::readPNG(file.path(figdir, "ind2tribal.png")), 
        interpolate = TRUE
    )
)

# Arrange in 3-column grids
grid.arrange(grobs = grobs, ncol = 3, nrow = 2)
dev.off()







