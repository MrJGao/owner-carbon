# ******************************************************************************
# Visualize the result of landis runs on financialization impacts on carbon
# 
# Author: Xiaojie Gao
# Date: 2026-04-08
# ******************************************************************************

# source("src/03_landis_simulation/mod_landis_run_summary.R")
source("src/03_landis_simulation/mod_landis_finan_impact.R")


figdir <- "out/03_landis_simulation/vis_run"
dir.create(figdir, showWarnings = FALSE, recursive = TRUE)


colors <- RColorBrewer::brewer.pal(9, "Set1")
colors <- adjustcolor(colors, 0.5)


{ # fig:
    png(
        file.path(figdir, "carbon_rel_to_historical.png"),
        width = 1500, height = 1200, res = 300
    )
    par(mar = c(3, 3, 1, 1), mgp = c(1.5, 0.5, 0), tck = -0.01)

    color_reorder <- colors[c(1, 2, 4)]
    x <- barplot(
        -rbind(
            noI2T_carbon_diff_dt[, change_total],
            static_carbon_diff_dt[, change_total],
            noT2NF_carbon_diff_dt[, change_total]
        ),
        beside = TRUE,
        names.arg = seq(2000, 2025, by = 5),
        ylim = c(-5, 20),
        ylab = "ΔCarbon (1 Tg C)",
        border = NA,
        col = color_reorder
    )
    x2 <- barplot(
        -rbind(
            noI2T_carbon_diff_dt[, change_carbon_direct],
            static_carbon_diff_dt[, change_carbon_direct],
            noT2NF_carbon_diff_dt[, change_carbon_direct]
        ),
        add = TRUE,
        beside = TRUE,
        density = 20,
        col = "white"
    )
    abline(h = 0, lty = 2)
    legend(
        "topleft",
        bty = "n",
        legend = c(
            "No Financialization",
            "No Ownership Change",
            "No Investor to New Family"
        ),
        fill = color_reorder, border = NA
    )

    text(
        x2[1, ], 
        noI2T_carbon_diff_dt[, -change_carbon_direct],
        paste0(round(noI2T_carbon_diff_dt[, -delta_direct_pct], 1), "%"),
        pos = 3, cex = 0.8, offset = 0.2
    )

    dev.off()
}



# ~ Not use - Conservation, relative to historical ####
# ~ ----------------------------------------------------------------------------
# { # fig:
#     png(
#         file.path(figdir, "conservation_rel_to_historical.png"),
#         width = 1500, height = 1200, res = 300
#     )
#     par(mar = c(3, 3, 1, 1), mgp = c(1.5, 0.5, 0), tck = -0.01)

#     cols <- hcl.colors(5, "Greens 2")[c(1, 3)]

#     x <- barplot(
#         -rbind(
#             conservNoHarv_carbon_diff_dt[, change_total],
#             conservParHarv_carbon_diff_dt[, change_total]
#         ),
#         beside = TRUE,
#         names.arg = seq(2000, 2025, by = 5),
#         xlim = c(1, 19),
#         ylim = c(0, 40),
#         ylab = "ΔCarbon (1 Million Megagram)",
#         border = NA,
#         col = cols
#     )
#     barplot(
#         -rbind(
#             conservNoHarv_carbon_diff_dt[, change_carbon_direct],
#             conservParHarv_carbon_diff_dt[, change_carbon_direct]
#         ),
#         beside = TRUE,
#         add = TRUE,
#         density = 20,
#         col = "lightgrey"
#     )
#     abline(h = 0, lty = 2)
    
#     # text(
#     #     x[1, ] - 0.3,
#     #     conservNoHarv_carbon_diff_dt[, -change_carbon_direct],
#     #     paste0(round(conservNoHarv_carbon_diff_dt[, -delta_direct_pct], 1), "%"),
#     #     pos = 3, cex = 0.8, offset = 0.2
#     # )
#     # text(
#     #     x[2, ] + 0.3,
#     #     conservParHarv_carbon_diff_dt[, -change_carbon_direct],
#     #     paste0(round(conservParHarv_carbon_diff_dt[, -delta_direct_pct], 1), "%"),
#     #     pos = 3, cex = 0.8, offset = 0.2
#     # )
    
#     legend(
#         "topleft",
#         bty = "n",
#         legend = c("Conservation No Harvest", "Conservation Partial Harvest"),
#         fill = cols, border = NA
#     )

#     dev.off()
# }
