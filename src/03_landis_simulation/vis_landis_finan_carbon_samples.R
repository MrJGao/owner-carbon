# ******************************************************************************
# Similar to `vis_landis_finan_carbon.R` but here we summarize 100 LANDIS runs.
# 
# Author: Xiaojie Gao
# Date: 2026-04-08
# ******************************************************************************
library(data.table)



pipedir <- file.path("pipe/03_landis_simulation", "landis_run_samples_result")
noI2T_carbon_diff_dt <- fread(file.path(pipedir, "noI2T_carbon_diff_dt.csv"))
static_carbon_diff_dt <- fread(file.path(pipedir, "static_carbon_diff_dt.csv"))
noT2NF_carbon_diff_dt <- fread(file.path(pipedir, "noT2NF_carbon_diff_dt.csv"))

# ----------------------------------------------------------------------------
# Uncertainty at 2025

noI2T_carbon_diff_dt[,
    .(mn = mean(change_total), sd = sd(change_total)),
    by = "year"
]
noI2T_carbon_diff_dt[,
    .(mn = mean(change_carbon_direct), sd = sd(change_carbon_direct)),
    by = "year"
]
noI2T_carbon_diff_dt[,
    .(mn = mean(delta_direct_pct), sd = sd(delta_direct_pct)),
    by = "year"
]

static_carbon_diff_dt[,
    .(mn = mean(change_total), sd = sd(change_total)),
    by = "year"
]
static_carbon_diff_dt[,
    .(mn = mean(change_carbon_direct), sd = sd(change_carbon_direct)),
    by = "year"
]
static_carbon_diff_dt[,
    .(mn = mean(delta_direct_pct), sd = sd(delta_direct_pct)),
    by = "year"
]

noT2NF_carbon_diff_dt[,
    .(mn = mean(change_total), sd = sd(change_total)),
    by = "year"
]
noT2NF_carbon_diff_dt[,
    .(mn = mean(change_carbon_direct), sd = sd(change_carbon_direct)),
    by = "year"
]
noT2NF_carbon_diff_dt[,
    .(mn = mean(delta_direct_pct), sd = sd(delta_direct_pct)),
    by = "year"
]

# ----------------------------------------------------------------------------


figdir <- "out/03_landis_simulation/vis_run"
dir.create(figdir, showWarnings = FALSE, recursive = TRUE)

colors <- RColorBrewer::brewer.pal(9, "Set1")
colors <- adjustcolor(colors, 0.5)

{ # fig:
    png(
        file.path(figdir, "carbon_rel_to_historical_samples.png"),
        width = 1500, height = 1200, res = 300
    )
    par(mar = c(3, 3, 1, 1), mgp = c(1.5, 0.5, 0), tck = -0.01)

    color_reorder <- colors[c(1, 2, 4)]
    x <- barplot(
        -rbind(
            noI2T_carbon_diff_dt[, mean(change_total), by = "year"]$V1,
            static_carbon_diff_dt[, mean(change_total), by = "year"]$V1,
            noT2NF_carbon_diff_dt[, mean(change_total), by = "year"]$V1
        ),
        beside = TRUE,
        names.arg = seq(2000, 2025, by = 5),
        xlim = c(0, 27),
        ylim = c(-8, 20),
        ylab = "ΔCarbon (1 Tg C)",
        border = NA,
        space = c(0, 1.5),
        col = color_reorder
    )
    x2 <- barplot(
        -rbind(
            noI2T_carbon_diff_dt[, mean(change_carbon_direct), by = "year"]$V1,
            static_carbon_diff_dt[, mean(change_carbon_direct), by = "year"]$V1,
            noT2NF_carbon_diff_dt[, mean(change_carbon_direct), by = "year"]$V1
        ),
        add = TRUE,
        beside = TRUE,
        density = 20,
        space = c(0, 1.5),
        col = "white"
    )
    segments(
        x2[1,], -noI2T_carbon_diff_dt[, mean(change_carbon_direct) + sd(change_carbon_direct), by = "year"]$V1,
        x2[1,], -noI2T_carbon_diff_dt[, mean(change_carbon_direct) - sd(change_carbon_direct), by = "year"]$V1
    )
    segments(
        x2[2,], -static_carbon_diff_dt[, mean(change_carbon_direct) + sd(change_carbon_direct), by = "year"]$V1,
        x2[2,], -static_carbon_diff_dt[, mean(change_carbon_direct) - sd(change_carbon_direct), by = "year"]$V1
    )
    segments(
        x2[3,], -noT2NF_carbon_diff_dt[, mean(change_carbon_direct) + sd(change_carbon_direct), by = "year"]$V1,
        x2[3,], -noT2NF_carbon_diff_dt[, mean(change_carbon_direct) - sd(change_carbon_direct), by = "year"]$V1
    )
    abline(h = 0, lty = 1)
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
        noI2T_carbon_diff_dt[, -mean(change_carbon_direct), by = "year"]$V1,
        paste0(
            round(
                noI2T_carbon_diff_dt[, -mean(delta_direct_pct), by = "year"]$V1, 
                0
            ), 
            "%"
        ),
        pos = 2, cex = 0.75, offset = 0.5
    )

    dev.off()
}


