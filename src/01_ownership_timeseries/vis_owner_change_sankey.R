# ******************************************************************************
# Use a sankey diagram to show ownership changes
# 
# Author: Xiaojie Gao
# Date: 2025-01-16
# ******************************************************************************
rm(list = ls())
source("src/vis_owner_color.R")
library(terra)
library(networkD3)
library(data.table)
library(magrittr)
library(htmlwidgets)



pipedir <- "pipe/01_ownership_timeseries"
figdir <- "out/01_ownership_timeseries"
dir.create(figdir, showWarnings = FALSE, recursive = TRUE)


yrs <- c(1995, 2000, 2005, 2010, 2014, 2018, 2019:2024)

# Read owner grid shpfiles back in
hm_change <- lapply(yrs, function(yr) {
    vect(
        file.path(pipedir, "owner_grid", 
        paste0("owner_grid_", yr, ".shp"))
    )
})

uni_owner <- sapply(hm_change, function(shp) {
    unique(shp$OwnerType)
}) %>%
    c() %>%
    unique() %>%
    na.omit()


link_dt <- NULL
for (i in 2:length(yrs)) {
    pre_yr <- hm_change[[i - 1]]
    the_yr <- hm_change[[i]]

    tmp <- data.table(
        pre_yr$OwnerType, the_yr$OwnerType
    )
    tmp <- na.omit(tmp)

    for (src in uni_owner) {
        for (tgt in uni_owner) {
            tmp_row <- data.table(
                source = src, target = tgt, 
                value = tmp[V1 == src & V2 == tgt, .N],
                src_yr = yrs[i - 1], tgt_yr = yrs[i]
            )
            
            link_dt <- rbind(link_dt, tmp_row)
        }
    }
}

link_dt[, ":=" (
    source_name = paste(src_yr, source, sep = "_"), 
    target_name = paste(tgt_yr, target, sep = "_")
)]

nodes <- data.table(
    name = unique(unlist(unique(link_dt[, .(source_name, target_name)])))
)
nodes$group <- sub("[0-9]+_", "", nodes$name)

link_dt[, ":=" (
    source_idx = match(source_name, nodes$name) - 1,
    target_idx = match(target_name, nodes$name) - 1
)]
link_dt$source_group <- sapply(link_dt$source_name, function(x) {
    strsplit(x, "_")[[1]][2]
})


# Customize color
# cols <- RColorBrewer::brewer.pal(length(uni_owner), "Paired")
# sort_owner_type <- levels(as.factor(col_dt$OwnerType))
# cols <- col_dt[match(sort_owner_type, col_dt$OwnerType), "color"]

# Cannot have space in names
col_dt$OwnerType_nospace <- sapply(col_dt$OwnerType, function(x) {
    gsub(" ", "_", x)
})
nodes$group <- sapply(nodes$group, function(x) {
    gsub(" ", "_", x)
})
link_dt$source <- sapply(link_dt$source, function(x) {
    gsub(" ", "_", x)
})
link_dt$target <- sapply(link_dt$target, function(x) {
    gsub(" ", "_", x)
})
link_dt$source_group <- sapply(link_dt$source_group, function(x) {
    gsub(" ", "_", x)
})


my_color <- paste0(
    "d3.scaleOrdinal() ",
    paste0(
        ".domain(",
        jsonlite::toJSON(col_dt$OwnerType_nospace),
        ")"
    ),
    paste0(
        " .range(",
        jsonlite::toJSON(col_dt$color),
        ")"
    )
)


p <- sankeyNetwork(
    Links = link_dt, Nodes = nodes,
    Source = "source_idx", Target = "target_idx",
    Value = "value", NodeID = "group",
    NodeGroup = "group", 
    fontSize = 12,
    sinksRight = FALSE,
    colourScale = my_color
)

# This JS function makes the link colors the same as the source node colors
javascript_string <- 'function(el) {
    d3.select(el).selectAll(".link").style("stroke", d => d.source.color);
}'
# Apply the JS function
p <- onRender(x = p, jsCode = javascript_string)

# out:
saveWidget(p, file = file.path(figdir, "owner_change_sankey.html"))



# ~ Clean Sankey ####
# ~ ----------------------------------------------------------------------------
# Since the full sankey diagram is a bit complex, here I make a cleaner version
# with just 1995, 2010, and 2024.
link_dt2 <- NULL
focus_yrs <- c(1995, 2005, 2010, 2014)
focus_yrs_idx <- which(yrs %in% focus_yrs)
for (i in 2:length(focus_yrs)) {
    pre_yr <- hm_change[[focus_yrs_idx[i - 1]]]
    the_yr <- hm_change[[focus_yrs_idx[i]]]

    tmp <- data.table(
        pre_yr$OwnerType, the_yr$OwnerType
    )
    tmp <- na.omit(tmp)

    for (src in uni_owner) {
        for (tgt in uni_owner) {
            tmp_row <- data.table(
                source = src, target = tgt,
                value = tmp[V1 == src & V2 == tgt, .N],
                src_yr = focus_yrs[i - 1], tgt_yr = focus_yrs[i]
            )

            link_dt2 <- rbind(link_dt2, tmp_row)
        }
    }
}

link_dt2[, ":="(
    source_name = paste(src_yr, source, sep = "_"),
    target_name = paste(tgt_yr, target, sep = "_")
)]

nodes2 <- data.table(
    name = unique(unlist(unique(link_dt2[, .(source_name, target_name)])))
)
nodes2$group <- sub("[0-9]+_", "", nodes2$name)

link_dt2[, ":="(
    source_idx = match(source_name, nodes2$name) - 1,
    target_idx = match(target_name, nodes2$name) - 1
)]
link_dt2$source_group <- sapply(link_dt2$source_name, function(x) {
    strsplit(x, "_")[[1]][2]
})


# Cannot have space
nodes2$group <- sapply(nodes2$group, function(x) {
    gsub(" ", "_", x)
})
link_dt2$source <- sapply(link_dt2$source, function(x) {
    gsub(" ", "_", x)
})
link_dt2$target <- sapply(link_dt2$target, function(x) {
    gsub(" ", "_", x)
})
link_dt2$source_group <- sapply(link_dt2$source_group, function(x) {
    gsub(" ", "_", x)
})

p2 <- sankeyNetwork(
    Links = link_dt2, Nodes = nodes2,
    Source = "source_idx", Target = "target_idx",
    Value = "value", NodeID = "group",
    NodeGroup = "group",
    fontSize = 16,
    sinksRight = FALSE,
    colourScale = my_color
)

# This JS function makes the link colors the same as the source node colors
javascript_string <- 'function(el) {
    d3.select(el).selectAll(".link").style("stroke", d => d.source.color);
}'
# Apply the JS function
p2 <- onRender(x = p2, jsCode = javascript_string)

# out:
saveWidget(p2, file = file.path(figdir, "owner_change_sankey_clean.html"))



# ----------------------------------------------------------------------------
# Some numbers

# 1. How much land changed from Industrial to TIMO from 1995 to 2005?
# ind2timo_area_1 <- link_dt[
#     source == "Industrial" & target == "REIT/TIMO" & 
#     src_yr == 1995 & tgt_yr == 2000, 
#     value * 200^2 / 1e6 # km^2
# ]

# ind2timo_area_2 <- link_dt[
#     source == "Industrial" & target == "REIT/TIMO" & 
#     src_yr == 2000 & tgt_yr == 2005, 
#     value * 200^2 / 1e6 # km^2
# ]

# ind2timo_area <- link_dt2[
#     source == "Industrial" & target == "REIT/TIMO" & 
#     src_yr == 1995 & tgt_yr == 2005, 
#     value * 200^2 / 1e6 # km^2
# ]

# NOTE: The `owner_areas_dt` comes from `src/mod_owner_change_shp.R`
# ind_area_1995 <- owner_areas_dt[ownerType == "Industrial" & yr == 1995, area]

# ind2timo_area_1 / ind_area_1995
# ind2timo_area_2 / ind_area_1995
# ind2timo_area / ind_area_1995

# # 2. How much land changed from TIMO to New Family from 2010 to 2014?
# timo2newfam <- link_dt[
#     source == "REIT/TIMO" & target == "New Family" &
#     src_yr == 2010 & tgt_yr == 2014,
#     value * 200^2 / 1e6
# ]
# timo_area_2010 <- owner_areas_dt[ownerType == "REIT/TIMO" & yr == 2010, area]

# timo2newfam / timo_area_2010

# owner_areas_dt[ownerType == "REIT/TIMO" & yr %in% c(2010, 2014)]

# data_mat_dt[ownerType == "REIT/TIMO", (`2010` - `2014`) / `2010`]

# ----------------------------------------------------------------------------


