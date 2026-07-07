# ******************************************************************************
# Calculate FIA plot shortest distances to available mills during the
# measurement periods through a road network.
# 
# Author: Xiaojie Gao
# Date: 2026-05-12
# ******************************************************************************
rm(list = ls())
library(data.table)
library(readxl)
library(dplyr)
library(sf)
library(sfnetworks)
library(tidygraph)
library(parallel)


# Make a road net
MakeRoadNet <- function(net_rds_file) {
    road_shp <- st_read("Y:/Plisinski/LSOG/Raw_Data/TIGER_Roads_2024_ME/Merged_Roads_2024_ME.shp")

    # Use only roads for cars
    road_shp <- road_shp[
        road_shp$MTFCC %in% c(
            "S1100", "S1200", "S1400", "S1500", "S1630", "S1640", "S1740"
        ),
    ]
    road_shp <- st_make_valid(road_shp)
    road_shp <- st_simplify(road_shp)
    geom <- st_geometry(road_shp)
    is_multi <- st_geometry_type(geom) == "MULTILINESTRING"
    geom[is_multi] <- st_line_merge(geom[is_multi])
    st_geometry(road_shp) <- geom
    road_shp <- st_cast(road_shp, "LINESTRING")


    net <- as_sfnetwork(road_shp, directed = FALSE) %>%
        activate("edges") %>%
        mutate(weight = as.numeric(st_length(geometry)))

    # Remove loop edges and multiple edges
    net <- net %>%
        activate("edges") %>%
        arrange(as.numeric(st_length(geometry))) %>%
        filter(!edge_is_multiple()) %>%
        filter(!edge_is_loop())

    net <- net %>%
        convert(to_spatial_subdivision)

    saveRDS(net, net_rds_file)
}


# Calculate shorted road distance to a mill
CalShotestRoadDist2Mill <- function(plotid, fia_harv_dt, net_rds_file, mills_dt, cachedir) {
    outfile <- file.path(cachedir, paste0(plotid, ".csv"))
    if (file.exists(outfile)) {
        return(fread(outfile))
    }

    plot_measyr <- fia_harv_dt[concatPlot == plotid, MEASYEAR]
    plot_xy <- fia_harv_dt[concatPlot == plotid, .(x, y)][1, ] %>%
        unlist()
    
    stopifnot(file.exists(net_rds_file))
    net <- readRDS(net_rds_file)

    edges <- st_as_sf(net, "edges")

    plot_pt <- st_sfc(st_point(plot_xy), crs = 4326)
    plot_pt <- st_transform(plot_pt, st_crs(net))

    res <- NULL
    for (measyr in plot_measyr) {
        # Filter the mills availabe
        mills_available <- mills_dt[
            (Year_opened < measyr & Year_closed > measyr) |
                (Year_opened == "unknown" & Open == 1) |
                (Year_opened <= measyr & is.na(Year_closed) & Open == 1),
        ]
        mills_sf <- st_as_sf(mills_available, coords = c("LON", "LAT"), crs = 4326)
        mills_sf <- st_transform(mills_sf, st_crs(net))

        # paths <- st_network_paths(net, from = plot_pt, to = mills_sf, weights = "weight")
        # cost_mat <- st_network_cost(
        #     net,
        #     from = plot_pt,
        #     to = mills_sf,
        #     weights = "weight"
        # )
        # shortest_dist <- min(cost_mat)
        paths <- st_network_paths(net, from = plot_pt, to = mills_sf)
        path_dists <- sapply(paths$edge_paths, \(edge_ids) {
            sum(st_length(edges[edge_ids, ]))
        })
        shortest_dist <- min(path_dists)

        arow <- data.table(
            concatPlot = plotid,
            MEASYEAR_aligned = measyr,
            shortest_dist = shortest_dist
        )
        res <- rbind(res, arow)
    }

    fwrite(res, outfile)

    return(res)
}



pipedir <- "pipe/02_harvest_practices"
dir.create(pipedir, showWarnings = FALSE, recursive = TRUE)


fia_harv_dt <- fread(file.path(pipedir, "fia_harvest_per_owner.csv"))
uniqueN(fia_harv_dt$concatPlot)


net_rds_file <- file.path(pipedir, "road_net_rds.Rds")
if (!file.exists(net_rds_file)) {
    MakeRoadNet(net_rds_file)
}



mills_dt <- read_excel(
    "data/matching_covariates/Maine Forest Mills - 30Apr26.xlsx", 
    "Mills"
) %>%
    as.data.table() %>%
    .[, ":="(Notes = NULL, Source_1 = NULL, Source_2 = NULL)]



tstart <- Sys.time()

cl <- makeCluster(40)
calls <- clusterCall(cl, function() {
    suppressMessages({
        library(data.table)
        library(dplyr)
        library(sf)
        library(sfnetworks)
        library(tidygraph)
    })
})


cachedir <- file.path(pipedir, "dist2mills_cache")
dir.create(cachedir, showWarnings = FALSE, recursive = TRUE)

plot2mill_dist <- clusterApplyLB(
    cl, 
    x = unique(fia_harv_dt$concatPlot), 
    fun = CalShotestRoadDist2Mill, 
    fia_harv_dt = fia_harv_dt, 
    net_rds_file = net_rds_file,
    mills_dt = mills_dt,
    cachedir = cachedir
)
stopCluster(cl)

plot2mill_dist <- rbindlist(plot2mill_dist)

# out: 
fwrite(plot2mill_dist, file.path(pipedir, "plot2mill_dist.csv"))

tend <- Sys.time()
ttake <- tend - tstart
message(round(ttake, 2), " ", units(ttake))


# paths %>%
#     slice(1) %>%
#     pull(node_paths) %>%
#     unlist()

# png("zzz.png", width = 800, height = 1200)
# plot_path <- function(node_path) {
#     net %>%
#         activate("nodes") %>%
#         slice(node_path) %>%
#         plot(cex = 1, lwd = 1, add = TRUE)
# }
# plot(net, col = "grey", lwd = 0.1, cex = 0.3)

# paths %>%
#     pull(node_paths) %>%
#     walk(plot_path)
# plot(p1, pch = 16, col = "red", cex = 2, add = TRUE)
# plot(mills, col = "blue", pch = 8, cex = 2, lwd = 2, add = TRUE)
# dev.off()