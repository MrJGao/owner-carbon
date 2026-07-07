# ******************************************************************************
# Download harnomic features created by Val's script
# 
# Author: Xiaojie Gao
# Date: 2025-02-11
# ******************************************************************************
library(parallel)



cl <- makeCluster(20)
calls <- clusterCall(cl, function() {
    suppressWarnings({
    })
})

dest_dir <- "data/raw/NE_harmonic"
dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)

# Google cloud url
url <- file.path("gs://me_features")
com <- paste("gsutil ls", url)
file_list <- system(com, intern = TRUE)

clusterExport(cl, c("dest_dir"))

null <- clusterApplyLB(cl, x = file_list, function(ff) {
    print(paste("Downloading", ff, "--------------------"))

    temp <- unlist(strsplit(ff, "/"))
    filename <- temp[length(temp)]
    full_path <- file.path(dest_dir, filename)

    if (file.exists(full_path) == FALSE) {
        system(paste("gsutil -m cp -r", ff, full_path))
    }
})
stopCluster(cl)
message("Done!!!")

