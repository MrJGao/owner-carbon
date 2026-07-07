# ******************************************************************************
# Use logistic model to get harvest rates, and summarize intensity.
# 
# Author: Xiaojie Gao
# Date: 2026-04-02
# ******************************************************************************
rm(list = ls())
library(data.table)
library(magrittr)
library(likelihood)



pipedir <- "pipe/02_harvest_practices"
dir.create(pipedir, showWarnings = FALSE, recursive = TRUE)



fia_harv_dt <- fread(file.path(pipedir, "fia_harvest_per_owner.csv"))

# Remove records with owner type changes
model_data <- fia_harv_dt[OwnerType == ownertype_init, ]
model_data[is.na(pc_ba_removed), pc_ba_removed := 0]


# ~ Harvest rates ####
# ~ ----------------------------------------------------------------------------
# Jonathan's modeling approach -----------------
likelihood_function <- function(x, pred) {
    ifelse(x > 0, log(pred), log(1 - pred))
}


# SINGLE MEAN NULL MODEL  (a gives the annual probabiliy of not being logged)
null_model <- function(a, R) {
    1 - (a^R)
}

par <- list(a = 0.9)
par_lo <- list(a = 0)
par_hi <- list(a = 1)
var <- list(x = "pc_ba_removed", R = "meas_gap", pred = "predicted")


harv_rates_dt <- lapply(unique(fia_harv_dt$OwnerType), \(ot) {
    owner_model_dt <- model_data[OwnerType == ot]
    if (nrow(owner_model_dt) < 10) {
        return(NULL)
    }
    results <- anneal(
        model = null_model,
        par, var,
        owner_model_dt,
        par_lo,
        par_hi,
        pdf = likelihood_function,
        dep_var = "pc_ba_removed",
        show_display = FALSE,
        max_iter = 20000
    )

    harv_prob <- 1 - results$best_pars$a
    harv_prob_std <- results$std_err$a

    arow <- data.table(
        OwnerType = ot,
        harv_prob, harv_prob_std
    )
    return(arow)
}) %>%
    rbindlist()
setorder(harv_rates_dt, -harv_prob)


# out: Harvest rates
fwrite(harv_rates_dt, file.path(pipedir, "owner_harvest_rates_dt.csv"))




# ~ Harvest intensity ####
# ~ ----------------------------------------------------------------------------
harv_int <- model_data[
    pc_ba_removed > 0,
    .(
        harv_int = mean(pc_ba_removed, na.rm = TRUE), 
        harv_int_sd = sd(pc_ba_removed, na.rm = TRUE),
        .N
    ),
    by = .(OwnerType)
]
incidental_int <- model_data[
    pc_ba_removed > 0 & pc_ba_removed < 10,
    .(
        incidental_int = mean(pc_ba_removed, na.rm = TRUE), 
        incidental_sd = sd(pc_ba_removed, na.rm = TRUE),
        .N
    ),
    by = .(OwnerType)
]

harv_int <- merge(harv_int, incidental_int, by = "OwnerType", all = TRUE)

setorder(harv_int, -harv_int)

model_data_harv <- model_data[pc_ba_removed > 0 & !OwnerType %in% c("Federal", "Municipal", "Unknown", "Other")]
pairwise.t.test(
    model_data_harv[pc_ba_removed > 0, pc_ba_removed], 
    model_data_harv[pc_ba_removed > 0, OwnerType]
)


t.test(
    model_data[pc_ba_removed > 0 & OwnerType == "Industrial", pc_ba_removed],
    model_data[pc_ba_removed > 0 & OwnerType == "REIT/TIMO", pc_ba_removed]
)

# out: Harvest intensity
fwrite(harv_int, file.path(pipedir, "owner_harvest_intensity_dt.csv"))

