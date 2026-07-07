# ******************************************************************************
# Assign a unique color to each owner type for visualization.
# 
# Author: Xiaojie Gao
# Date: 2025-02-05
# ******************************************************************************

# cols <- RColorBrewer::brewer.pal(12, "Paired")

col_dt <- data.table::data.table(
    OwnerType = c(
        "Industrial", "REIT/TIMO", 
        "Non-Profit Conservation", "Non-Profit Other", 
        "Contractor", 
        "Old Family", "Family Forest", "New Family",
        "Municipal", "State", "Federal", "Tribal", 
        "Other",
        "Small Family",
        "Unknown"
    ),
    color = c(
        "#1F78B4", "#E31A1C",
        "#B2DF8A", "#33A02C", 
        "#FB9A99",
        "#B15928", "#FDBF6F", "#FF7F00",
        "#CAB2D6", "#6A3D9A", "#FFFF99", "#A6CEE3",
        "#4d4d4d",
        "grey50",
        "grey"
    )
)
