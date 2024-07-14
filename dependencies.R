if (!require("BiocManager", quietly = TRUE)) {
    install.packages("BiocManager")
}

BiocManager::install("M3C")
install.packages("shiny")
install.packages("here")
install.packages("bslib")
install.packages("readr")
install.packages("plotly")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("tsne")
