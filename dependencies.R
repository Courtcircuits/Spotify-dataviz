if (!require("BiocManager", quietly = TRUE)) {
    install.packages("BiocManager", Ncpus = 6)
}

BiocManager::install("M3C", Ncpus = 6)
install.packages("shiny", Ncpus = 6)
install.packages("here", Ncpus = 6)
install.packages("bslib", Ncpus = 6)
install.packages("readr", Ncpus = 6)
install.packages("plotly", Ncpus = 6)
install.packages("ggplot2", Ncpus = 6)
install.packages("dplyr", Ncpus = 6)
install.packages("tsne", Ncpus = 6)
