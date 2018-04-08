library(ggplot2)

columnOneTitle <- "Date"
columnTwoTitle <- "Quarter"
columnThreeTitle <- "Sales"

# selects and reads data table WITHOUT HEADERS
forecastTable <- read.csv(file.choose(), header = FALSE, sep = "")
# SETS COLUMN NAMES
colnames(forecastTable) <- c(columnOneTitle, columnTwoTitle, columnThreeTitle)

source("seasonalDecomposition.R")