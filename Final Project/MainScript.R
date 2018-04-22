library(ggplot2)
library(fpp)

# Sink console output to file
sink(file = "ProjectOutput.txt", append = FALSE, type = "output")


# Import data
source("ImportData.R")
# auto.ARIMA tests
source("ARIMA.R")
# Holt's Exponential Smoothing Model
source("Holt.R")
# Simple Exponential Smoothing Model
source("SES.R")

closeAllConnections()