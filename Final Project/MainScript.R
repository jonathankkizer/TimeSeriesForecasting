library(ggplot2)
library(fpp)

# Sink console output to file
sink(file = "ProjectOutput.txt", append = FALSE, type = "output")

cat("\n-----------------------BEGIN ANALYSIS-----------------------\n")

# Import data
source("ImportData.R")
# auto.ARIMA tests
source("ARIMA.R")
# Holt's Exponential Smoothing Model
source("Holt.R")
# Simple Exponential Smoothing Model
source("SES.R")
# GARCH Model
source("GARCH.R")
# ARMAGARCH Model -- using rugarch package
source("ARMAGARCH.R")
cat("\n-----------------------END ANALYSIS-----------------------\n")
closeAllConnections()