forecastTable$LogData <- log(forecastTable$Sales)
logIndex <- ncol(forecastTable)


# change frequency for data type
logDataTableTimeSeries <- ts(forecastTable[,logIndex], frequency = 4)
logDataTableTimeSeriesComponents <- stl(logDataTableTimeSeries, s.window = 7)

seasonal <- logDataTableTimeSeriesComponents[,1]
forecastTable$Seasonal <- seasonal

# LogA is the deseasonalized data
forecastTable$LogA <- (forecastTable$LogData - forecastTable$Seasonal)
forecastTable$A <- exp(forecastTable$LogA)