library(ggplot2)
library(fpp)

file_data <- "STA372_Homework10_Question1.dat.txt"
data_table <- read.table(file_data, header = FALSE, sep = "")
colnames(data_table) <- c("Quarter", "Consumption")

consumptionVTimePlot <- ggplot() +
  geom_point(aes(x = data_table$Quarter, y = data_table$Consumption), color = "black") +
  geom_line(aes(x = data_table$Quarter, y = data_table$Consumption), color = "blue") +
  xlab("Quarter") + ylab("Consumption") + scale_y_continuous() + ggtitle("Consumption vs. Quarter")
ggsave("q1_consumptionVQuarter.png", last_plot())

firstDiffs <- diff(data_table$Consumption, difference = 1)
data_table$firstDiffs[1] <- NA
data_table$firstDiffs[2:164] <- firstDiffs

firstDiffsVTimePlot <- ggplot() +
  geom_point(aes(x = data_table$Quarter, y = data_table$firstDiffs), color = "black") +
  geom_line(aes(x = data_table$Quarter, y = data_table$firstDiffs), color = "blue") +
  xlab("Quarter") + ylab("First Differences Consumption") + scale_y_continuous() + ggtitle("First Differences Consumption vs. Quarter")
ggsave("q1_firstDiffsConsumptionVTime.png", last_plot())

firstDiffsTimeSeries <- ts(data_table$firstDiffs[2:164])
print(adf.test(x = firstDiffsTimeSeries))

png("q1_firstDiffsPACF.png")
plot(pacf(data_table$firstDiffs[2:164]))
dev.off()


png("q1_firstDiffsACF.png")
plot(acf(data_table$firstDiffs[2:164]))
dev.off()

result <- Arima(firstDiffsTimeSeries, order=c(3,0,0), include.constant = TRUE)
print(result)

AR3ResidualsPlot <- ggplot() +
  geom_point(aes(x = data_table$Quarter[2:164], y = result$residuals), color="black") +
  geom_line(aes(x = data_table$Quarter[2:164], y = result$residuals), color = "blue") +
  xlab("Quarters") + ylab("AR(3) First Diffs Residuals") + ggtitle("Residuals vs. Quarters") + scale_y_continuous()
ggsave("q1_firstDiffsAR3Residuals.png", last_plot())

png("q1_AR3ResidualsPACF.png")
plot(pacf(result$residuals))
dev.off()


png("q1_AR3ResidualsACF.png")
plot(acf(result$residuals))
dev.off()

consumptionTS <- ts(data_table$Consumption)
autoResult <- auto.arima(consumptionTS, ic = "aicc", test = "adf", stepwise = FALSE, approximation = FALSE, trace = TRUE)
print(autoResult)

autoResultStepwise <- auto.arima(consumptionTS, ic = "aicc", test = "adf", stepwise = TRUE, approximation = FALSE, trace = TRUE)
print(autoResultStepwise)

autoResultForecast <- forecast(autoResult, h = 2)
print(autoResultForecast)