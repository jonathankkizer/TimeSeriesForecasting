library(ggplot2)
library(fpp)

file_data <- "STA372_Homework7_Question1.txt"
data_table <- read.table(file_data, header = FALSE, sep = "")
colnames(data_table) <- c("Time", "Sales", "seas_adj_Sales", "seasonal_Indices")

data_table$LogSales <- log(data_table$seas_adj_Sales)

y_time_series <- ts(data_table[,5])

result <- holt(y_time_series, h = 5)
print(summary(result))

holts_inSampleForecastActual <- ggplot() +
  geom_point(aes(x = data_table$Time, y = data_table$LogSales, color = "Actual")) +
  geom_point(aes(x = data_table$Time, y = result$fitted, color = "Model")) +
  geom_line(aes(x = data_table$Time, y = data_table$LogSales, color = "Actual")) +
  geom_line(aes(x = data_table$Time, y = result$fitted, color = "Model")) +
  ylab("Actual and Model Data") + xlab("Time") + ggtitle("Actual & Model Data vs. Time") +
  scale_y_continuous()
ggsave("holts_inSampleComparison.png", last_plot())

data_table$Forecast <- result$fitted
data_table$Residuals <- result$residuals

data_table$ForecastWithSeasonality <- exp(data_table$Forecast) * data_table$seasonal_Indices
ForecastVSales <- ggplot() +
  geom_point(aes(x = data_table$Time, y = data_table$Sales, color = "Actual")) +
  geom_point(aes(x = data_table$Time, y = data_table$ForecastWithSeasonality, color = "Model")) +
  geom_line(aes(x = data_table$Time, y = data_table$Sales, color = "Actual")) +
  geom_line(aes(x = data_table$Time, y = data_table$ForecastWithSeasonality, color = "Model")) +
  ylab("Actual and Model Data") + xlab("Time") + ggtitle("Actual & Model Data vs. Time w/ Seasonality") +
  scale_y_continuous()
ggsave("holts_ForecastVSales.png", last_plot())

seasonalIndicesForecast <- c(data_table$seasonal_Indices[24:27], data_table$seasonal_Indices[24])
extendedTime <- c(28, 29, 30, 31, 32)
Prediction <- data.frame(extendedTime, result$mean, result$lower, result$upper, seasonalIndicesForecast)
Prediction$SeasonalizedForecast <- exp(Prediction$result.mean) * Prediction$seasonalIndicesForecast
Prediction$Lower80Interval <- exp(Prediction$X80.) * Prediction$seasonalIndicesForecast
Prediction$Upper80Interval <- exp(Prediction$X80..1) * Prediction$seasonalIndicesForecast

print(Prediction)

forecastModelPlot <- ggplot() +
  geom_point(aes(x = data_table$Time, y = data_table$ForecastWithSeasonality, color = "In-sample Model")) +
  geom_line(aes(x = data_table$Time, y = data_table$ForecastWithSeasonality, color = "In-sample Model")) +
  geom_point(aes(x = data_table$Time, y = data_table$Sales, color = "Actual Data")) +
  geom_line(aes(x = data_table$Time, y = data_table$Sales, color = "Actual Data")) +
  geom_point(aes(x = Prediction$extendedTime, y = Prediction$SeasonalizedForecast, color = "Forecast")) +
  geom_line(aes(x = Prediction$extendedTime, y = Prediction$SeasonalizedForecast, color = "Forecast")) +
  scale_y_continuous() +
  xlab("Time") +
  ylab("Sales & Modeled/Forecast Sales") +
  ggtitle("Actual Data & Model vs Time")
ggsave("holts_ForecastModelPlot.png", last_plot())

holtsResiduals <- ggplot() +
  geom_point(aes(x = data_table$Time, y = data_table$Residuals)) +
  geom_line(aes(x = data_table$Time, y = data_table$Residuals)) +
  geom_hline(aes(yintercept = 0)) +
  scale_y_continuous() +
  ylab("Residuals with Holt's Model") +
  xlab("Time") +
  ggtitle("Residuals vs. Time")
ggsave("holts_modelResiduals.png", last_plot())
