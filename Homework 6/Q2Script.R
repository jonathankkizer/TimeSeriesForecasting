library(ggplot2)
library(fpp)

file <- "STA372_Homework6_Question2.dat.txt"

data_table <- read.table(file, header = FALSE, sep = "")

colnames(data_table) <- c("Time", "Quarter", "Sales")

data_table$logSales <- log(data_table$Sales)

timeVlogSales <- ggplot() +
  geom_point(aes(x = data_table$Time, y = data_table$logSales), color = "black") +
  geom_line(aes(x = data_table$Time, y = data_table$logSales), color = "blue") +
  scale_y_continuous() +
  xlab("Time") +
  ylab("Log(Sales)") +
  ggtitle("Log(Sales) vs. Time")
ggsave("Q2timeVLogSales.png", last_plot())

Log_Sales_time_series <- ts(data_table[,4], frequency = 4)

print(head(Log_Sales_time_series))

fit <- stl(Log_Sales_time_series, s.window = 7)
png("Q2stlFit.png")
plot(fit)
dev.off()

data_table$Seasonal <- fit$time.series[,1]
data_table$LogA <- data_table$logSales - data_table$Seasonal
data_table$A <- exp(data_table$LogA)

print(data_table)

seasonalIndicesVTime <- ggplot() +
  geom_point(aes(x = data_table$Time, y = data_table$Seasonal), color = "black") +
  geom_line(aes(x = data_table$Time, y = data_table$Seasonal), color = "#66CCCC") +
  ylab("Seasonal Indices") +
  xlab("Time") +
  ggtitle("Seasonal Indices vs. Time") +
  scale_y_continuous()
ggsave("seasonalVTime.png", last_plot())

AVsTime <- ggplot() +
  geom_point(aes(x = data_table$Time, y = data_table$A), color = "black") +
  geom_line(aes(x = data_table$Time, y = data_table$A), color = "blue") +
  xlab("Time") +
  ylab("A") +
  scale_y_continuous() +
  ggtitle("A(t) vs. Time") 
ggsave("aVsTime.png", last_plot())

y_time_series <- ts(data_table[,7])
print(data_table[,7])

result <- (holt(y_time_series, h = 4))
print(result)
print(result$model)

data_table$forecasts <- result$fitted
data_table$residuals <- result$residuals

seasonallyAdjSalesVsForecast <- ggplot() +
  geom_point(aes(x = data_table$Time, y = data_table$A), color = "red") +
  geom_point(aes(x = data_table$Time, y = data_table$forecasts), color = "blue") +
  geom_line(aes(x = data_table$Time, y = data_table$A), color = "red") +
  geom_line(aes(x = data_table$Time, y = data_table$forecasts), color = "blue") +
  scale_y_continuous() +
  ggtitle("Adjusted Sales vs. Forecasts") +
  ylab("Adjusted Sales (red) & Forecast Sales (blue)") +
  xlab("Time")
ggsave("Q2AvsForecast.png", last_plot())

residualsVsTime <- ggplot() +
  geom_point(aes(x = data_table$Time, y = data_table$residuals), color = "blue") +
  geom_line(aes(x = data_table$Time, y = data_table$residuals), color = "blue") +
  ylab("Residuals") +
  xlab("Time") +
  ggtitle("Residuals vs. Time") +
  geom_hline(aes(yintercept = 0)) +
  scale_y_continuous()
ggsave("Q2ResidualsVsTime.png", last_plot())

acf_residuals <- acf(data_table$residuals)
print(acf_residuals)
png("Q2ACFResiduals.png")
plot(acf_residuals)
dev.off()

print(exp(log(result$mean[1]) + data_table$Seasonal[58]))
print(exp(log(result$mean[2]) + data_table$Seasonal[59]))
print(exp(log(result$mean[3]) + data_table$Seasonal[60]))
print(exp(log(result$mean[4]) + data_table$Seasonal[61]))

closeAllConnections()

