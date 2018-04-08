library(ggplot2)

file_output <- "Homework3Q3.txt"
#sink(file_output, append=FALSE, split=TRUE)

# read into dataframe sales data, create log(Sales) column
file <- "STA372_Homework4_Question2.dat.txt"
Sales_table <- read.table(file, header = FALSE, sep = "")
colnames(Sales_table) <- c("Time", "Quarter", "Sales")
Sales_table$TimeSq <- Sales_table$Time^2
Sales_table$LogSales <- log(Sales_table$Sales)

logSalesVTime <- ggplot() +
  geom_point(aes(x = Sales_table$Time, y = Sales_table$LogSales), color = "black") +
  geom_line(aes(x = Sales_table$Time, y = Sales_table$LogSales), color = "blue") +
  scale_y_continuous() +
  xlab("Time") +
  ylab("Log(Sales)") +
  ggtitle("Time vs. Log(Sales)")
ggsave("Q2LogSalesVTime.png", last_plot())

print(head(Sales_table))

LogSales_time_series <- ts(Sales_table[,5], frequency = 4)

fit <- stl(LogSales_time_series, s.window = 7)
png("Q2stlFit.png")
plot(fit)
dev.off()

Sales_table$Seasonal <- fit$time.series[,1]
Sales_table$LogA <- Sales_table$LogSales - Sales_table$Seasonal
Sales_table$A <- exp(Sales_table$LogA)
print(head(Sales_table))
print(tail(Sales_table))

salesVDeseasonalizedSales <- ggplot() +
  geom_line(aes(x = Sales_table$Time, y = Sales_table$Sales), color = "red") +
  geom_line(aes(x = Sales_table$Time, y = Sales_table$A), color = "blue") + 
  xlab("Time") + 
  ggtitle("Sales & Deseasonalized Sales vs. Time")
ggsave("Q2salesVDeseasonalizedSales.png", last_plot())

Q3logAvsTime <- ggplot() +
  geom_line(aes(x = Sales_table$Time, y = Sales_table$LogA), color = "blue") +
  geom_point(aes(x = Sales_table$Time, y = Sales_table$LogA), color = "black") +
  scale_y_continuous() +
  xlab("Time") +
  ylab("log(A)") +
  ggtitle("log(A) vs. Time")
ggsave("Q2logAvsTime.png", last_plot())

Q3AvsTime <- ggplot() +
  geom_line(aes(x = Sales_table$Time, y = Sales_table$A), color = "blue") +
  scale_y_continuous() +
  xlab("Time") +
  ylab("A") +
  ggtitle("A vs. Time")
ggsave("Q2AvsTime.png", last_plot())

reg_output <- lm(Sales_table$LogA ~ Sales_table$Time + Sales_table$TimeSq)

print(summary(reg_output))

fitted_values_logA = reg_output$coef[1] + reg_output$coef[2] * Sales_table$Time + reg_output$coef[3] * Sales_table$TimeSq

forecastLogAvsLogA <- ggplot() +
  geom_line(aes(x = Sales_table$Time, y = fitted_values_logA), color = "blue") +
  geom_line(aes(x = Sales_table$Time, y = Sales_table$LogA), color = "red") +
  geom_point(aes(x = Sales_table$Time, y = Sales_table$LogA), color = "red") +
  geom_point(aes(x = Sales_table$Time, y = fitted_values_logA), color = "blue") +
  xlab("Time") +
  ylab("Fitted Values Log(A) [Blue] & Log(A) [Red]") +
  ggtitle("Fitted Values Log(A) & Log(A) vs. Time") +
  scale_y_continuous()
ggsave("Q2forecastLogAvsLogA.png", last_plot())

Time <- c(Sales_table$Time, 41:44)
TimeSq <- Time^2
Seasonal <- c(Sales_table$Seasonal, Sales_table$Seasonal[37], Sales_table$Seasonal[38], Sales_table$Seasonal[39], Sales_table$Seasonal[40])
Sales_table_extended <- data.frame(Time, TimeSq, Seasonal)
print(tail(Sales_table_extended))

Sales_table_extended$forecast_logA = reg_output$coef[1] + reg_output$coef[2] * Sales_table_extended$Time + reg_output$coef[3] * Sales_table_extended$TimeSq
Sales_table_extended$forecast_logSales = Sales_table_extended$forecast_logA + Sales_table_extended$Seasonal
Sales_table_extended$forecast_Sales = exp(Sales_table_extended$forecast_logSales)

forecastSalesvSales <- ggplot() +
  geom_line(aes(x = Sales_table_extended$Time, y = Sales_table_extended$forecast_Sales), color = "red", linetype = 2) +
  geom_line(aes(x = Sales_table$Time, y = Sales_table$Sales), color = "blue") +
  scale_y_continuous() +
  xlab("Time") +
  ylab("Forecast Sales [red, dashed] & Actual Sales[blue, solid]") +
  ggtitle("Forecast Sales & Actual Sales vs. Time")
ggsave("Q2forecastSalesvSales.png", last_plot())

residualsVTime <- ggplot() +
  geom_line(aes(x = Sales_table$Time, y = reg_output$residuals), color = "blue") +
  geom_point(aes(x = Sales_table$Time, y = reg_output$residuals), color = "black") +
  geom_hline(yintercept = 0) +
  scale_y_continuous() +
  xlab("Time") +
  ylab("Residuals") +
  ggtitle("Residuals vs. Time")
ggsave("Q2ResidualsVTime.png", last_plot())

residuals <- reg_output$residuals
acf_residuals <- acf(residuals)
print(acf_residuals)
png("Q2SeriesResidualsACF.png")
plot(acf_residuals)
dev.off()

print(tail(Sales_table_extended))

Sales_table$logA_lag[2:40] <- Sales_table$LogA[1:39]
is.na(Sales_table$logA_lag[1])
print(head(Sales_table))

reg_output_lag <- lm(LogA ~ logA_lag + Time + TimeSq, Sales_table)
print(summary(reg_output_lag))

residuals_lag <- reg_output_lag$residuals
residualsLagPlot <- ggplot() +
  geom_point(aes(x = Sales_table$Time[2:40], y = residuals_lag), color = "blue") +
  geom_line(aes(x = Sales_table$Time[2:40], y = residuals_lag), color = "blue") +
  ggtitle("Lagged Model Residuals vs. Time") +
  xlab("Time") +
  ylab("Residuals")
ggsave("Q2ResidualsLaggedModel.png", last_plot())

acf_residuals_lag <- acf(residuals_lag)
print(acf_residuals_lag)
png("Q2LaggedSeriesResidualsACF.png")
plot(acf_residuals_lag)
dev.off()

Sales_table$ForecastLagged_logA = reg_output_lag$coef[1] + reg_output_lag$coef[2] * Sales_table$logA_lag + reg_output_lag$coef[3] * Sales_table$Time + reg_output_lag$coef[4] * Sales_table$TimeSq
Sales_table$ForecastLagged_logSales = Sales_table$ForecastLagged_logA + Sales_table$Seasonal
Sales_table$ForecastLagged_Sales = exp(Sales_table$ForecastLagged_logSales)

laggedLogAPlot <- ggplot() +
  geom_point(aes(x = Sales_table$Time, y = Sales_table$LogA), color = "black") +
  geom_line(aes(x = Sales_table$Time, y = Sales_table$LogA), color = "black") +
  geom_point(aes(x = Sales_table$Time, y = Sales_table$ForecastLagged_logA), color = "blue") +
  geom_line(aes(x = Sales_table$Time, y = Sales_table$ForecastLagged_logA), color = "blue") +
  xlab("Time") +
  ylab("LogA [Black] & Forecast LogA [blue]") +
  ggtitle("Lagged Forecast LogA & Actual LogA vs. Time")
ggsave("laggedLogAvsLogA.png", last_plot())

laggedSalesvActSales <- ggplot() +
  geom_point(aes(x = Sales_table$Time, y = Sales_table$Sales), color = "black") +
  geom_line(aes(x = Sales_table$Time, y = Sales_table$Sales), color = "black") +
  geom_point(aes(x = Sales_table$Time, y = Sales_table$ForecastLagged_Sales), color = "blue") +
  geom_line(aes(x = Sales_table$Time, y = Sales_table$ForecastLagged_Sales), color = "blue") +
  xlab("Time") +
  ylab("Sales [Black] & Forecast Sales [blue]") +
  ggtitle("Lagged Forecast Sales & Actual Sales vs. Time")
ggsave("laggedSalesvActSales.png", last_plot())

period41 <- reg_output_lag$coef[1] + reg_output_lag$coef[2] * Sales_table$logA_lag[40] + reg_output_lag$coef[3] * 41 + reg_output_lag$coef[4] * (41^2)
print("Period 41:, ")
period41 <- period41 + Sales_table$Seasonal[37]
print(period41)

period42 <- reg_output_lag$coef[1] + reg_output_lag$coef[2] * period41 + reg_output_lag$coef[3] * 42 + reg_output_lag$coef[4] * (42^2)
period42 <- period42 + Sales_table$Seasonal[38]
print(period42)
closeAllConnections()