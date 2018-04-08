library(fpp)
file_data <- "STA372_Homework7_Question1.txt"
data_table <- read.table(file_data, header = FALSE, sep = "")
colnames(data_table) <- c("Time", "Sales", "seas_adj_Sales", "seasonal_Indices")

data_table$LogSales <- log(data_table$seas_adj_Sales)

y_time_series <- ts(data_table[3])
y_log_time_series <- ts(data_table[4])
colnames(y_time_series) <- c("Y")
colnames(y_time_series) <- c("LogY")

acf_coef <- acf(y_time_series)
#pacf_coef <- pacf(y_time_series)
print(acf_coef)
png("LaggedY_Autocorrelation Coefficients.png")
plot(acf_coef)
dev.off()
#print(pacf_coef)

data_table$LogSales_lag1[2:27] <- data_table$LogSales[1:26]
is.na(data_table$LogSales_lag1[1])
print(head(data_table))

reg_output_lag <- lm(LogSales ~ LogSales_lag1 + Time, data_table)
print(summary(reg_output_lag))


blah <- c(0, reg_output_lag$fitted.values)
data_table$forecastValues <- blah
is.na(data_table$forecastValues[1])
laggedYVsActual <- ggplot() +
  geom_line(aes(x = data_table$Time[2:27], y = data_table$LogSales, color = "Actual Log(Sales)")) +
  geom_line(aes(x = data_table$Time[2:27], y = data_table$forecastValues, color = "Regression Fitted Values"))
ggsave("LaggedY_Actual.png", last_plot())

residuals_lag <- reg_output_lag$residuals
acf_residuals_lag <- acf(residuals_lag)
print(acf_residuals_lag)
png("LaggedY_residualACF.png")
plot(acf_residuals_lag)
dev.off()

residualsLagVTime <- ggplot() +
  geom_point(aes(x = data_table$Time[2:27], y = residuals_lag)) +
  geom_line(aes(x = data_table$Time[2:27], y = residuals_lag)) +
  geom_hline(aes(yintercept = 0)) +
  xlab("Time") + ylab("Lagged Residuals") + ggtitle("Time vs. Lagged Residuals")
ggsave("LaggedY_LagResidualsVTime.png", last_plot())