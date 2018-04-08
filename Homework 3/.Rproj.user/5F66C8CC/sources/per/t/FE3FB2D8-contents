library(ggplot2)

file_output <- "Homework3Q3.txt"
#sink(file_output, append=FALSE, split=TRUE)

# read into dataframe sales data, create log(Sales) column
file <- "STA372_Homework3_Question3.dat.txt"
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
ggsave("Q3LogSalesVTime.png", last_plot())

print(head(Sales_table))

LogSales_time_series <- ts(Sales_table[,5], frequency = 4)

fit <- stl(LogSales_time_series, s.window = 7)
png("Q3stlFit.png")
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
ggsave("Q3salesVDeseasonalizedSales.png", last_plot())

Q3logAvsTime <- ggplot() +
  geom_line(aes(x = Sales_table$Time, y = Sales_table$LogA), color = "blue") +
  geom_point(aes(x = Sales_table$Time, y = Sales_table$LogA), color = "black") +
  scale_y_continuous() +
  xlab("Time") +
  ylab("log(A)") +
  ggtitle("log(A) vs. Time")
ggsave("Q3logAvsTime.png", last_plot())

Q3AvsTime <- ggplot() +
  geom_line(aes(x = Sales_table$Time, y = Sales_table$A), color = "blue") +
  scale_y_continuous() +
  xlab("Time") +
  ylab("A") +
  ggtitle("A vs. Time")
ggsave("Q3AvsTime.png", last_plot())

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
ggsave("Q3forecastLogAvsLogA.png", last_plot())

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
ggsave("Q3forecastSalesvSales.png", last_plot())

residualsVTime <- ggplot() +
  geom_line(aes(x = Sales_table$Time, y = reg_output$residuals), color = "blue") +
  geom_point(aes(x = Sales_table$Time, y = reg_output$residuals), color = "black") +
  geom_hline(yintercept = 0) +
  scale_y_continuous() +
  xlab("Time") +
  ylab("Residuals") +
  ggtitle("Residuals vs. Time")
ggsave("Q3ResidualsVTime.png", last_plot())

residuals <- reg_output$residuals
acf_residuals <- acf(residuals)
print(acf_residuals)
png("Q3SeriesResidualsACF.png")
plot(acf_residuals)
dev.off()

print(tail(Sales_table_extended))

closeAllConnections()