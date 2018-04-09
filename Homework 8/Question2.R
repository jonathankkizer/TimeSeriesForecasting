library(ggplot2)
library(fpp)

file_data <- "STA372_Homework8_Question2.dat.txt"
data_table <- read.table(file_data, header = FALSE, sep = "")
colnames(data_table) <- c("Month", "Yields")

yieldsVTime <- ggplot() +
  geom_point(aes(x = data_table$Month, y = data_table$Yields, color = "Yield")) +
  geom_line(aes(x = data_table$Month, y = data_table$Yields, color = "Yield")) +
  xlab("Time") + ylab("Yield") + ggtitle("Yield vs. Month") + scale_y_continuous()
ggsave("q2_yieldsVStime.png", last_plot())

acfYield <- acf(data_table$Yields)
print(acfYield)
png("q2_acfYields.png")
plot(acfYield)
dev.off()
print(adf.test(data_table$Yields))
print(adf.test(data_table$Yields, k = 12))

Diff_Yields <- diff(data_table$Yields, difference = 1)
data_table$Diff_Yields[1] <- NA
data_table$Diff_Yields[2:125] <- Diff_Yields
print(head(data_table))

diffYieldsVTime <- ggplot() +
  geom_point(aes(x = data_table$Month, y = data_table$Diff_Yields), color = "black") +
  geom_line(aes(x = data_table$Month, y = data_table$Diff_Yields), color = "blue") +
  geom_hline(aes(yintercept = 0)) +
  xlab("Time") + ylab("First Differences of Yields") + ggtitle("First Diffs of Yields vs. Month") + scale_y_continuous()
ggsave("q2_firstDiffYieldsVMonth.png", last_plot())

diffACFYield <- acf(data_table$Diff_Yields[2:125])
png("q2_DiffACFYields.png")
plot(diffACFYield)
dev.off()
print(adf.test(data_table$Diff_Yields[2:125]))
print(adf.test(data_table$Diff_Yields[2:125], k = 12))

y_time_series <- ts(data_table[,2])
result <- Arima(y_time_series, order = c(0, 1, 1), include.constant=TRUE)
cat("\nEstimate of Alpha is...\n")
print(result$coef)
cat("\nEstimate of sigma^2 (not sigma) is... \n")
print(result$sigma)
cat("\nFive step ahead forecasts and confidence intervals...\n")
print(forecast(result, h = 5))
cat("\n\nResults Printout:\n")
print(result)

data_table$MA1_Model <- result$fitted
data_table$MA1_Residuals <- result$residuals

acf_residuals <- acf(result$residuals)
print(acf_residuals)
png("q2_MA1ResidualsACF.png")
plot(acf_residuals)
dev.off()

print(head(data_table))

modelVactual <- ggplot() +
  geom_point(aes(x=data_table$Month, y=data_table$MA1_Model)) +
  geom_line(aes(x=data_table$Month, y=data_table$MA1_Model, color = "MA(1) Model")) +
  geom_point(aes(x=data_table$Month, y=data_table$Yields)) +
  geom_line(aes(x=data_table$Month, y=data_table$Yields, color = "Actual Data")) +
  xlab("Time") + ylab("In-sample Data & Model") + ggtitle("In-sample Data & Model vs. Time") + scale_y_continuous()
ggsave("q2_MA(1)Vactual.png", last_plot())

errorPlot <- ggplot() +
  geom_point(aes(x=data_table$Month, y=data_table$MA1_Residuals), color = "black") +
  geom_line(aes(x=data_table$Month, y=data_table$MA1_Residuals), color = "blue") +
  geom_hline(aes(yintercept = 0)) +
  xlab("Time") + ylab("Residuals") + ggtitle("Residuals vs. Time") + scale_y_continuous()
ggsave("q2_MA(1)Residuals.png", last_plot())