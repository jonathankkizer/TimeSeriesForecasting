library(ggplot2)
library(fpp)
cat("\n--------ARIMA Model Analysis--------\n")
CCITimeSeries <- ts(data_table$CCI)
autoResult <- auto.arima(CCITimeSeries, ic = "aicc", test = "adf", stepwise = FALSE, approximation = FALSE, trace = TRUE)
print(autoResult)

autoArimaModel <- ggplot() +
  geom_point(aes(x = data_table$Time, y = data_table$CCI), color = "red") +
  geom_line(aes(x = data_table$Time, y = data_table$CCI), color = "red") +
  geom_point(aes(x = data_table$Time, y = autoResult$fitted), color = "blue") +
  geom_line(aes(x = data_table$Time, y = autoResult$fitted), color = "blue") +
  xlab("Time") + ylab("CCI (Red) & ARIMA Model (Blue)") + ggtitle("CCI & ARIMA Model vs. Time") +
  scale_y_continuous()
ggsave("ARIMA_v_CCI.png", last_plot())

autoArimaModelResiduals <- ggplot() +
  geom_point(aes(x = data_table$Time, y = autoResult$residuals), color = "blue") +
  geom_line(aes(x = data_table$Time, y = autoResult$residuals), color = "blue") +
  xlab("Time") + ylab("Residuals") + ggtitle("Residuals vs. Time") + scale_y_continuous()
ggsave("ARIMA_Residuals_v_Time.png", last_plot())

autoARIMAHistogram <- ggplot() +
  geom_histogram(aes(autoResult$residuals), binwidth = .05) +
  ggtitle("Histogram of Auto ARIMA Residuals, Binwidth = .05") +
  scale_y_continuous()
ggsave("ARIMA_Residuals_Histogram.png", last_plot())

print(adf.test(x = autoResult$residuals))

png("ARIMA_ACF.png")
plot(acf(autoResult$residuals))
dev.off()

png("ARIMA_PACF.png")
plot(pacf(autoResult$residuals))
dev.off()

data_table$ARIMA_Fitted <- autoResult$fitted
data_table$ARIMA_Residuals <- autoResult$residuals

ARIMAForecast <- forecast(autoResult, h = 9)
print(ARIMAForecast)