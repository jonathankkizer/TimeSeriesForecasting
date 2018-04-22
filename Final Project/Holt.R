library(ggplot2)
library(fpp)
cat("\n--------Holt's Model Analysis--------\n")
data_table$HoltLogCCI <- log(data_table$CCI)

logCCIvTime <- ggplot() +
  geom_point(aes(x = data_table$Time, y = data_table$HoltLogCCI)) + 
  geom_line(aes(x = data_table$Time, y = data_table$HoltLogCCI), color = "orange") +
  xlab("Time") + ylab("Log(CCI)") + ggtitle("Log(CCI) vs. Time") + scale_y_continuous()
ggsave("Holt_LogCCIvsTime.png", last_plot())

# Seasonal check; seasonality is incredibly small in this data set
# logCCITS <- ts(data_table[,6], frequency = 12)
# holtResult <- stl(logCCITS, s.window = 7)
# plot(holtResult)
# data_table$HoltSeasonal <- holtResult$time.series[,1]

logCCIITS <- ts(data_table[,6])
holtResult <- holt(logCCIITS, h = 9)
print(holtResult$model)
print(holtResult)

data_table$HoltModelFitted <- exp(holtResult$fitted)
data_table$HoltModelResiduals <- exp(holtResult$residuals)

png("Holt_ACF.png")
plot(acf(data_table$HoltModelResiduals))
dev.off()

png("Holt_PACF.png")
plot(pacf(data_table$HoltModelResiduals))
dev.off()

HoltResidualsHistogram <- ggplot() +
  geom_histogram(aes(holtResult$residuals), binwidth = .001) +
  ggtitle("Histogram of Holt Residuals, Binwidth = .001") +
  scale_y_continuous()
ggsave("Holt_Residuals_Histogram.png", last_plot())

holtModelvsCCI <- ggplot() +
  geom_point(aes(x = data_table$Time, y = data_table$CCI), color = "red") +
  geom_line(aes(x = data_table$Time, y = data_table$CCI), color = "red") +
  geom_point(aes(x = data_table$Time, y = data_table$HoltModelFitted), color = "orange") +
  geom_line(aes(x = data_table$Time, y = data_table$HoltModelFitted), color = "orange") +
  xlab("Time") + ylab("CCI (Red) & Holt's Model (Orange)") + ggtitle("CCI & Holt's Model vs. Time") + scale_y_continuous()
ggsave("Holt_ModelvsData.png", last_plot())

holtModelResidualsvsTime <- ggplot() +
  geom_point(aes(x = data_table$Time, y = data_table$HoltModelResiduals), color = "orange") +
  geom_line(aes(x = data_table$Time, y = data_table$HoltModelResiduals), color = "orange") +
  xlab("Time") + ylab("Holt Model Residuals") + ggtitle("Holt Model Residuals vs. Time") +
  scale_y_continuous()
ggsave("Holt_ResidualsvsTime.png", last_plot())

               