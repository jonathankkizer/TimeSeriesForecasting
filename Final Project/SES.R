library(ggplot2)
library(fpp)
library(nortest)

SESTS <- ts(data_table[,3])

SESResult <- ses(SESTS, h = 9)

cat("\n--------SES Model Analysis--------\n")
print(SESResult$model)
print(SESResult)

SESvsData <- ggplot() +
  geom_point(aes(x = data_table$Time, y = data_table$CCI), color = "red") +
  geom_line(aes(x = data_table$Time, y = data_table$CCI), color = "red") +
  geom_point(aes(x = data_table$Time, y = SESResult$fitted), color = "brown") +
  geom_line(aes(x = data_table$Time, y = SESResult$fitted), color = "brown") +
  ylab("CCI (Red) & SES Model (Brown") + xlab("Time") + ggtitle("SES & Data vs Time") +
  scale_y_continuous()
ggsave("SES_v_CCI.png", last_plot())

SESResiduals <- ggplot() +
  geom_point(aes(x = data_table$Time, y = SESResult$residuals), color = "brown") +
  geom_line(aes(x = data_table$Time, y = SESResult$residuals), color = "brown") +
  ylab("SES Model Residuals") + xlab("Time") + ggtitle("SES Model Residuals vs. Time") +
  scale_y_continuous()
ggsave("SES_Model_Residuals.png", last_plot())

SESResidualsHistogram <- ggplot() +
  geom_histogram(aes(SESResult$residuals), binwidth = .05) +
  ggtitle("Histogram of SES Residuals, Binwidth = .05") +
  scale_y_continuous()
ggsave("SES_Residuals_Histogram.png", last_plot())

print(adf.test(x = SESResult$residuals))

png("SES_ACF.png")
plot(acf(SESResult$residuals))
dev.off()

png("SES_PACF.png")
plot(pacf(SESResult$residuals))
dev.off()

data_table$SESModelFitted <- SESResult$fitted
data_table$SESModelResiduals <- SESResult$residuals

print(ad.test(SESResult$residuals))

png("SES_QQ.png")
qqnorm(SESResult$residuals, datax = FALSE)
qqline(SESResult$residuals)
dev.off()