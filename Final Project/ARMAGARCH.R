library(ggplot2)
library(fpp)
library(fGarch)
library(rugarch)
library(nortest)

cat("\n--------ARMA-GARCH Model Analysis--------\n")

model=ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(3, 1), include.mean = TRUE),
  distribution.model = "norm"
)

uGarchResult <- ugarchfit(model, data_table$CCI, solver = 'hybrid')
print(uGarchResult)

data_table$ARMAGARCHfitted <- uGarchResult@fit$fitted.values
data_table$ARMAGARCHresiduals <- uGarchResult@fit$residuals


data_table$uGarch_Eta <- uGarchResult@fit$residuals / uGarchResult@fit$sigma


png("ARMA-GARCH_eta_ACF.png")
acf(data_table$uGarch_Eta)
dev.off()

png("ARMA-GARCH_eta^2_ACF.png")
acf(data_table$uGarch_Eta^2)
dev.off()

ARMAGARCHvsData <- ggplot() +
  geom_point(aes(x = data_table$Time, y = data_table$CCI), color = "red") +
  geom_line(aes(x = data_table$Time, y = data_table$CCI), color = "red") +
  geom_point(aes(x = data_table$Time, y = uGarchResult@fit$fitted.values), color = "purple") +
  geom_line(aes(x = data_table$Time, y = uGarchResult@fit$fitted.values), color = "purple") +
  ylab("CCI (Red) & ARMA(3,1)-GARCH(1,1) Model (Brown") + xlab("Time") + ggtitle("ARMA-GARCH & Data vs Time") +
  scale_y_continuous()
ggsave("ARMA-GARCHvsCCI.png", last_plot())

ARMAGARCHErrorsVTime <- ggplot() +
  geom_point(aes(x = data_table$Time, y = uGarchResult@fit$residuals), color = "purple") +
  #geom_line(aes(x = data_table$Time, y = uGarchResult@fit$residuals), color = "purple") +
  ylab("ARMA(3,1)-GARCH(1,1) Residuals") + xlab("Time") + ggtitle("ARMA-GARCH Residuals vs. Time") +
  scale_y_continuous()
ggsave("ARMA-GARCH_ResidualsVTime.png", last_plot())

ARMAGARCHresidualsHistogram <- ggplot() +
  geom_histogram(aes(data_table$ARMAGARCHresiduals), binwidth = .01) +
  ggtitle("Histogram of ARMA-GARCH Residuals, Binwidth = .01") + xlab("ARMAGARCH Residuals") + ylab("Count")
scale_y_continuous()
ggsave("ARMAGARCH_Residuals_Histogram.png", last_plot())

print(ugarchforecast(uGarchResult, data = NULL, n.ahead = 9))

actAIC <- -2.1457 * NROW(data_table$CCI)
cat("\nCorrected AIC (AIC * n):\n")
cat(actAIC)