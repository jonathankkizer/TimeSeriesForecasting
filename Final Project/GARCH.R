library(ggplot2)
library(fpp)
library(fGarch)

CCI_sq = ts(data_table$CCI^2)

png("CCI_sq_ACF.png")
plot(acf(CCI_sq))
dev.off()

png("CCI_sq_PACF.png")
plot(pacf(CCI_sq))
dev.off()

garchResult <- garchFit(data_table$CCI~garch(1,0), data = data_table$CCI, include.constant = T, trace = F)
print(summary(garchResult))
print(ad.test(garchResult@residuals))

garchModelVCCI <- ggplot() +
  geom_point(aes(x = data_table$Time, y = data_table$CCI), color = "red") +
  geom_line(aes(x = data_table$Time, y = data_table$CCI), color = "red") +
  geom_point(aes(x = data_table$Time, y = garchResult@fitted), color = "purple") +
  geom_line(aes(x = data_table$Time, y = garchResult@fitted), color = "purple") +
  xlab("Time") + ylab("CCI (Red) & GARCH Model (Purple)") + ggtitle("CCI & GARCH Model vs. Time") +
  scale_y_continuous()
ggsave("GARCH_v_CCI.png", last_plot())

fit <- nnetar(data_table$CCI)
print(fit)
neuralNet <- ggplot() +
  geom_point(aes(x = data_table$Time, y = data_table$CCI), color = "red") +
  geom_line(aes(x = data_table$Time, y = data_table$CCI), color = "red") +
  geom_point(aes(x = data_table$Time, y = fit$fitted), color = "purple") +
  geom_line(aes(x = data_table$Time, y = fit$fitted), color = "purple") +
  xlab("Time") + ylab("CCI (Red) & GARCH Model (Purple)") + ggtitle("CCI & GARCH Model vs. Time") +
  scale_y_continuous()
ggsave("neural_v_CCI.png", last_plot())

print(ad.test(fit$residuals))

png("fit_QQ.png")
qqnorm(fit$residuals, datax = FALSE)
qqline(fit$residuals)
dev.off()

png("garch_QQ.png")
qqnorm(garchResult@residuals, datax = FALSE)
qqline(garchResult@residuals)
dev.off()