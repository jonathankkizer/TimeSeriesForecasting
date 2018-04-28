library(ggplot2)
library(fpp)
library(fGarch)
library(rugarch)
library(nortest)
cat("\n--------GARCH Model Analysis--------\n")
CCI_sq = ts(data_table$CCI^2)

mean_CCI <- mean(data_table$CCI)
stdev_CCI <- sd(data_table$CCI)

png("CCI_sq_ACF.png")
plot(acf(CCI_sq))
dev.off()

png("CCI_sq_PACF.png")
plot(pacf(CCI_sq))
dev.off()

garchResult <- garchFit(data_table$CCI~garch(1,0), data = data_table$CCI, include.constant = T, trace = F)
print(summary(garchResult))

data_table$GARCH1_eta = garchResult@residuals / garchResult@sigma.t 

print(acf(data_table$GARCH1_eta))

garchModelVCCI <- ggplot() +
  geom_point(aes(x = data_table$Time, y = data_table$CCI - mean_CCI), color = "red") +
  geom_line(aes(x = data_table$Time, y = data_table$CCI - mean_CCI), color = "red") +
  geom_point(aes(x = data_table$Time, y = garchResult@sigma.t), color = "purple") +
  geom_line(aes(x = data_table$Time, y = garchResult@sigma.t), color = "purple") +
  xlab("Time") + ylab("CCI (Red) & GARCH Model (Purple)") + ggtitle("CCI & GARCH Model vs. Time") +
  scale_y_continuous()
ggsave("GARCH_v_CCI.png", last_plot())

png("garch_QQ.png")
qqnorm(garchResult@residuals, datax = FALSE)
qqline(garchResult@residuals)
dev.off()


