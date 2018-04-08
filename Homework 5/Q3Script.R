library(ggplot2)
library(fpp)
library(nortest)

file <- "STA372_Homework5_Question3.dat.txt"
data_table <- read.table(file, header = FALSE, sep = "")
colnames(data_table) <- c("Week", "Y", "FixedForecast")

yVsWeek <- ggplot() +
  geom_point(aes(x = data_table$Week, y = data_table$Y)) +
  geom_line(aes(x = data_table$Week, y = data_table$Y), color = "blue") +
  scale_y_continuous() +
  xlab("Week") +
  ylab("Y") +
  ggtitle("Week vs. Y")
ggsave("yVsWeek.png", last_plot())

data_table$Error <- data_table$Y - data_table$FixedForecast
RMSE_FixedForecast <- sqrt(sum(data_table$Error^2)/nrow(data_table))
cat("RMSE for the Fixed Forecast:")
print(RMSE_FixedForecast)

y_time_series <- ts(data_table[,2])

randomWalkModel <- ses(y_time_series, initial = "simple", alpha = 1, h = 1)
print(randomWalkModel$model)
print(randomWalkModel)

yVsRandomWalk <- ggplot() +
  geom_point(aes(x = data_table$Week, y = data_table$Y), color = "black") +
  geom_line(aes(x = data_table$Week, y = data_table$Y), color = "black") +
  geom_point(aes(x = data_table$Week, y = randomWalkModel$fitted), color = "blue") +
  geom_line(aes(x = data_table$Week, y = randomWalkModel$fitted), color = "blue") +
  scale_y_continuous()
ggsave("yVsRandomWalk.png", last_plot())

simpleExponModel <- ses(y_time_series, h = 1)
print(simpleExponModel$model)
print(simpleExponModel)

yVsExponForecast <- ggplot() +
  geom_point(aes(x = data_table$Week, y = data_table$Y), color = "black") +
  geom_line(aes(x = data_table$Week, y = data_table$Y), color = "black") +
  geom_point(aes(x = data_table$Week, y = simpleExponModel$fitted), color = "blue") +
  geom_line(aes(x = data_table$Week, y = simpleExponModel$fitted), color = "blue") +
  xlab("Time (Weeks)") +
  ylab("Simple Exponential Model & Actual Values") +
  ggtitle("Y (Black) vs. Simple Exponential Forecast (Blue) over Time (Weeks)") +
  scale_y_continuous()
ggsave("yvsExponentialForecast.png", last_plot())

exponForecastResiduals <- ggplot() +
  geom_point(aes(x = data_table$Week, y = simpleExponModel$residuals)) +
  geom_line(aes(x = data_table$Week, y = simpleExponModel$residuals)) +
  ylab("Residuals") + 
  xlab("Time (Weeks)") +
  scale_y_continuous() +
  geom_hline(yintercept = 0, size = 2) +
  ggtitle("Residuals vs Time")
ggsave("ExponForecastResiduals.png", last_plot())

acf_residuals <- acf(simpleExponModel$residuals)
print(acf_residuals)
png("residualsACF.png")
plot(acf_residuals)
dev.off()

data_table$forecast <- simpleExponModel$fitted
data_table$residuals <- simpleExponModel$residuals
print(tail(data_table))
print(simpleExponModel)

print(.975*726.19)
print(.975*832.5441)
print(.975*938.8982)