library(ggplot2)
library(fpp)

file_data <- "STA372_Homework9_Question2.dat.txt"
data_table <- read.table(file_data, header = FALSE, sep = "")
colnames(data_table) <- c("Month", "Shipments", "Seasonal_Index", "Consumer_Pack", "Consumer_Pack_Lag1", "Consumer_Pack_Lag2", "Dealer_Allowance", "Dealer_Allowance_Lag1", "Dealer_Allowance_Lag2")

shipmentsVMonth <- ggplot() +
  geom_point(aes(x = data_table$Month, y = data_table$Shipments), color = "black") +
  geom_line(aes(x = data_table$Month, y = data_table$Shipments), color = "blue") +
  scale_y_continuous() + xlab("Month") + ylab("Shipments") + ggtitle("Shipments vs. Month")
ggsave("q2_shipmentsVSMonth.png", last_plot())

seasonallyAdjusted <- data_table$Shipments / data_table$Seasonal_Index
data_table$ShipmentsSA <- seasonallyAdjusted
print(head(data_table))

shipmentsVMonth <- ggplot() +
  geom_point(aes(x = data_table$Month, y = data_table$ShipmentsSA), color = "black") +
  geom_line(aes(x = data_table$Month, y = data_table$ShipmentsSA), color = "blue") +
  scale_y_continuous() + xlab("Month") + ylab("Shipments SA") + ggtitle("Seasonally-Adjusted Shipments vs. Month")
ggsave("q2_shipmentsSAVSMonth.png", last_plot())

y_time_series <- ts(data_table[,10])
X <- cbind(data_table$Month, data_table$Consumer_Pack, data_table$Consumer_Pack_Lag1, data_table$Consumer_Pack_Lag2, data_table$Dealer_Allowance, data_table$Dealer_Allowance_Lag1, data_table$Dealer_Allowance_Lag2)
colnames(X) <- c("Month", "ConsumerPack", "CP_lag1", "CP_lag2", "DealerAllowance", "DA_lag1", "DA_lag2") 
result <- Arima(y_time_series, xreg=X, order=c(2, 0, 0))
print(result)

ARIMA2ACF <- acf(result$residuals)
print(ARIMA2ACF)
png("q2_arima2ACF.png")
plot(ARIMA2ACF)
dev.off()

X2 <- cbind(data_table$Month, data_table$Dealer_Allowance, data_table$Dealer_Allowance_Lag1) 
colnames(X2) <- c("Month", "DealerAllowance", "DA_lag1")
result2 <- Arima(y_time_series, xreg=X2, order=c(1, 0, 0))
print (result2)

ARIMA22ACF <- acf(result2$residuals)
print(ARIMA22ACF)
png("q2_arima22ACF.png")
plot(ARIMA22ACF)
dev.off()

Dealer_Allowance_Forecast <- cbind(c(49, 50), c(200000, 100000), c(data_table$Dealer_Allowance_Lag1[48], 200000))
colnames(Dealer_Allowance_Forecast) <- c("Month", "DealerAllowance", "DA_Lag1")
print(Dealer_Allowance_Forecast)
result_forecast <- forecast(result2, xreg=Dealer_Allowance_Forecast, h=2)
print(result_forecast)

closeAllConnections()