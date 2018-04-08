library(ggplot2)

file_data <- "STA372_Homework7_Question1.txt"
data_table <- read.table(file_data, header = FALSE, sep = "")
colnames(data_table) <- c("Time", "Sales", "seas_adj_Sales", "seasonal_Indices")

data_table$LogSales <- log(data_table$seas_adj_Sales)

salesVTime <- ggplot() +
  geom_point(aes(x = data_table$Time, y = data_table$Sales), color = "blue") +
  geom_line(aes(x = data_table$Time, y = data_table$Sales), color = "blue") +
  scale_y_continuous() +
  ylab("Sales") + xlab("Time") + ggtitle("Sales vs. Time")
ggsave("SalesVTime.png", last_plot())

seasonallyAdjustedSalesVTime <- ggplot() +
  geom_point(aes(x = data_table$Time, y = data_table$seas_adj_Sales), color = "blue") +
  geom_line(aes(x = data_table$Time, y = data_table$seas_adj_Sales), color = "blue") +
  scale_y_continuous() +
  ylab("Seasonally-Adjusted Sales") + xlab("Time") + ggtitle("Seasonally-Adjusted Sales vs. Time")
ggsave("seasonallyAdjustedSalesVTime.png", last_plot())

logSalesVTime <- ggplot() +
  geom_point(aes(x = data_table$Time, y = data_table$LogSales), color = "blue") +
  geom_line(aes(x = data_table$Time, y = data_table$LogSales), color = "blue") +
  scale_y_continuous() +
  ylab("Log(Sales)") + xlab("Time") + ggtitle("Log(Sales) vs. Time")
ggsave("logSalesVTime.png", last_plot())