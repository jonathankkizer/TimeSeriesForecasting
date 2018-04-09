library(ggplot2)
data_table <- read.csv("US_CCI_.csv")

data_table$IncTime <- 1:nrow(data_table)
data_table <- subset(data_table, select = c(IncTime, TIME, Value))
names(data_table) <- c("Time", "YearMonth", "Value")

print(head(data_table))
print(tail(data_table))

CCIVTime <- ggplot() +
  geom_point(aes(x = data_table$Time, y = data_table$Value), color = "black") + 
  geom_line(aes(x = data_table$Time, y = data_table$Value), color = "blue") +
  xlab("Time") + ylab("Consumer Confidence Index") + ggtitle("CCI vs. Time, 1960-Present") +
  scale_y_continuous()
ggsave("cciVTime.png", last_plot())

write.csv(data_table, "postETLCCI.csv")