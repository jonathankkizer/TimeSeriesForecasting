library(ggplot2)
data_table <- read.csv("US_CCI_.csv")

data_table$IncTime <- 1:nrow(data_table)
data_table <- subset(data_table, select = c(IncTime, TIME, Value))
names(data_table) <- c("Time", "YearMonth", "CCI")
cat("\n Data Table First and Last Ten Columns:\n")
print(head(data_table))
print(tail(data_table))

CCIVTime <- ggplot() +
  geom_point(aes(x = data_table$Time, y = data_table$CCI), color = "blue") + 
  geom_line(aes(x = data_table$Time, y = data_table$CCI), color = "red") +
  xlab("Time") + ylab("US Consumer Confidence Index") + ggtitle("CCI vs. Time, 1960-Present") +
  scale_y_continuous()
ggsave("cciVTime.png", last_plot())

write.csv(data_table, "postETLCCI.csv")