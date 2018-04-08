library(ggplot2)

file_output <- "SeasonalDecomposition_Homework2.txt"
sink(file_output, append=FALSE, split=TRUE)

# Read Sales Data, create log(Sales) column

file <- "STA372_Homework2.dat.txt"
Sales_table <- read.table(file, header = FALSE, sep = "")
colnames(Sales_table) <- c("Time", "Quarter", "Sales") 
Sales_table$LogSales <- log(Sales_table$Sales)
cat("\n", "First six rows of the data set are:", "\n", "\n")
print(head(Sales_table))

# Sales vs. Time Plot

salesVTime <- ggplot() + 
  geom_point(aes(x=Sales_table$Time, y=Sales_table$Sales), color="Black") + 
  geom_line(aes(x=Sales_table$Time, y=Sales_table$Sales), color="Blue") + 
  scale_y_continuous() +
  ggtitle("Sales vs. Time") + 
  xlab("Time") + 
  ylab("Sales")
#print(salesVTime)
ggsave("SalesVTime.png", last_plot())

# log(Sales) vs Time Plot

logSalesVTime <- ggplot() +
  geom_point(aes(x=Sales_table$Time, y=Sales_table$LogSales), color="Black") +
  geom_line(aes(x=Sales_table$Time, y=Sales_table$LogSales), color="Blue") +
  scale_y_continuous() +
  ggtitle("log(Sales) vs. Time") +
  xlab("Time") +
  ylab("logSales")
#print(logSalesVTime)
ggsave("log(Sales)VTime.png", last_plot())

# save log(sales) as a time-series object

LogSales_time_series <- ts(Sales_table[,4], frequency = 4)

# Decompose logSalesTimeSeries

LogSales_time_series_components <- stl(LogSales_time_series, s.window=7) 
print(LogSales_time_series_components)
# saves to .png named LogSales_time_series_components.png in working directory
png("LogSales_time_series_components.png")
plot(LogSales_time_series_components)
dev.off()

# Add trend log(Sales) and Seaonal factors to Sales_date DF
seasonal <- LogSales_time_series_components$time.series[,1]
Sales_table$Seasonal <- seasonal

Sales_table$LogA <- (Sales_table$LogSales-Sales_table$Seasonal)
#print(LogSales_time_series_components$time.series[,2])

# plot seasonally-adjusted log sales

seasonallyAdjustedLogSalesVTime <- ggplot() +
  geom_point(aes(x=Sales_table$Time, y=Sales_table$LogA), color = "black") +
  geom_line(aes(x=Sales_table$Time, y=Sales_table$LogA), color = "blue") + 
  scale_y_continuous() +
  xlab("Time") +
  ylab("LogA") +
  ggtitle("Seasonally Adjusted log(Sales)")
ggsave("SeasonallyAdjustedlogSales.png", last_plot())

# Exp(log(Sales)) = sales; added to Sales_data DF

Sales_table$A <- exp(Sales_table$LogA)
cat("\nFirst six observations from the final Sales Table are:\n")
print(head(Sales_table))

# plot seasonally-adjusted sales

seasonallyAdjustedSalesVTime <- ggplot() +
  geom_point(aes(x=Sales_table$Time, y=Sales_table$A), color = "black") + 
  geom_line(aes(x=Sales_table$Time, y=Sales_table$A), color = "blue") + 
  scale_y_continuous() +
  xlab("Time") +
  ylab("A") +
  ggtitle("Seasonally Adjusted Sales")
ggsave("SeasonallyAdjustedSales.png", last_plot())