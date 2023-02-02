library(tidyquant)
library(ggplot2)

#sp_500 <- tq_index("SP500") %>% tq_get(get="stock.prices")

plot_log_returns <- function(ticker, start_date, end_date) {
  
  # Subset data for the specified ticker and date range
  stock_prices <- sp_500 %>%
    filter(symbol == ticker) %>%
    filter(date >= start_date & date <= end_date)
  
  # Calculate daily log returns
  stock_returns <- diff(log(stock_prices$close))
  
  # Plot ggplot of log returns
  ggplot(data.frame(x = qnorm(runif(length(stock_returns))), y = sample(stock_returns)),
         aes(x, y)) +
    geom_point() +
    ggtitle(paste0(ticker, " Daily Log Returns (", start_date, " to ", end_date, ")")) +
    xlab("Theoretical Quantiles") +
    ylab("Sample Quantiles")
}
#Just ONE
plot_log_returns("CRL", "2013-01-01", "2023-01-01")
companies <- unique(sp_500$symbol)

#All of them
for (ticker in companies) {
  plot_log_returns(ticker, "2013-01-01", "2023-01-01")
}
