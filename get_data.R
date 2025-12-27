# get_data.R
library(quantmod)

get_stock_data <- function(symbol, from_date = "2020-01-01") {
  tryCatch({
    stock_data <- getSymbols(Symbols = symbol, src = "yahoo", 
                             from = from_date, auto.assign = FALSE)
    return(stock_data)
  }, error = function(e) {
    stop(paste("Error fetching stock data:", e$message))
  })
}

