library(prophet)

run_prophet <- function(stock_data, horizon = 30) {
  df <- data.frame(ds = index(stock_data), y = as.numeric(stock_data))
  m <- prophet(df)
  future <- make_future_dataframe(m, periods = horizon)
  predict(m, future)
}
