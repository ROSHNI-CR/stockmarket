library(forecast)

run_arima <- function(stock_data, horizon = 30) {
  fit <- auto.arima(stock_data)
  forecast(fit, h = horizon)
}
