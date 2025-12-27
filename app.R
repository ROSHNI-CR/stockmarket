library(shiny)
library(quantmod)
library(forecast)
library(ggplot2)
library(plotly)

source("get_data.R")
source("arima_model.R")

ui <- fluidPage(
  titlePanel("📈 Stock Market Prediction Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("symbol", "Enter Stock Symbol (e.g., AAPL):", "AAPL"),
      numericInput("horizon", "Forecast Days:", 30, min = 1, max = 180),
      actionButton("go", "Predict")
    ),
    mainPanel(
      plotlyOutput("stockPlot"),
      br(),
      plotlyOutput("forecastPlot")
    )
  )
)

server <- function(input, output) {
  stock_data <- eventReactive(input$go, {
    get_stock_data(input$symbol)
  })
  
  output$stockPlot <- renderPlotly({
    req(stock_data())
    df <- data.frame(date = index(stock_data()), price = as.numeric(Cl(stock_data())))
    ggplotly(
      ggplot(df, aes(x = date, y = price)) +
        geom_line(color = "steelblue") +
        labs(title = paste("Historical Prices for", input$symbol),
             x = "Date", y = "Closing Price")
    )
  })
  
  output$forecastPlot <- renderPlotly({
    req(stock_data())
    forecasted <- run_arima(Cl(stock_data()), horizon = input$horizon)
    
    forecast_df <- data.frame(
      date = time(forecasted$mean),
      predicted = as.numeric(forecasted$mean)
    )
    
    plot_ly() %>%
      add_lines(x = index(stock_data()), y = as.numeric(Cl(stock_data())), name = "Historical") %>%
      add_lines(x = forecast_df$date, y = forecast_df$predicted, name = "ARIMA Forecast", line = list(color = "red")) %>%
      layout(title = paste("Forecast for", input$symbol),
             xaxis = list(title = "Date"),
             yaxis = list(title = "Price"))
  })
}

shinyApp(ui = ui, server = server)
