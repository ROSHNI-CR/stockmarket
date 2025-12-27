library(shiny)
library(forecast)
library(dplyr)
library(ggplot2)
library(readr)
library(bslib)
library(glue)
library(plotly)

# Load full dataset once
all_data <- read_csv("cleandata.csv")
all_data$Date <- as.Date(all_data$Date)

# UI
ui <- fluidPage(
  theme = bs_theme(
    version = 5, 
    bootswatch = "flatly", 
    base_font = font_google("Roboto"), 
    heading_font = font_google("Roboto Slab")
  ),
  tags$head(
    tags$style(HTML("
      .panel { 
        box-shadow: 0 2px 8px rgba(0,0,0,0.04); 
        border-radius: 8px; 
        background: #fff; 
        margin-bottom: 16px;
        padding-bottom: 10px;
      }
      .main-header { 
        margin-bottom: 24px; 
      }
      .section-title { 
        font-size: 1.25rem; 
        font-weight: 600;
        margin-top: 24px; 
        margin-bottom: 12px; 
        color: #2c3e50;
      }
      .investment-summary { 
        background: #f8f9fa; 
        border-radius: 8px; 
        padding: 18px; 
        border: 1px solid #e5e5e5;
        font-size: 1.1rem;
        margin-top: 18px;
      }
      .btn-primary {
        background-color: #2c3e50 !important;
        border-color: #2c3e50 !important;
      }
      .form-label {
        font-weight: 500;
      }
    "))
  ),
  titlePanel(
    div(class = "main-header",
        h2("NSE Stock Trend & Investment Forecast", style = "font-weight: 700;"),
        p("A professional dashboard for stock analysis", style = "color: #666;")
    )
  ),
  sidebarLayout(
    sidebarPanel(
      class = "panel",
      selectInput("ticker", "Choose NSE Stock:", choices = unique(all_data$Ticker)),
      numericInput("days", "Predict how many days ahead?", value = 5, min = 1, max = 100),
      numericInput("amount", "Investment Amount (₹):", value = 10000, min = 1),
      actionButton("go", "Predict & Calculate Return", class = "btn btn-primary w-100"),
      div(class = "section-title", "Investment Forecast"),
      div(class = "investment-summary", uiOutput("investmentSummary"))
    ),
    mainPanel(
      div(class = "section-title", "Stock Price Trend"),
      plotlyOutput("trendPlot", height = "450px"),
      br(),
      div(class = "section-title", "Forecast Summary"),
      verbatimTextOutput("forecastOutput")
    )
  )
)

# Server
server <- function(input, output) {
  
  stock_data <- eventReactive(input$go, {
    all_data %>%
      filter(Ticker == input$ticker) %>%
      arrange(Date)
  })
  
  output$trendPlot <- renderPlotly({
    df <- stock_data()
    p <- ggplot(df, aes(x = Date, y = Close)) +
      geom_line(color = "#2c7fb8", size = 1) +
      labs(title = paste("Closing Price Trend -", input$ticker),
           x = "Date", y = "Closing Price (₹)") +
      theme_minimal(base_size = 14)
    ggplotly(p, tooltip = c("x", "y"))
  })
  
  output$forecastOutput <- renderPrint({
    df <- stock_data()
    ts_data <- ts(df$Close, frequency = 365)
    model <- auto.arima(ts_data)
    future <- forecast(model, h = input$days)
    cat("Predicted price after", input$days, "days: ₹",
        round(future$mean[input$days], 2))
  })
  
  output$investmentSummary <- renderUI({
    df <- stock_data()
    current_price <- tail(df$Close, 1)
    ts_data <- ts(df$Close, frequency = 365)
    model <- auto.arima(ts_data)
    future <- forecast(model, h = input$days)
    predicted_price <- future$mean[input$days]
    
    shares <- input$amount / current_price
    future_value <- shares * predicted_price
    return_percent <- ((future_value - input$amount) / input$amount) * 100
    
    HTML(glue("
      <b>Current Price:</b> ₹{round(current_price, 2)} <br>
      <b>Predicted Price:</b> ₹{round(predicted_price, 2)} <br>
      <b>Shares Bought:</b> {round(shares, 2)} <br>
      <b>Future Value:</b> ₹{round(future_value, 2)} <br>
      <b>Return:</b> {round(return_percent, 2)}%
    "))
  })
}

shinyApp(ui = ui, server = server) 