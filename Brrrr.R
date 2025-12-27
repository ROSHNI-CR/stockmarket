# Load required libraries
library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(plotly)
library(forecast)
library(lubridate)
library(DT)
library(scales)

# Function to load and process data from local file
load_stock_data <- function() {
  # Read the CSV file from the local directory
  stocks <- read.csv("stockNSE.csv", stringsAsFactors = FALSE)
  
  # Convert date strings to Date objects
  stocks$Date <- dmy(stocks$Date)
  
  # Convert numeric columns from character to numeric
  numeric_cols <- c("PREV_CLOSE", "Open", "High", "Low", "LAST_PRICE", "Close", 
                    "AVG_PRICE", "Volume", "TURNOVER_LACS", "Traded", "DELIV_QTY", "DELIV_PER")
  
  stocks[numeric_cols] <- lapply(stocks[numeric_cols], function(x) as.numeric(as.character(x)))
  
  return(stocks)
}

# Function to generate dummy data when needed
generate_dummy_data <- function(current_row, days_back = 30) {
  set.seed(123)  # For reproducibility
  
  # Create sequence of dates (assuming current_row$Date is a Date object)
  dates <- seq(current_row$Date - days_back, current_row$Date, by = "day")
  
  # Generate small variations around current values
  variations <- data.frame(
    Open = jitter(rep(current_row$Open, days_back + 1), factor = 0.1),
    High = jitter(rep(current_row$High, days_back + 1), factor = 0.1),
    Low = jitter(rep(current_row$Low, days_back + 1), factor = 0.1),
    Close = jitter(rep(current_row$Close, days_back + 1), factor = 0.1),
    Volume = pmax(1, round(current_row$Volume * runif(days_back + 1, 0.8, 1.2)))
  )
  
  # Ensure High >= Low and High >= Close >= Low
  variations <- variations %>%
    mutate(
      High = pmax(Open, Close, High),
      Low = pmin(Open, Close, Low),
      Close = pmin(High, pmax(Low, Close))
    )
  
  # Create dummy dataframe
  data.frame(
    Date = dates,
    Ticker = current_row$Ticker,
    SERIES = current_row$SERIES,
    variations,
    stringsAsFactors = FALSE
  )
}

# Function to predict stock price using ARIMA
predict_stock_price <- function(data, days = 30) {
  # If insufficient data, generate dummy rows
  if (nrow(data) < 30 && nrow(data) > 0) {
    dummy <- generate_dummy_data(tail(data, 1), 30 - nrow(data))
    data <- bind_rows(data, dummy) %>% arrange(Date)
  }
  
  if (nrow(data) >= 30) {
    # Check for NA values in Close
    if(any(is.na(data$Close))) {
      data <- data[!is.na(data$Close),]
    }
    
    ts_data <- ts(data$Close, frequency = 7)  # Assuming daily data with weekly seasonality
    
    # Model fitting
    model <- tryCatch({
      auto.arima(ts_data)
    }, error = function(e) {
      return(NULL)
    })
    
    if(is.null(model)) return(NULL)
    
    forecast_result <- forecast(model, h = days)
    
    # Create forecast data frame
    last_date <- max(data$Date)
    forecast_dates <- seq(last_date + lubridate::days(1), by = "day", length.out = days)
    
    forecast_df <- data.frame(
      Date = forecast_dates,
      Predicted = as.numeric(forecast_result$mean),
      Lower = as.numeric(forecast_result$lower[, 2]),  # 95% confidence interval
      Upper = as.numeric(forecast_result$upper[, 2])
    )
    
    list(
      model = model,
      forecast = forecast_result,
      forecast_df = forecast_df,
      last_price = tail(data$Close, 1)
    )
  } else {
    NULL
  }
}

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Stock Price Predictor"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Predictions", tabName = "predictions", icon = icon("chart-line")),
      menuItem("Investment Returns", tabName = "returns", icon = icon("money-bill")),
      menuItem("Top Stocks", tabName = "topstocks", icon = icon("trophy")),
      menuItem("About", tabName = "about", icon = icon("info-circle"))
    ),
    
    # Input controls - only show relevant controls for each tab
    conditionalPanel(
      condition = "input.sidebarMenu != 'topstocks'",
      selectInput("ticker", "Select Stock:", choices = NULL),
      dateRangeInput("dateRange", "Date Range:",
                     start = "2020-01-01",
                     end = Sys.Date()),
      numericInput("days", "Prediction Period (Days):", 30, min = 1, max = 100),
      numericInput("investment", "Investment Amount (₹):", 10000, min = 100),
      actionButton("predict", "Predict", icon = icon("calculator"), 
                   style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")
    )
  ),
  
  dashboardBody(
    tabItems(
      # Dashboard tab
      tabItem(tabName = "dashboard",
              fluidRow(
                infoBoxOutput("currentPriceBox"),
                infoBoxOutput("changeBox"),
                infoBoxOutput("volumeBox")
              ),
              fluidRow(
                box(
                  title = "Stock Price Trend", status = "primary", solidHeader = TRUE,
                  collapsible = TRUE, width = 12,
                  plotlyOutput("pricePlot", height = "400px")
                )
              ),
              fluidRow(
                box(
                  title = "Trading Volume", status = "info", solidHeader = TRUE,
                  collapsible = TRUE, width = 6,
                  plotlyOutput("volumePlot", height = "300px")
                ),
                box(
                  title = "Stock Statistics", status = "warning", solidHeader = TRUE,
                  collapsible = TRUE, width = 6,
                  DTOutput("statsTable")
                )
              )
      ),
      
      # Predictions tab
      tabItem(tabName = "predictions",
              fluidRow(
                box(
                  title = "Price Prediction", status = "primary", solidHeader = TRUE,
                  width = 12,
                  plotlyOutput("predictionPlot", height = "400px")
                )
              ),
              fluidRow(
                valueBoxOutput("predPriceBox"),
                valueBoxOutput("predChangeBox"),
                valueBoxOutput("confidenceBox")
              ),
              fluidRow(
                box(
                  title = "Prediction Details", status = "info", solidHeader = TRUE,
                  width = 12,
                  verbatimTextOutput("predictionDetails")
                )
              )
      ),
      
      # Investment Returns tab
      tabItem(tabName = "returns",
              fluidRow(
                box(
                  title = "Potential Investment Returns", status = "success", solidHeader = TRUE,
                  width = 12,
                  plotlyOutput("returnsPlot", height = "400px")
                )
              ),
              fluidRow(
                valueBoxOutput("investmentBox"),
                valueBoxOutput("projectedValueBox"),
                valueBoxOutput("returnPercentBox")
              ),
              fluidRow(
                box(
                  title = "Investment Scenarios", status = "warning", solidHeader = TRUE,
                  width = 12,
                  DTOutput("scenariosTable")
                )
              )
      ),
      
      # Top Stocks tab (NEW)
      tabItem(tabName = "topstocks",
              fluidRow(
                box(
                  title = "Stock Price Filter", status = "primary", solidHeader = TRUE,
                  width = 12,
                  sliderInput("priceFilter", "Stock Price Range (₹):",
                              min = 0, max = 10000, value = c(0, 5000),
                              step = 100),
                  numericInput("topStocksDays", "Prediction Period (Days):", 30, min = 1, max = 100),
                  actionButton("calculateTopStocks", "Calculate Top Stocks", icon = icon("calculator"), 
                               style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")
                )
              ),
              fluidRow(
                valueBoxOutput("analyzedStocksBox"),
                valueBoxOutput("avgReturnBox"),
                valueBoxOutput("topReturnBox")
              ),
              fluidRow(
                box(
                  title = "Top 10 Stocks by Investment Return", status = "success", solidHeader = TRUE,
                  width = 12,
                  DTOutput("topStocksTable")
                )
              ),
              fluidRow(
                box(
                  title = "Return Comparison", status = "info", solidHeader = TRUE,
                  width = 12,
                  plotOutput("topStocksPlot", height = "400px") # Changed from plotlyOutput to plotOutput
                )
              )
      ),
      
      # About tab
      tabItem(tabName = "about",
              box(
                title = "About This Dashboard", status = "primary", solidHeader = TRUE,
                width = 12,
                p("This dashboard provides stock price prediction and investment analysis tools using NSE data."),
                p("Features:"),
                tags$ul(
                  tags$li("Historical stock price visualization"),
                  tags$li("Time series forecasting for price prediction"),
                  tags$li("Investment return calculation"),
                  tags$li("Top stocks recommendation based on predicted returns"),
                  tags$li("Multiple visualization options")
                ),
                p("The prediction model uses ARIMA (AutoRegressive Integrated Moving Average) for time series forecasting."),
                p("Note: Stock predictions are based on historical patterns and should not be the sole basis for investment decisions.")
              )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  # Load stock data
  stocks_data <- reactive({
    # Wrap in try-catch to handle file reading errors
    tryCatch({
      load_stock_data()
    }, error = function(e) {
      showNotification(paste("Error loading data:", e$message), type = "error")
      return(data.frame())
    })
  })
  
  # Update ticker choices
  observe({
    data <- stocks_data()
    if(nrow(data) > 0) {
      updateSelectInput(session, "ticker", choices = sort(unique(data$Ticker)))
    }
  })
  
  # Filter data based on selected ticker and date range
  filtered_data <- reactive({
    req(input$ticker)
    data <- stocks_data()
    if(nrow(data) == 0) return(data.frame())
    
    data %>%
      filter(Ticker == input$ticker,
             Date >= input$dateRange[1],
             Date <= input$dateRange[2]) %>%
      arrange(Date)
  })
  
  # Current stock info
  current_stock <- reactive({
    data <- filtered_data()
    if (nrow(data) > 0) {
      tail(data, 1)
    } else {
      NULL
    }
  })
  
  # Info boxes
  output$currentPriceBox <- renderInfoBox({
    stock <- current_stock()
    if (!is.null(stock)) {
      infoBox(
        "Current Price", paste0("₹", format(stock$Close, big.mark = ",", digits = 2)),
        icon = icon("rupee-sign"), color = "blue"
      )
    } else {
      infoBox("Current Price", "N/A", icon = icon("rupee-sign"), color = "blue")
    }
  })
  
  output$changeBox <- renderInfoBox({
    data <- filtered_data()
    if (nrow(data) >= 2) {
      current <- tail(data, 1)$Close
      previous <- tail(data, 2)[1, "Close"]
      change <- (current - previous) / previous * 100
      color <- ifelse(change >= 0, "green", "red")
      icon_type <- ifelse(change >= 0, "arrow-up", "arrow-down")
      
      infoBox(
        "Daily Change", paste0(format(change, digits = 2), "%"),
        icon = icon(icon_type), color = color
      )
    } else {
      infoBox("Daily Change", "N/A", icon = icon("percent"), color = "yellow")
    }
  })
  
  output$volumeBox <- renderInfoBox({
    stock <- current_stock()
    if (!is.null(stock)) {
      infoBox(
        "Volume", format(stock$Volume, big.mark = ","),
        icon = icon("chart-bar"), color = "purple"
      )
    } else {
      infoBox("Volume", "N/A", icon = icon("chart-bar"), color = "purple")
    }
  })
  
  # Price plot
  output$pricePlot <- renderPlotly({
    data <- filtered_data()
    if (nrow(data) == 1) {
      p <- ggplot(data, aes(x = Date)) +
        geom_point(aes(y = Close, color = "Close"), size = 3) +
        geom_point(aes(y = Open, color = "Open"), size = 3, alpha = 0.7) +
        labs(title = paste("Price for", input$ticker),
             y = "Price (₹)", x = "Date") +
        theme_minimal() +
        scale_color_manual(values = c("Close" = "#2c3e50", "Open" = "#3498db")) +
        scale_y_continuous(labels = scales::comma) +
        theme(legend.title = element_blank())
      ggplotly(p) %>% layout(hovermode = "x unified")
    } else if (nrow(data) > 1) {
      p <- ggplot(data, aes(x = Date)) +
        geom_line(aes(y = Close, color = "Close"), size = 1) +
        geom_line(aes(y = Open, color = "Open"), size = 1, alpha = 0.7) +
        labs(title = paste("Price Trend for", input$ticker),
             y = "Price (₹)", x = "Date") +
        theme_minimal() +
        scale_color_manual(values = c("Close" = "#2c3e50", "Open" = "#3498db")) +
        scale_y_continuous(labels = scales::comma) +
        theme(legend.title = element_blank())
      ggplotly(p) %>% layout(hovermode = "x unified")
    }
  })
  
  # Volume plot
  output$volumePlot <- renderPlotly({
    data <- filtered_data()
    if (nrow(data) > 0) {
      p <- ggplot(data, aes(x = Date, y = Volume)) +
        geom_bar(stat = "identity", fill = "#3498db", alpha = 0.7) +
        labs(title = paste("Trading Volume for", input$ticker),
             y = "Volume", x = "Date") +
        theme_minimal() +
        scale_y_continuous(labels = scales::comma)
      
      ggplotly(p)
    }
  })
  
  # Stats table
  output$statsTable <- renderDT({
    data <- filtered_data()
    if (nrow(data) > 0) {
      stats <- data.frame(
        Metric = c("Average Close", "Highest Price", "Lowest Price", 
                   "Average Volume", "Price Volatility"),
        Value = c(
          paste0("₹", format(mean(data$Close, na.rm = TRUE), big.mark = ",", digits = 2)),
          paste0("₹", format(max(data$High, na.rm = TRUE), big.mark = ",", digits = 2)),
          paste0("₹", format(min(data$Low, na.rm = TRUE), big.mark = ",", digits = 2)),
          format(mean(data$Volume, na.rm = TRUE), big.mark = ",", digits = 0),
          paste0(format(sd(data$Close, na.rm = TRUE) / mean(data$Close, na.rm = TRUE) * 100, digits = 2), "%")
        ),
        stringsAsFactors = FALSE
      )
      
      datatable(stats, options = list(dom = 't', ordering = FALSE, paging = FALSE))
    }
  })
  
  # Prediction model
  prediction_model <- eventReactive(input$predict, {
    predict_stock_price(filtered_data(), input$days)
  })
  
  # Prediction plot
  output$predictionPlot <- renderPlotly({
    data <- filtered_data()
    pred <- prediction_model()
    
    if (!is.null(pred) && nrow(data) > 0) {
      # Combine historical and predicted data
      historical <- data %>% 
        select(Date, Close) %>%
        rename(Price = Close)
      
      predicted <- pred$forecast_df %>%
        select(Date, Predicted, Lower, Upper)
      
      # Plot
      p <- ggplot() +
        # Historical data
        geom_line(data = historical, aes(x = Date, y = Price, color = "Historical"), size = 1) +
        # Prediction
        geom_line(data = predicted, aes(x = Date, y = Predicted, color = "Predicted"), size = 1) +
        # Confidence interval
        geom_ribbon(data = predicted, aes(x = Date, ymin = Lower, ymax = Upper), 
                    fill = "blue", alpha = 0.2) +
        labs(title = paste("Price Prediction for", input$ticker),
             y = "Price (₹)", x = "Date") +
        theme_minimal() +
        scale_color_manual(values = c("Historical" = "#2c3e50", "Predicted" = "#e74c3c")) +
        scale_y_continuous(labels = scales::comma) +
        theme(legend.title = element_blank())
      
      ggplotly(p) %>% layout(hovermode = "x unified")
    }
  })
  
  # Prediction value boxes
  output$predPriceBox <- renderValueBox({
    pred <- prediction_model()
    if (!is.null(pred)) {
      final_pred <- tail(pred$forecast_df$Predicted, 1)
      valueBox(
        paste0("₹", format(final_pred, big.mark = ",", digits = 2)),
        paste("Predicted Price after", input$days, "days"),
        icon = icon("chart-line"),
        color = "blue"
      )
    } else {
      valueBox("N/A", "Predicted Price", icon = icon("chart-line"), color = "blue")
    }
  })
  
  output$predChangeBox <- renderValueBox({
    pred <- prediction_model()
    if (!is.null(pred)) {
      final_pred <- tail(pred$forecast_df$Predicted, 1)
      last_price <- pred$last_price
      change <- (final_pred - last_price) / last_price * 100
      
      color <- ifelse(change >= 0, "green", "red")
      icon_type <- ifelse(change >= 0, "arrow-up", "arrow-down")
      
      valueBox(
        paste0(format(change, digits = 2), "%"),
        "Predicted Change",
        icon = icon(icon_type),
        color = color
      )
    } else {
      valueBox("N/A", "Predicted Change", icon = icon("percent"), color = "yellow")
    }
  })
  
  output$confidenceBox <- renderValueBox({
    pred <- prediction_model()
    if (!is.null(pred)) {
      final_lower <- tail(pred$forecast_df$Lower, 1)
      final_upper <- tail(pred$forecast_df$Upper, 1)
      final_pred <- tail(pred$forecast_df$Predicted, 1)
      
      range_percent <- (final_upper - final_lower) / final_pred * 100
      
      valueBox(
        paste0("±", format(range_percent/2, digits = 2), "%"),
        "Prediction Confidence (95%)",
        icon = icon("percentage"),
        color = "purple"
      )
    } else {
      valueBox("N/A", "Prediction Confidence", icon = icon("percentage"), color = "purple")
    }
  })
  
  # Prediction details
  output$predictionDetails <- renderPrint({
    pred <- prediction_model()
    if (!is.null(pred)) {
      summary(pred$model)
    } else {
      cat("Insufficient data for prediction. Please select a stock with more historical data.")
    }
  })
  
  # Investment calculations
  investment_scenarios <- reactive({
    pred <- prediction_model()
    if (!is.null(pred) && input$investment > 0) {
      last_price <- pred$last_price
      predicted_prices <- pred$forecast_df$Predicted
      
      # Calculate number of shares that can be purchased
      shares <- floor(input$investment / last_price)
      
      # Calculate investment value over time
      investment_value <- shares * predicted_prices
      
      # Create scenarios data frame
      scenarios <- data.frame(
        Date = pred$forecast_df$Date,
        Shares = shares,
        PredictedPrice = predicted_prices,
        InvestmentValue = investment_value,
        Return = (investment_value - input$investment) / input$investment * 100,
        stringsAsFactors = FALSE
      )
      
      scenarios
    } else {
      NULL
    }
  })
  
  # Returns plot
  output$returnsPlot <- renderPlotly({
    scenarios <- investment_scenarios()
    
    if (!is.null(scenarios)) {
      p <- ggplot(scenarios, aes(x = Date)) +
        geom_line(aes(y = InvestmentValue, color = "Investment Value"), size = 1) +
        geom_hline(aes(yintercept = input$investment, color = "Initial Investment"), 
                   linetype = "dashed", size = 1) +
        labs(title = "Projected Investment Value Over Time",
             y = "Value (₹)", x = "Date") +
        theme_minimal() +
        scale_color_manual(values = c("Investment Value" = "#27ae60", "Initial Investment" = "#e74c3c")) +
        scale_y_continuous(labels = scales::comma) +
        theme(legend.title = element_blank())
      
      ggplotly(p) %>% layout(hovermode = "x unified")
    }
  })
  
  # Investment value boxes
  output$investmentBox <- renderValueBox({
    valueBox(
      paste0("₹", format(input$investment, big.mark = ",", digits = 0)),
      "Initial Investment",
      icon = icon("money-bill"),
      color = "green"
    )
  })
  
  output$projectedValueBox <- renderValueBox({
    scenarios <- investment_scenarios()
    if (!is.null(scenarios)) {
      final_value <- tail(scenarios$InvestmentValue, 1)
      
      valueBox(
        paste0("₹", format(final_value, big.mark = ",", digits = 2)),
        paste("Projected Value after", input$days, "days"),
        icon = icon("chart-line"),
        color = "blue"
      )
    } else {
      valueBox("N/A", "Projected Value", icon = icon("chart-line"), color = "blue")
    }
  })
  
  output$returnPercentBox <- renderValueBox({
    scenarios <- investment_scenarios()
    if (!is.null(scenarios)) {
      final_return <- tail(scenarios$Return, 1)
      
      color <- ifelse(final_return >= 0, "green", "red")
      icon_type <- ifelse(final_return >= 0, "arrow-up", "arrow-down")
      
      valueBox(
        paste0(format(final_return, digits = 2), "%"),
        "Projected Return",
        icon = icon(icon_type),
        color = color
      )
    } else {
      valueBox("N/A", "Projected Return", icon = icon("percent"), color = "yellow")
    }
  })
  
  # Scenarios table
  output$scenariosTable <- renderDT({
    scenarios <- investment_scenarios()
    if (!is.null(scenarios)) {
      # Create a summary table with different time horizons
      days <- c(1, 7, 14, 30, min(input$days, nrow(scenarios)))
      days <- days[!duplicated(days)]  # Remove duplicates
      
      summary_table <- data.frame(
        Period = paste(days, "Days"),
        PredictedPrice = sapply(days, function(d) format(scenarios$PredictedPrice[min(d, nrow(scenarios))], digits = 2)),
        ProjectedValue = sapply(days, function(d) paste0("₹", format(scenarios$InvestmentValue[min(d, nrow(scenarios))], big.mark = ",", digits = 2))),
        Return = sapply(days, function(d) paste0(format(scenarios$Return[min(d, nrow(scenarios))], digits = 2), "%")),
        stringsAsFactors = FALSE
      )
      
      datatable(summary_table, options = list(dom = 't', ordering = FALSE, paging = FALSE))
    }
  })
  
  # TOP STOCKS TAB FUNCTIONALITY (FIXED)
  
  # Calculate top stocks based on predicted returns
  top_stocks <- eventReactive(input$calculateTopStocks, {
    # Show progress notification
    withProgress(message = "Analyzing stocks...", value = 0, {
      
      # Get all unique tickers
      all_tickers <- unique(stocks_data()$Ticker)
      
      # Apply price filter
      filtered_tickers <- stocks_data() %>%
        filter(Close >= input$priceFilter[1], Close <= input$priceFilter[2]) %>%
        pull(Ticker) %>%
        unique()
      
      # Calculate predicted returns for each ticker
      results <- data.frame(
        Ticker = character(),
        CurrentPrice = numeric(),
        PredictedPrice = numeric(),
        PredictedReturn = numeric(),
        Volatility = numeric(),
        Volume = numeric(),
        stringsAsFactors = FALSE
      )
      
      total_tickers <- length(filtered_tickers)
      
      for(i in seq_along(filtered_tickers)) {
        ticker <- filtered_tickers[i]
        
        # Update progress
        incProgress(1/total_tickers, detail = paste("Analyzing", ticker, "-", i, "of", total_tickers))
        
        # Get latest data for this ticker
        ticker_data <- stocks_data() %>%
          filter(Ticker == ticker) %>%
          arrange(Date) %>%
          tail(60)  # Use last 60 days for prediction
        
        if(nrow(ticker_data) > 30) {
          # Use the prediction function
          pred <- predict_stock_price(ticker_data, input$topStocksDays)
          
          if(!is.null(pred)) {
            # Get current price and predicted price
            current_price <- as.numeric(pred$last_price)
            predicted_price <- as.numeric(tail(pred$forecast_df$Predicted, 1))
            predicted_return <- (predicted_price - current_price) / current_price * 100
            
            # Calculate volatility (risk measure)
            volatility <- sd(ticker_data$Close, na.rm = TRUE) / mean(ticker_data$Close, na.rm = TRUE) * 100
            
            # Add to results
            results <- rbind(results, data.frame(
              Ticker = ticker,
              CurrentPrice = current_price,
              PredictedPrice = predicted_price,
              PredictedReturn = predicted_return,
              Volatility = volatility,
              Volume = mean(ticker_data$Volume, na.rm = TRUE),
              stringsAsFactors = FALSE
            ))
          }
        }
      }
      
      # Sort by predicted return (descending) and take top 10
      results %>%
        arrange(desc(PredictedReturn)) %>%
        head(10)
    })
  })
  
  # Top stocks value boxes
  output$analyzedStocksBox <- renderValueBox({
    top <- top_stocks()
    if(!is.null(top) && nrow(top) > 0) {
      valueBox(
        nrow(top),
        "Top Stocks Analyzed",
        icon = icon("list"),
        color = "blue"
      )
    } else {
      valueBox("N/A", "Top Stocks Analyzed", icon = icon("list"), color = "blue")
    }
  })
  
  output$avgReturnBox <- renderValueBox({
    top <- top_stocks()
    if(!is.null(top) && nrow(top) > 0) {
      avg_return <- mean(top$PredictedReturn, na.rm = TRUE)
      
      color <- ifelse(avg_return >= 0, "green", "red")
      icon_type <- ifelse(avg_return >= 0, "arrow-up", "arrow-down")
      
      valueBox(
        paste0(format(avg_return, digits = 2), "%"),
        "Average Predicted Return",
        icon = icon(icon_type),
        color = color
      )
    } else {
      valueBox("N/A", "Average Predicted Return", icon = icon("percent"), color = "yellow")
    }
  })
  
  output$topReturnBox <- renderValueBox({
    top <- top_stocks()
    if(!is.null(top) && nrow(top) > 0) {
      top_return <- max(top$PredictedReturn, na.rm = TRUE)
      top_ticker <- top$Ticker[which.max(top$PredictedReturn)]
      
      valueBox(
        paste0(format(top_return, digits = 2), "%"),
        paste("Top Return:", top_ticker),
        icon = icon("trophy"),
        color = "green"
      )
    } else {
      valueBox("N/A", "Top Return", icon = icon("trophy"), color = "green")
    }
  })
  
  # Render the top stocks table
  output$topStocksTable <- renderDT({
    top <- top_stocks()
    
    if(!is.null(top) && nrow(top) > 0) {
      top_formatted <- data.frame(
        Ticker = top$Ticker,
        CurrentPrice = paste0("₹", format(top$CurrentPrice, big.mark = ",", digits = 2)),
        PredictedPrice = paste0("₹", format(top$PredictedPrice, big.mark = ",", digits = 2)),
        PredictedReturn = paste0(format(top$PredictedReturn, digits = 2), "%"),
        Volatility = paste0(format(top$Volatility, digits = 2), "%"),
        Volume = format(top$Volume, big.mark = ",", digits = 0),
        stringsAsFactors = FALSE
      )
      
      datatable(top_formatted, options = list(pageLength = 10, dom = 'tip', paging = FALSE))
    }
  })
  
  # Render the top stocks comparison plot - FIXED to use regular plot instead of plotly
  output$topStocksPlot <- renderPlot({
    top <- top_stocks()
    
    if(!is.null(top) && nrow(top) > 0) {
      # Create a basic ggplot without plotly conversion
      ggplot(top, aes(x = reorder(Ticker, PredictedReturn), y = PredictedReturn, fill = PredictedReturn)) +
        geom_bar(stat = "identity") +
        scale_fill_gradient2(low = "red", mid = "yellow", high = "green", midpoint = 0) +
        labs(title = "Predicted Returns for Top Stocks",
             x = "Stock", y = "Predicted Return (%)") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server) 