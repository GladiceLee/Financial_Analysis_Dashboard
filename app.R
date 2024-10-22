# app.R
library(shiny)
library(ggplot2)
library(dplyr)
library(scales)
library(DT)
library(readxl)
library(shinydashboard)
library(ggpubr)

# UI Definition
ui <- dashboardPage(
  dashboardHeader(title = "Financial Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Adjust Rates", tabName = "rates", icon = icon("calculator")),
      selectInput("conversion", "Select Conversion:", choices = c("USD to NTD", "NTD to USD")),
      sliderInput("exchange_rate", "Exchange Rate:", min = 27, max = 33, value = 30, step = 0.1),
      sliderInput("tax_rate", "Tax Rate (%):", min = 0, max = 20, value = 5),
      sliderInput("cost_adjustment", "Adjust Cost (% of Revenue):", min = 60, max = 90, value = 75, step = 1),
      downloadButton("download_report", "Download Report")
    )
  ),
  dashboardBody(
    fluidRow(
      box(
        title = "Cost, Revenue, and Profit (Adjusted for Exchange Rate and Tax)",
        status = "primary", solidHeader = TRUE, width = 12,
        plotOutput("financial_plot")
      )
    ),
    fluidRow(
      box(
        title = "Detailed Financial Data Table",
        status = "primary", solidHeader = TRUE, width = 12,
        DTOutput("financial_table")
      )
    )
  )
)

# Server Logic
server <- function(input, output) {
  
  financial_data <- read_excel("financial_data_36_months.xlsx")
  financial_data$Month <- as.Date(financial_data$Month)
  
  # Function to apply exchange and tax rates, and calculate additional metrics
  adjusted_finances <- reactive({
    finances <- financial_data
    finances$`Cost (USD)` <- finances$`Revenue (USD)` * (input$cost_adjustment / 100)
    if (input$conversion == "USD to NTD") {
      finances$Revenue_NTD <- finances$`Revenue (USD)` * input$exchange_rate
      finances$Cost_NTD <- finances$`Cost (USD)` * input$exchange_rate
    } else {
      finances$Revenue_NTD <- finances$`Revenue (USD)` / input$exchange_rate
      finances$Cost_NTD <- finances$`Cost (USD)` / input$exchange_rate
    }
    finances$Revenue_After_Tax <- finances$Revenue_NTD * (1 - input$tax_rate / 100)
    finances$Profit_NTD <- finances$Revenue_After_Tax - finances$Cost_NTD
    finances$Gross_Margin <- (finances$Profit_NTD / finances$Revenue_NTD) * 100
    finances$Cost_to_Revenue_Ratio <- (finances$Cost_NTD / finances$Revenue_NTD) * 100
    finances <- finances %>%
      mutate(Cumulative_Profit = cumsum(Profit_NTD))
    return(finances)
  })
  
  output$financial_plot <- renderPlot({
    finances <- adjusted_finances()
    p1 <- ggplot(finances, aes(x = Month)) +
      geom_line(aes(y = Revenue_After_Tax, color = "Revenue (After Tax)")) +
      geom_line(aes(y = Cost_NTD, color = "Cost")) +
      geom_line(aes(y = Profit_NTD, color = "Profit")) +
      scale_y_continuous(labels = dollar_format(prefix = ifelse(input$conversion == "USD to NTD", "NT$", "$"))) +
      labs(
        title = "Cost, Revenue, and Profit Over Time (Adjusted for Exchange Rate and Tax)",
        x = "Month", y = "Amount"
      ) +
      scale_color_manual(values = c("Revenue (After Tax)" = "blue", "Cost" = "red", "Profit" = "green")) +
      theme_minimal() +
      theme(legend.title = element_blank())
    
    p2 <- ggplot(finances, aes(x = Month, y = Cumulative_Profit)) +
      geom_line(color = "purple") +
      scale_y_continuous(labels = dollar_format(prefix = ifelse(input$conversion == "USD to NTD", "NT$", "$"))) +
      labs(
        title = "Cumulative Profit Over Time",
        x = "Month", y = "Cumulative Profit"
      ) +
      theme_minimal()
    
    ggarrange(p1, p2, ncol = 1, nrow = 2)
  })
  
  output$financial_table <- renderDT({
    finances <- adjusted_finances()
    finances <- finances %>%
      mutate(
        Revenue_Growth = (Revenue_NTD - lag(Revenue_NTD)) / lag(Revenue_NTD) * 100,
        Profit_Growth = (Profit_NTD - lag(Profit_NTD)) / lag(Profit_NTD) * 100
      )
    datatable(
      finances %>%
        select(Month, Revenue_After_Tax, Cost_NTD, Profit_NTD, Gross_Margin, Cumulative_Profit, Revenue_Growth, Profit_Growth) %>%
        rename(
          `Month` = Month,
          `Revenue (After Tax)` = Revenue_After_Tax,
          `Cost (NTD)` = Cost_NTD,
          `Profit (NTD)` = Profit_NTD,
          `Gross Margin (%)` = Gross_Margin,
          `Cumulative Profit (NTD)` = Cumulative_Profit,
          `Revenue Growth (%)` = Revenue_Growth,
          `Profit Growth (%)` = Profit_Growth
        )
    ) %>%
      formatCurrency(c("Revenue (After Tax)", "Cost (NTD)", "Profit (NTD)", "Cumulative Profit (NTD)"), currency = ifelse(input$conversion == "USD to NTD", "NT$", "$")) %>%
      formatPercentage(c("Gross Margin (%)", "Revenue Growth (%)", "Profit Growth (%)"), 2)
  })
  
  output$download_report <- downloadHandler(
    filename = function() {
      paste("financial_report_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      pdf(file)
      finances <- adjusted_finances()
      p1 <- ggplot(finances, aes(x = Month)) +
        geom_line(aes(y = Revenue_After_Tax, color = "Revenue (After Tax)")) +
        geom_line(aes(y = Cost_NTD, color = "Cost")) +
        geom_line(aes(y = Profit_NTD, color = "Profit")) +
        scale_y_continuous(labels = dollar_format(prefix = ifelse(input$conversion == "USD to NTD", "NT$", "$"))) +
        labs(
          title = "Cost, Revenue, and Profit Over Time (Adjusted for Exchange Rate and Tax)",
          x = "Month", y = "Amount"
        ) +
        scale_color_manual(values = c("Revenue (After Tax)" = "blue", "Cost" = "red", "Profit" = "green")) +
        theme_minimal() +
        theme(legend.title = element_blank())
      
      p2 <- ggplot(finances, aes(x = Month, y = Cumulative_Profit)) +
        geom_line(color = "purple") +
        scale_y_continuous(labels = dollar_format(prefix = ifelse(input$conversion == "USD to NTD", "NT$", "$"))) +
        labs(
          title = "Cumulative Profit Over Time",
          x = "Month", y = "Cumulative Profit"
        ) +
        theme_minimal()
      print(p1)
      print(p2)
      dev.off()
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
