# ui.R

library(shiny)
library(shinydashboard)

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
