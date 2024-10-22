# app.R
library(shiny)

source("UserInterface.R")
source("Server.R")

shinyApp(ui = ui, server = server)
