# Loading packages
library(shiny)

# Activating App
if (interactive()) {
  # Source UI and Server
  source("ui.R", local = TRUE)
  source("server.R")
  
  # Start App
  shinyApp(
    ui = ui,
    server = server
  )
}