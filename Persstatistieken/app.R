# Loading packages
library(shiny)

# Activating App
if (interactive()) {
  # Source UI and Server
  source("ui.R", local = TRUE)
  source("server.R")
  
  # # source functions
  # source("./Modules/Functies/percentages.R", local = FALSE)
  
  # Start App
  shinyApp(
    ui = ui,
    server = server
  )
}