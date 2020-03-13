library(shiny)
library(ggplot2)
library(RColorBrewer)

# SERVER function ---------------------------------------------
Persberichten.beleid.tabel <- function(dataframe, plottitle, type) {
  # Create Table ----------------------------------------------
  persberichten.beleid.table <- reactive({
    dataframe()
  })
}