library(shiny)
library(ggplot2)
library(RColorBrewer)


# Function: Persreturn per beleid ---------------------------------------------
Persberichten.beleid.maand.tabel <- function(dataframe, beleid) {
  table.berichten.Maand.totaal.per.Beleid <- reactive({
    data.frame(
      Beleid = dataframe()[[beleid()]]$Beleid,
      Maand = dataframe()[[beleid()]]$Maand,
      Freq = dataframe()[[beleid()]]$Freq
    ) 
  })
}