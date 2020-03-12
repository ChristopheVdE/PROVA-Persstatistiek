library(shiny)
library(ggplot2)
library(RColorBrewer)


# Function: Persreturn per beleid ---------------------------------------------
Persreturn.beleid.tabel <- function(dataframe, plottitle, type) {
  # Create Table ----------------------------------------------
  persreturn.beleid.tabel <- reactive({
    if ("Beleid"==type()) {
      temp <- split(dataframe(), dataframe()$Persreturn)
      temp <- data.frame(Beleid = levels(dataframe()$Beleid),
                         Ja = temp$Ja$Freq,
                         Nee = temp$Nee$Freq
              )
      colnames(temp) <- c("Beleid", "Persreturn: Ja", "Persreturn: Nee")
      return(temp)
    } else if("Detail" == type()) {
      temp <- split(dataframe(), dataframe()$Persreturn)
      temp <- data.frame(Beleid = levels(dataframe()$Beleid),
                         Detail = levels(dataframe()$Detail),
                         Ja = temp$Ja$Freq,
                         Nee = temp$Nee$Freq
      )
      colnames(temp) <- c("Beleid", "Detail beleid", "Persreturn: Ja", "Persreturn: Nee")
      return(temp)
    }
  })
}