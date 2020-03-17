bericht.alg.maand.tabel <- function(data) {

  # Preparation ------------------------------------------------------
  df.berichten.Maand <- reactive({
    berichten <- data.frame(table(data()$Maand))
    colnames(berichten) <- c("Maand", "Freq")
    berichten$Maand <- factor(berichten$Maand, levels = c("jan", "feb", "mrt", "apr", "mei", "jun", "jul", "aug", "sep", "okt", "nov", "dec"))
    return(berichten)
  })
  
  return(df.berichten.Maand)

}

