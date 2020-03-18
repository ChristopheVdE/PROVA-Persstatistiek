bericht.verzender.maand.tabel <- function(data, verzender) {
  
  # Preparation ------------------------------------------------------
  df.berichten.verzender.maand <- reactive({
    berichten <- data.frame(table(data()$Verzender, data()$Maand))
    colnames(berichten) <- c("Verzender", "Maand", "Freq")
    # Add in possible missing "Verzender"
    for(i in c("Persdienst", "Provincie", "Gouverneur", "Extern")) {
      if(!(i %in% levels(berichten$Verzender))) {
        temp <- data.frame(
          Maand = levels(berichten$Maand),
          Verzender = i,
          Freq = 0
        )
        berichten <- rbind(berichten, temp)
      }
    }
    # Split on month
    berichten <- split(berichten, berichten$Verzender)
    
    # Return dataset
    return(berichten[[verzender()]])
  })
  return(df.berichten.verzender.maand)
}