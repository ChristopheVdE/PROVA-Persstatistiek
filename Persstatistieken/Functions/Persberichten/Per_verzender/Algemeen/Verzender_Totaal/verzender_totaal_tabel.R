bericht.verzender.alg.totaal.tabel <- function(data) {
  
  # Preparation --------------------------------------------------------
  df.berichten.verzender.totaal <- reactive({
    berichten <- data.frame(table(data()$Verzender))
    colnames(berichten) <- c("Verzender", "Freq")
    
    # Add in possible missing "Verzender"
    for(i in c("Persdienst", "Provincie", "Gouverneur", "Extern")) {
      if(!(i %in% levels(berichten$Verzender))) {
        temp <- data.frame(
          Verzender = i,
          Freq = 0
        )
        berichten <- rbind(berichten, temp)
      }
    }
    # Return dataset
    return(berichten)
  })
  
  return(df.berichten.verzender.totaal)
}