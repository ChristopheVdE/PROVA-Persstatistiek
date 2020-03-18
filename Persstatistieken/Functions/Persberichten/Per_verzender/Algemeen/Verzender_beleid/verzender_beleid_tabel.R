bericht.verzender.alg.beleid.tabel <- function(data) {
  
  # Preparation --------------------------------------------------------
  df.berichten.verzender.beleid <- reactive({
    berichten <- data.frame(table(data()$Beleid, data()$Verzender))
    colnames(berichten) <- c("Beleid", "Verzender", "Freq")
    
    # Add in possible missing "Verzender"
    for(i in c("Persdienst", "Provincie", "Gouverneur", "Extern")) {
      if(!(i %in% levels(berichten$Verzender))) {
        temp <- data.frame(
          Beleid = c("Economie", "Gouverneur", "Leefmilieu", "Mobiliteit", "Onderwijs en Educatie", "Provinciebestuur", "Ruimte", "Vrije Tijd"),
          Verzender = i,
          Freq = 0
        )
        berichten <- rbind(berichten, temp)
      }
    }
    # Return dataset
    return(berichten)
  })
  
  # Table --------------------------------------------------------------
  berichten.verzender.beleid.table <- reactive({
    temp <- split(df.berichten.verzender.beleid(), df.berichten.verzender.beleid()$Verzender)
    temp <- data.frame(
      Beleid = c("Economie", "Gouverneur", "Leefmilieu", "Mobiliteit", "Onderwijd en Educatie", "Provinciebestuur", "Ruimte", "Vrije Tijd"),
      Persdienst = temp$Persdienst$Freq,
      Provincie = temp$Provincie$Freq,
      Gouverneur = temp$Gouverneur$Freq,
      Extern = temp$Extern$Freq
    )
    return(temp)
  })
  return(berichten.verzender.beleid.table)
}