bericht.type.tabel <- function(data) {

  # Preparation --------------------------------------------------------
  df.berichten.type <- reactive({
    berichten <- data.frame(table(data()$Beleid, data()$Soort))
    colnames(berichten) <- c("Beleid", "Type", "Freq")
    berichten$Type <- as.factor(berichten$Type)
    
    # Add in possible missing "Type"
    for(i in c("Activiteitenkalender", "Agendatip", "Evenementenkalender", "Persagenda", "Persbericht", "Persuitnodiging")) {
      if(!(i %in% levels(berichten$Type))) {
        temp <- data.frame(
          Beleid = c("Economie", "Gouverneur", "Leefmilieu", "Mobiliteit", "Onderwijs en Educatie", "Provinciebestuur", "Ruimte", "Vrije Tijd"),
          Type = i,
          Freq = 0
        )
        berichten <- rbind(berichten, temp)
      }
    }
    # Return dataset  
    return(berichten)
  })
  
  # Table --------------------------------------------------------------
  berichten.type.table <- reactive({
    temp <- df.berichten.type()
    temp <- split(df.berichten.type(), df.berichten.type()$Type)
    temp <- data.frame(
      Beleid = c("Economie", "Gouverneur", "Leefmilieu", "Mobiliteit", "Onderwijd en Educatie", "Provinciebestuur", "Ruimte", "Vrije Tijd"),
      Activiteitenkalender = temp$Activiteitenkalender$Freq,
      Agendatip = temp$Agendatip$Freq,
      Evenementenkalender = temp$Evenementenkalender$Freq,
      Persagenda = temp$Persagenda$Freq,
      Persbericht = temp$Persbericht$Freq,
      Persuitnodiging = temp$Persuitnodiging$Freq
    )
    return(temp)
  })

  return(berichten.type.table)
  
}