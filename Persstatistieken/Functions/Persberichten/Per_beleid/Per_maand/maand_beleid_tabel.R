bericht.beleid.maand.tabel <- function(data, beleid) {
  
  # Preparation ----------------------------------------------------
  df.berichten.Maand.totaal.per.Beleid <-  reactive({
    berichten <- data.frame(table(data()$Beleid, data()$Maand))
    colnames(berichten) <- c("Beleid", "Maand", "Freq")
    berichten$Maand <- factor(berichten$Maand, levels = c("jan", "feb", "mrt", "apr", "mei", "jun", "jul", "aug", "sep", "okt", "nov", "dec"))
    split(berichten, berichten$Beleid)
  })
  
  table.berichten.Maand.totaal.per.Beleid <- reactive({
    data.frame(
      Beleid = df.berichten.Maand.totaal.per.Beleid()[[beleid()]]$Beleid,
      Maand = df.berichten.Maand.totaal.per.Beleid()[[beleid()]]$Maand,
      Freq = df.berichten.Maand.totaal.per.Beleid()[[beleid()]]$Freq
    ) 
  })
  
  return(table.berichten.Maand.totaal.per.Beleid)

}