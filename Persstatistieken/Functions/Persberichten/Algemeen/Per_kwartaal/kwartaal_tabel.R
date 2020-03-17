bericht.alg.kwartaal.tabel <- function(data) {
  
  # Preparation ------------------------------------------------------
  df.berichten.Kwartaal <-  reactive({
    berichten <- data.frame(table(data()$Kwartaal))
    colnames(berichten) <- c("Kwartaal", "Freq")
    return(berichten)
  })

  return(df.berichten.Kwartaal)
  
}