bericht.beleid.beleid.tabel <- function(data, beleid) {
  
  # Preparation ---------------------------------------------------------
  df.persberichten.beleid.detail <- reactive({
    Persberichten <- split(data(), data()$Beleid)
    Persberichten <- data.frame(table(Persberichten[[beleid()]]$"Detail beleid"))
    Persberichten <- cbind(Beleid = beleid(), Persberichten)
    colnames(Persberichten) <- c("Beleid", "Detail","Persberichten")
    return(Persberichten)
  })
  return(df.persberichten.beleid.detail)
}