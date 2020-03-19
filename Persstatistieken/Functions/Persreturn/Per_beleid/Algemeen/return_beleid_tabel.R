persreturn.beleid.tabel <- function(data) {
  
  # Preparation ---------------------------------------------------------------
  df.return.beleid <- reactive({
    return.beleid <- data.frame(table(data()$Beleid,
                                      data()$Persreturn))
    colnames(return.beleid) <- c("Beleid", "Persreturn", "Freq")
    return(return.beleid)
  })
  
  # Tabel ---------------------------------------------------------------------
  tabel <- reactive({
    temp <- split(df.return.beleid(), df.return.beleid()$Persreturn)
    temp <- data.frame(Beleid = levels(df.return.beleid()$Beleid),
                       Ja = temp$Ja$Freq,
                       Nee = temp$Nee$Freq
    )
    colnames(temp) <- c("Beleid", "Persreturn: Ja", "Persreturn: Nee")
    return(temp)
  })
  
  # Return --------------------------------------------------------------------
  return(tabel)
}