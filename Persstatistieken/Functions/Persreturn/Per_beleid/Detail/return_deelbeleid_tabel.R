persreturn.deelbeleid.tabel <- function(data, beleid) {
  
  # Preparation --------------------------------------------------------
  df.Persreturn.beleid.detail <- reactive({
    Persreturn <- split(data(), data()$Beleid)
    
    for (i in levels(data()$Beleid)) ({
      Persreturn[[i]] <- data.frame(table(Persreturn[[i]]$"Detail beleid", Persreturn[[i]]$Persreturn))
      Persreturn[[i]] <- cbind(Beleid = i, Persreturn[[i]])
      colnames(Persreturn[[i]]) <- c("Beleid","Detail", "Persreturn", "Freq")
      
      # Fix missing freq
      temp <- Persreturn[[i]]
      temp$Persreturn <- as.factor(temp$Persreturn)
      if(!("Ja" %in% levels(temp$Persreturn))) {
        df.Ja <- Persreturn[[i]]
        df.Ja$Persreturn <- "Ja"
        df.Ja$Freq <- 0
        Persreturn[[i]] <- rbind(df.Ja, Persreturn[[i]])
      } else if(!("Nee" %in% levels(temp$Persreturn))) {
        df.Nee <- Persreturn[[i]]
        df.Nee$Persreturn <- "Nee"
        df.Nee$Freq <- 0
        Persreturn[[i]] <- rbind(df.Nee, Persreturn[[i]])
      }
    })
    return(Persreturn[[beleid()]])
  })
  
  # Tabel ---------------------------------------------------------------------
  tabel <- reactive({
    temp <- split(df.Persreturn.beleid.detail(), df.Persreturn.beleid.detail()$Persreturn)
    temp <- data.frame(Beleid = levels(df.Persreturn.beleid.detail()$Beleid),
                       Detail = levels(df.Persreturn.beleid.detail()$Detail),
                       Ja = temp$Ja$Freq,
                       Nee = temp$Nee$Freq
    )
    colnames(temp) <- c("Beleid", "Detail beleid", "Persreturn: Ja", "Persreturn: Nee")
    return(temp)
  })
  
  # Return --------------------------------------------------------------------
  return(tabel)
}