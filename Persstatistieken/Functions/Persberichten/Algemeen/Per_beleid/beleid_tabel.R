bericht.alg.beleid.tabel <- function(data) {

  # Preparation ------------------------------------------------------
  df.bericht.beleid <- reactive({
    # Create dataframe for barplot --------------------------------------------
    bericht.beleid <- data.frame(table(data()$Beleid))
    
    # Rename columns ----------------------------------------------------------
    colnames(bericht.beleid) <- c("Beleid","Persberichten")
    return(bericht.beleid)
  })
  
  return(df.bericht.beleid)

}

