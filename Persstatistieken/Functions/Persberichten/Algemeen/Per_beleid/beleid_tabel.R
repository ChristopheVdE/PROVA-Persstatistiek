###############################################################################
# Functie voor tabel: Persberichten algemeen per beleid
###############################################################################

# Function ====================================================================
bericht.alg.beleid.tabel <- function(data) {

  # Preparation ---------------------------------------------------------------
  df.bericht.beleid <- reactive({
    # Basic dataframe for barplot ---------------------------------------------
    bericht.beleid <- data.frame(table(data()$Beleid))
    colnames(bericht.beleid) <- c("Beleid","Persberichten")
    
    # Add Total ---------------------------------------------------------------
    levels(bericht.beleid$Beleid) <- c(levels(bericht.beleid$Beleid), "Totaal")
    total <- sum(bericht.beleid$Persberichten)
    bericht.beleid <- rbind(bericht.beleid, c("Totaal", total))
    
    # Add Percentages ---------------------------------------------------------
    for (i in 0:length(bericht.beleid$Persberichten)) {
      if (i == 0) {
        column <- NULL
      } else {
        column <- c(column, 
                   (as.numeric(bericht.beleid$Persberichten[[i]]) / total * 100)  
                  )
      }
    }
    bericht.beleid <- cbind(bericht.beleid, column)
    colnames(bericht.beleid) <- c("Beleid","Persberichten", "Procentueel")
    
    return(bericht.beleid)
  })
  return(df.bericht.beleid)
}

