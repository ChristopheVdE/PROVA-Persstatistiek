###############################################################################
# FUNCTIE: Preaparation of tables for the beleid-maand-plot-module
###############################################################################

df.prep <- function(Id, data, Xaxis, beleid = NULL) {
  df.berichten <- reactive(
    if (Id == "alg.kwartaal") {
      
    } else if (Id == "alg.maand") {
      
    } else if (Id == "alg.beleid") {
      
    } else if (Id == "beleid.maand") {
        berichten <- data.frame(table(data()$Beleid, data()[[Xaxis]]))
        colnames(berichten) <- c("Beleid", Xaxis, "Persberichten")
        for (i in 0:length(levels(berichten[[Xaxis]]))) {
          levels(berichten[[Xaxis]])[i] <- month.abb[i]
        }
        berichten[[Xaxis]] <- factor(berichten$Maand, levels = c(month.abb, "Totaal"))
        berichten <- split(berichten, berichten$Beleid)
        berichten <- berichten[[beleid]]
        return(berichten)
    } else if (Id == "beleid.beleid") {
        berichten <- split(data(), data()$Beleid)
        berichten <- data.frame("Beleid" = beleid, table(berichten[[beleid]]$"Deelbeleid"))
        colnames(berichten) <- c("Beleid", "Deelbeleid","Persberichten")
        levels(berichten$Deelbeleid) <- c(levels(berichten$Deelbeleid), "Totaal")
        return(berichten)
    } else if (Id == "verzender.alg.verzender") {
      
    } else if (Id == "verzender.alg.beleid") {
      
    } else if (Id == "verzender.maand") {
      
    } else if (Id == "type") {
      
    }
  )
  return(df.berichten())
}