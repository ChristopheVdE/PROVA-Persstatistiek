###############################################################################
# FUNCTIE: Preaparation of tables for the beleid-maand-plot-module
###############################################################################

df.prep <- function(Id, data, beleid = NULL) {
  df.berichten <- reactive(
  # Algemeen Kwartaal ---------------------------------------------------------
    if (Id == "alg.kwartaal") {
        # Create dummy dataframe
        berichten <- data.frame(
          Kwartaal = c("Q1", "Q2", "Q3", "Q4"),
          Persberichten = 0
        )
        berichten$Kwartaal <- factor(berichten$Kwartaal, c("Q1", "Q2", "Q3", "Q4", "Totaal"))
        
        # Create table
        temp <- data.frame(table(data()$Kwartaal))
        colnames(temp) <- c("Kwartaal", "Persberichten")
        
        # Update values of dummy dataframe
        for (i in temp$Kwartaal) {
          berichten$Persberichten[grepl(i, berichten$Kwartaal)] <- temp$Persberichten[grepl(i, temp$Kwartaal)]
        }
        return(berichten)
    } 
  # Algemeen Maand ------------------------------------------------------------
    else if (Id == "alg.maand") {
        berichten <- data.frame(table(data()$Maand))
        colnames(berichten) <- c("Maand", "Persberichten")
        for (i in 0:length(levels(berichten$Maand))) {
          levels(berichten$Maand)[i] <- month.abb[i]
        }
        berichten$Maand <- factor(berichten$Maand, levels = c(month.abb, "Totaal"))
        return(berichten)
    } 
  # Algemeen Beleid -----------------------------------------------------------
    else if (Id == "alg.beleid") {-
        berichten <- data.frame(table(data()$Beleid))
        colnames(berichten) <- c("Beleid","Persberichten")
        return(berichten)
    } 
  # Per beleid: Maand ---------------------------------------------------------
    else if (Id == "beleid.maand") {
        berichten <- data.frame(table(data()$Beleid, data()$Maand))
        colnames(berichten) <- c("Beleid", "Maand", "Persberichten")
        for (i in 0:length(levels(berichten$Maand))) {
          levels(berichten$Maand)[i] <- month.abb[i]
        }
        berichten$Maand <- factor(berichten$Maand, levels = c(month.abb, "Totaal"))
        berichten <- split(berichten, berichten$Beleid)
        berichten <- berichten[[beleid]]
        return(berichten)
    }
  # Per beleid: Deelbeleid ----------------------------------------------------
    else if (Id == "beleid.beleid") {
        berichten <- split(data(), data()$Beleid)
        berichten <- data.frame("Beleid" = beleid, table(berichten[[beleid]]$"Deelbeleid"))
        colnames(berichten) <- c("Beleid", "Deelbeleid","Persberichten")
        levels(berichten$Deelbeleid) <- c(levels(berichten$Deelbeleid), "Totaal")
        return(berichten)
    } 
  # Verzender Algemeen - Verzender --------------------------------------------
    else if (Id == "verzender.alg.verzender") {
      berichten <- data.frame(table(data()$Verzender))
      colnames(berichten) <- c("Verzender", "Persberichten")
      for(i in c("Persdienst", "Provincie", "Gouverneur", "Extern")) {
        if(!(i %in% levels(berichten$Verzender))) {
          temp <- data.frame(
            Verzender = i,
            Persberichten = 0
          )
          berichten <- rbind(berichten, temp)
        }
      }
      return(berichten)
    } 
  # Verzender Algemeen - Beleid -----------------------------------------------
    else if (Id == "verzender.alg.beleid") {
      berichten <- data.frame(table(data()$Beleid, data()$Verzender))
      colnames(berichten) <- c("Beleid", "Verzender", "Persberichten")
      for(i in c("Persdienst", "Provincie", "Gouverneur", "Extern")) {
        if(!(i %in% levels(berichten$Verzender))) {
          temp <- data.frame(
            Beleid = c("Economie", "Gouverneur", "Leefmilieu", "Mobiliteit", "Onderwijs en Educatie", "Provinciebestuur", "Ruimte", "Vrije Tijd"),
            Verzender = i,
            Persberichten = 0
          )
          berichten <- rbind(berichten, temp)
        }
      }
      return(berichten)
    }
  # Verzender: Maand ----------------------------------------------------------
    else if (Id == "verzender.maand") {
      berichten <- data.frame(table(data()$Verzender, data()$Maand))
      colnames(berichten) <- c("Verzender", "Maand", "Persberichten")
      for(i in c("Persdienst", "Provincie", "Gouverneur", "Extern")) {
        if(!(i %in% levels(berichten$Verzender))) {
          temp <- data.frame(
            Maand = levels(berichten$Maand),
            Verzender = i,
            Persbreichten = 0
          )
          berichten <- rbind(berichten, temp)
        }
      }
      berichten <- split(berichten, berichten$Verzender)
      return(berichten[[verzender()]])
    } 
  # Type ----------------------------------------------------------------------
    else if (Id == "type") {
      berichten <- data.frame(table(data()$Beleid, data()$Soort))
      colnames(berichten) <- c("Beleid", "Type", "Persbereichten")
      berichten$Type <- as.factor(berichten$Type)
      for(i in c("Activiteitenkalender", "Agendatip", "Evenementenkalender", "Persagenda", "Persbericht", "Persuitnodiging")) {
        if(!(i %in% levels(berichten$Type))) {
          temp <- data.frame(
            Beleid = c("Economie", "Gouverneur", "Leefmilieu", "Mobiliteit", "Onderwijs en Educatie", "Provinciebestuur", "Ruimte", "Vrije Tijd"),
            Type = i,
            Persberichten = 0
          )
          berichten <- rbind(berichten, temp)
        }
      }
      return(berichten)
    }
  )
  return(df.berichten())
}