###############################################################################
# FUNCTION: Persberichten - Calcuate percentages
###############################################################################

# Calculate percentages =======================================================
calc_percentages <- function(Id, data) {
# Simple tables ---------------------------------------------------------------
  if (!(Id == "verzender.alg.beleid" || Id == "type" || Id == "return.beleid.alg" || Id == "return.beleid.beleid" || Id == "return.medium")) {
    total <- sum(data$Persberichten)
    column <- NULL
    for (i in 1:length(data$Persberichten)) {
      column <- c(column,
                 round(as.numeric(data$Persberichten[[i]] / total * 100), digits = 2))
    }
    return(column)
  } 
# Persberichten per beleid per verzender --------------------------------------
  else if (Id == "verzender.alg.beleid") {
    # split dataframe on "Verzender"
      temp <- split(data, data$Verzender)
    # Create empty dummy columns
      column.Extern <- NULL
      column.Gouverneur <- NULL
      column.Persdienst <- NULL
      column.Provincie <- NULL
    # Calculate totals
      total.Extern <- sum(temp[["Extern"]]$Persberichten)
      total.Gouverneur <- sum(temp[["Gouverneur"]]$Persberichten)
      total.Persdiest <- sum(temp[["Persdienst"]]$Persberichten)
      total.Provincie <- sum(temp[["Provincie"]]$Persberichten)
    # Calculate percentages  
      # Extern
      for (i in 1:length(temp[["Extern"]]$Persberichten)) {
        column.Extern <- c(column.Extern,
                           round(as.numeric(temp[["Extern"]]$Persberichten[[i]] / total.Extern * 100), digits = 2))
      }
      # Gouverneur
      for (i in 1:length(temp[["Gouverneur"]]$Persberichten)) {
        column.Gouverneur <- c(column.Gouverneur,
                               round(as.numeric(temp[["Gouverneur"]]$Persberichten[[i]] / total.Gouverneur * 100), digits = 2))
      }
      # Persdienst
      for (i in 1:length(temp[["Persdienst"]]$Persberichten)) {
        column.Persdienst <- c(column.Persdienst,
                               round(as.numeric(temp[["Persdienst"]]$Persberichten[[i]] / total.Persdiest * 100), digits = 2))
      }
      # Provincie
      for (i in 1:length(temp[["Provincie"]]$Persberichten)) {
        column.Provincie <- c(column.Provincie,
                              round(as.numeric(temp[["Provincie"]]$Persberichten[[i]] / total.Provincie * 100), digits = 2))
      }

    # Merge
      column <- c(column.Extern, column.Gouverneur, column.Persdienst, column.Provincie)
    # Return
      return(column)
  }
# Persberichten per type ------------------------------------------------------
  else if (Id == "type") {
  # split dataframe on "Verzender"
    temp <- split(data, data$Beleid)
  # Create empty dummy columns
    column.Economie <- NULL
    column.Gouverneur <- NULL
    column.Leefmilieu <- NULL
    column.Mobiliteit <- NULL
    column.Onderwijs <- NULL
    column.Provinciebestuur <- NULL
    column.Ruimte <- NULL
    column.VrijeTijd <- NULL
  # Calculate totals
    total.Economie <- sum(temp[["Economie"]]$Persberichten)
    total.Gouverneur <- sum(temp[["Gouverneur"]]$Persberichten)
    total.Leefmilieu <- sum(temp[["Leefmilieu"]]$Persberichten)
    total.Mobiliteit <- sum(temp[["Mobiliteit"]]$Persberichten)
    total.Onderwijs <- sum(temp[["Onderwijs en Educatie"]]$Persberichten)
    total.Provinciebestuur <- sum(temp[["Provinciebestuur"]]$Persberichten)
    total.Ruimte <- sum(temp[["Ruimte"]]$Persberichten)
    total.VrijeTijd <- sum(temp[["Vrije Tijd"]]$Persberichten)
  # Calculate percentages  
    # Economie
    for (i in 1:length(temp[["Economie"]]$Persberichten)) {
      column.Economie <- c(column.Economie,
                         round(as.numeric(temp[["Economie"]]$Persberichten[[i]] / total.Economie* 100), digits = 2))
    }
    # Evenementenkalender
    for (i in 1:length(temp[["Gouverneur"]]$Persberichten)) {
      column.Gouverneur <- c(column.Gouverneur,
                             round(as.numeric(temp[["Gouverneur"]]$Persberichten[[i]] / total.Gouverneur * 100), digits = 2))
    }
    # Persagenda
    for (i in 1:length(temp[["Leefmilieu"]]$Persberichten)) {
      column.Leefmilieu <- c(column.Leefmilieu,
                             round(as.numeric(temp[["Leefmilieu"]]$Persberichten[[i]] / total.Leefmilieu * 100), digits = 2))
    }
    # Mobiliteit
    for (i in 1:length(temp[["Mobiliteit"]]$Persberichten)) {
      column.Mobiliteit <- c(column.Mobiliteit,
                            round(as.numeric(temp[["Mobiliteit"]]$Persberichten[[i]] / total.Mobiliteit * 100), digits = 2))
    }
    # Onderwijs
    for (i in 1:length(temp[["Onderwijs en Educatie"]]$Persberichten)) {
      column.Onderwijs <- c(column.Onderwijs,
                            round(as.numeric(temp[["Onderwijs en Educatie"]]$Persberichten[[i]] / total.Onderwijs * 100), digits = 2))
    }
    # Provinciebestuur
    for (i in 1:length(temp[["Provinciebestuur"]]$Persberichten)) {
      column.Provinciebestuur <- c(column.Provinciebestuur,
                            round(as.numeric(temp[["Provinciebestuur"]]$Persberichten[[i]] / total.Provinciebestuur * 100), digits = 2))
    }
    # Ruimte
    for (i in 1:length(temp[["Ruimte"]]$Persberichten)) {
      column.Ruimte <- c(column.Ruimte,
                         round(as.numeric(temp[["Ruimte"]]$Persberichten[[i]] / total.Ruimte * 100), digits = 2))
    }
    # Vrije Tijd
    for (i in 1:length(temp[["Vrije Tijd"]]$Persberichten)) {
      column.VrijeTijd <- c(column.VrijeTijd,
                                   round(as.numeric(temp[["Vrije Tijd"]]$Persberichten[[i]] / total.VrijeTijd * 100), digits = 2))
    }
  # Merge
    column <- c(column.Economie, column.Gouverneur, column.Leefmilieu, column.Mobiliteit, column.Onderwijs, column.Provinciebestuur, column.Ruimte, column.VrijeTijd)
  # Return
    return(column)
  }
}
# =============================================================================