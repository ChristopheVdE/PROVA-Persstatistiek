###############################################################################
# Table creation: convert dataframes into nice clean tables to display
###############################################################################

tables <- function(Id, data) {
# Persberichten:  verzenders per beleid ---------------------------------------
  if(Id == "verzender.alg.beleid") {
    temp <- split(data, data$Verzender)
  # Persberichten aantal
    table.aantal <- data.frame(
      Beleid = levels(data$Beleid),
      Persdienst = temp$Persdienst$Persberichten,
      Provincie = temp$Provincie$Persberichten,
      Gouverneur = temp$Gouverneur$Persberichten,
      Extern = temp$Extern$Persberichten
    )
  # Persberichten procentueel
    table.procent <- data.frame(
      Beleid = levels(data$Beleid),
      Persdienst = temp$Persdienst$Procentueel,
      Provincie = temp$Provincie$Procentueel,
      Gouverneur = temp$Gouverneur$Procentueel,
      Extern = temp$Extern$Procentueel
    )
  # Return
    return(list(aantal = table.aantal, procent = table.procent))
  }
# Persberichten per type ------------------------------------------------------
  else if (Id == "type") {
    temp <- split(data, data$Type)
  # Persberichten aantal
    table.aantal <- data.frame(
      Beleid = levels(data$Beleid),
      Activiteitenkalender = temp$Activiteitenkalender$Persberichten,
      Agendatip = temp$Agendatip$Persberichten,
      Evenementenkalender = temp$Evenementenkalender$Persberichten,
      Persagenda = temp$Persagenda$Persberichten,
      Persbericht = temp$Persbericht$Persberichten,
      Persuitnodiging = temp$Persuitnodiging$Persberichten
    )
  # Persberichten procentueel
    table.procent <- data.frame(
      Beleid = levels(data$Beleid),
      Activiteitenkalender = temp$Activiteitenkalender$Procentueel,
      Agendatip = temp$Agendatip$Procentueel,
      Evenementenkalender = temp$Evenementenkalender$Procentueel,
      Persagenda = temp$Persagenda$Procentueel,
      Persbericht = temp$Persbericht$Procentueel,
      Persuitnodiging = temp$Persuitnodiging$Procentueel
    )
  # Return
    return(list(aantal = table.aantal, procent = table.procent))
  }
# Persreturn per beleid (alg) -------------------------------------------------
  else if (Id == "return.beleid.alg") {
    
  }
# Persreturn per deelbeleid ---------------------------------------------------
}