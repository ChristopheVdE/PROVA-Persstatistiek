###############################################################################
# Name: Data preparation (function)
# FUCTION: prepare data for usage, clean dataset, remove un-neccessairy values
###############################################################################

# Load packages ===============================================================
library(readxl)
library(lubridate)
# =============================================================================

# FUNCTION ====================================================================
data.preparation <- function(file, sheet, headers, manual.headers, kwartaal) {
  # Prepare dataset -----------------------------------------------------------
  Persstatistiek <- reactive({
    req(file())
    # Reading Excel -----------------------------------------------------------
    Excel <- read_excel(file(), sheet = sheet(), col_names = headers())
    # Fixing colnames ---------------------------------------------------------
    if(headers() == FALSE) {
      
      # Catch errors
      for(i in manual.headers()) {
        if( i == "") {
          stop("One or more columnsnames doesn't have a column assign to it")
        } else if (TRUE %in% duplicated(manual.headers())) {
          stop("One or more columns are assigned to the same columnname")
        }
      }
      # prep  
      for (i in 1:length(manual.headers())) {
        manual.headers()[i] <- paste0("...", manual.headers()[i])
      }
      
      # Process columns & names
      Excel <- data.frame(
        Kwartaal = Excel[[manual.headers()[3]]],
        Verzender = Excel[[manual.headers()[4]]],
        Persreturn = Excel[[manual.headers()[6]]],
        Web = Excel[[manual.headers()[7]]],
        TV = Excel[[manual.headers()[8]]],
        Beleid = Excel[[manual.headers()[1]]],
        Detail = Excel[[manual.headers()[2]]],
        Soort = Excel[[manual.headers()[5]]],
        Maand = Excel[[manual.headers()[9]]]
      )
      colnames(Excel)[4] <- "Alleen web"
      colnames(Excel)[7] <- "Deelbeleid"
      
      # Errorcatching kwartaal niet gevonden  
      if(!(kwartaal() %in% levels(Excel$Kwartaal))) {
        stop("Verkeerde kolomnr gekoppeld aan kolom 'Kwartaal': kan geselecteerd kwartaal niet terugvinden in kolom. ")
      }
    } 
    # Keep only the required columns ------------------------------------------
    else {
      for(i in colnames(Excel)) {
        if(!(i %in% c("Beleid", "Deelbeleid", "Kwartaal", "Verzender", "Persreturn", "Alleen web", "TV", "Soort", "Datum", "Maand"))) {
          Excel[[i]] <- NULL
        }
      }
    }
    # Check for and remove possible NA values ---------------------------------
    Excel <- Excel[complete.cases(Excel),]
  # Fixing Mistakes -----------------------------------------------------------
    # Verzender ---------------------------------------------------------------
    Excel$Verzender <- gsub("extern", "Extern", Excel$Verzender, ignore.case = FALSE)
    Excel$Verzender <- gsub("gouverneur", "Gouverneur", Excel$Verzender, ignore.case = FALSE)
    Excel$Verzender <- gsub("persdienst", "Persdienst", Excel$Verzender, ignore.case = FALSE)
    Excel$Verzender <- gsub("provincie", "Provincie", Excel$Verzender, ignore.case = FALSE)
    
    # Pu bij Pb; Persreturn; Alleen web; TV -----------------------------------
    for (i in c("Persreturn", "Alleen web", "TV")) ({
      Excel[[i]] <- gsub("Ja", "Ja", Excel[[i]], ignore.case = FALSE)
      Excel[[i]] <- gsub("nee", "Nee", Excel[[i]], ignore.case = FALSE)
    })
    # Beleid ------------------------------------------------------------------
    Excel$Beleid <- gsub("economie", "Economie", Excel$Beleid, ignore.case = FALSE)
    Excel$Beleid <- gsub("gouverneur", "Gouverneur", Excel$Beleid, ignore.case = FALSE)
    Excel$Beleid <- gsub("leefmilieu", "Leefmilieu", Excel$Beleid, ignore.case = FALSE)
    Excel$Beleid <- gsub("mobiliteit", "Mobiliteit", Excel$Beleid, ignore.case = FALSE)
    Excel$Beleid <- gsub("Onderwijs en educatie", "Onderwijs en Educatie", Excel$Beleid, ignore.case = FALSE)
    Excel$Beleid <- gsub("provinciebestuur", "Provinciebestuur", Excel$Beleid, ignore.case = FALSE)
    Excel$Beleid <- gsub("ruimte", "Ruimte", Excel$Beleid, ignore.case = FALSE)
    Excel$Beleid <- gsub("Vrije tijd", "Vrije Tijd", Excel$Beleid, ignore.case = FALSE)
    
    # Deelbeleid --------------------------------------------------------------
    Excel$"Deelbeleid" <- gsub("De Warande", "de Warande", Excel$"Deelbeleid", ignore.case = FALSE)
    Excel$"Deelbeleid" <- gsub("Economie, innovatie en samenleving", "Economie, Innovatie en Samenleving", Excel$"Deelbeleid", ignore.case = FALSE)
    Excel$"Deelbeleid" <- gsub("Economie, Innovatie en samenleving", "Economie, Innovatie en Samenleving", Excel$"Deelbeleid", ignore.case = FALSE)
    Excel$"Deelbeleid" <- gsub("Economie, innovatie en Samenleving", "Economie, Innovatie en Samenleving", Excel$"Deelbeleid", ignore.case = FALSE)
    Excel$"Deelbeleid" <- gsub("Regionaal Landschappen", "Regionale Landschappen", Excel$"Deelbeleid", ignore.case = FALSE)
    Excel$"Deelbeleid" <- gsub("Toerisme provincie Antwerpen", "Toerisme Provincie Antwerpen", Excel$"Deelbeleid", ignore.case = FALSE)
    # Soort -------------------------------------------------------------------
    Excel$Soort <- gsub("persbericht", "Persbericht", Excel$Soort, ignore.case = FALSE)
    Excel$Soort <- gsub("agendatip", "Agendatip", Excel$Soort, ignore.case = FALSE)
    Excel$Soort <- gsub("persagenda", "Persagenda", Excel$Soort, ignore.case = FALSE)
    Excel$Soort <- gsub("activiteitenkalender", "Activiteitenkalender", Excel$Soort, ignore.case = FALSE)
    Excel$Soort <- gsub("evenementenkalender", "Evenementenkalender", Excel$Soort, ignore.case = FALSE)
    # Datum, dag, week & maand ------------------------------------------------
    Excel$Datum <- as.character(Excel$Datum)
    Excel$Dag <- factor(weekdays(as.Date(Excel$Datum), abbreviate = TRUE), levels = c("Ma", "Di", "Wo", "Do","Vr", "Za", "Zo"))
    Excel$Week <- isoweek(as.Date(Excel$Datum))
    Excel$Maand <- factor(months(as.Date(Excel$Datum), abbreviate = TRUE), levels = c("jan", "feb", "mrt", "apr", "mei","jun","jul","aug","sep","okt","nov","dec"))
    levels(Excel$Maand) <- month.abb
  # As factor -----------------------------------------------------------------
    for (i in c("Verzender", "Beleid")) ({
      Excel[[i]] <- as.factor(Excel[[i]])
    })
  # Split ---------------------------------------------------------------------
    Persstatistiek <- split.data.frame(Excel, Excel$Kwartaal)
    Persstatistiek$Jaar <- Excel
  # Removing Excel ------------------------------------------------------------
    Excel <- NULL
  # Return dataset ------------------------------------------------------------
    return(Persstatistiek[[kwartaal()]])
  })
}