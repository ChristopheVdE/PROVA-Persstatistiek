###############################################################################
# Name: Data preparation (function)
# FUCTION: prepare data for usage, clean dataset, remove un-neccessairy values
###############################################################################

# Load packages ===============================================================
library(readxl)
library(lubridate)
# =============================================================================

# FUNCTION ====================================================================
data.preparation <- function(file, 
                             sheet, 
                             headers, 
                             manual.beleid, 
                             manual.deelbeleid, 
                             manual.verzender, 
                             manual.type, 
                             manual.return.alg, 
                             manual.return.web, 
                             manual.return.tv, 
                             manual.datum, 
                             kwartaal) {
  # Prepare dataset -----------------------------------------------------------
  Persstatistiek <- reactive({
    req(file())
    # Reading Excel -----------------------------------------------------------
    Excel <- read_excel(file(), sheet = sheet(), col_names = headers())
    # Fixing colnames ---------------------------------------------------------
    if(headers() == FALSE) {

      # Catch errors
      for(i in c(manual.beleid(), manual.deelbeleid(), manual.verzender(), manual.type(), manual.return.alg(), manual.return.web(), manual.datum())) {
        if( i == "") {
          stop("One or more columnsnames doesn't have a column assign to it")
        } else if (TRUE %in% duplicated(c(manual.beleid(), manual.deelbeleid(), manual.verzender(), manual.type(), manual.return.alg(), manual.return.web(), manual.datum()))) {
          stop("One or more columns are assigned to the same columnname")
        }
      }

      # Process columns & names
      Excel <- data.frame(
        Verzender = Excel[[paste0("...",manual.verzender())]],
        Persreturn = Excel[[paste0("...",manual.return.alg())]],
        Web = Excel[[paste0("...",manual.return.web())]],
        TV = Excel[[paste0("...",manual.return.tv())]],
        Beleid = Excel[[paste0("...",manual.beleid())]],
        Deelbeleid = Excel[[paste0("...",manual.deelbeleid())]],
        Soort = Excel[[paste0("...",manual.type())]],
        Datum = as.character(Excel[[paste0("...",manual.datum())]])
      )
      colnames(Excel)[3] <- "Alleen web"
      
    } 
    # Keep only the required columns ------------------------------------------
    else {
      for(i in colnames(Excel)) {
        if(!(i %in% c("Beleid", "Deelbeleid", "Verzender", "Persreturn", "Alleen web", "TV", "Soort", "Datum"))) {
          Excel[[i]] <- NULL
        }
        Excel$Datum <- as.character(Excel$Datum)
      }
    }
    
    # Kwartaal toevoegen ------------------------------------------------------
    Excel$Kwartaal <- quarters(as.Date(Excel$Datum))
    # Dag Toevoegen -----------------------------------------------------------
    Excel$Dag <- factor(format(as.Date(Excel$Datum), format = "%u"), levels = c(1:7))
    levels(Excel$Dag) <- c("ma", "di", "wo", "do", "vr", "za", "zo")
    # Week toevoegen ----------------------------------------------------------
    Excel$Week <- isoweek(as.Date(Excel$Datum))
    # Maand toevoeegn ---------------------------------------------------------
    Excel$Maand <- factor(format(as.Date(Excel$Datum), format = "%m"), levels = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12"))
    levels(Excel$Maand) <- c("jan", "feb", "mrt", "apr", "mei","jun","jul","aug","sep","okt","nov","dec")
    
  # Fixing Mistakes -----------------------------------------------------------
    # Verzender ---------------------------------------------------------------
    Excel$Verzender <- gsub("extern", "Extern", Excel$Verzender, ignore.case = FALSE)
    Excel$Verzender <- gsub("gouverneur", "Gouverneur", Excel$Verzender, ignore.case = FALSE)
    Excel$Verzender <- gsub("persdienst", "Persdienst", Excel$Verzender, ignore.case = FALSE)
    Excel$Verzender <- gsub("provincie", "Provincie", Excel$Verzender, ignore.case = FALSE)
    
    # Pu bij Pb; Persreturn; Alleen web; TV -----------------------------------
    for (i in c("Persreturn", "Alleen web")) ({
      Excel[[i]] <- gsub("ja", "Ja", Excel[[i]], ignore.case = FALSE)
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