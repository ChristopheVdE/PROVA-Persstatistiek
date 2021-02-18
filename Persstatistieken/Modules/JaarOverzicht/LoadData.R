###############################################################################
# Name: Load Basis Data (function)
# FUCTION: Load basis data for usage
###############################################################################

# Load packages ===============================================================
library(readxl)
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
                             manual.persconferentie,
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
        Persconferentie.Datum  = as.character(Excel[[paste0("...",manual.persconferentie())]]),
        Datum = as.character(Excel[[paste0("...",manual.datum())]])
      )
      colnames(Excel)[3] <- "Alleen web"
      colnames(Excel)[8] <- "Datum PC"
      
    } 
    # Keep only the required columns ------------------------------------------
    else {
      for(i in colnames(Excel)) {
        if(!(i %in% c("Beleid", "Deelbeleid", "Datum PC", "Verzender", "Persreturn", "Alleen web", "TV", "Soort", "Datum"))) {
          Excel[[i]] <- NULL
        }
        Excel$Datum <- as.character(Excel$Datum)
        Excel$"Datum PC" <- as.character(Excel$"Datum PC")
      }
    }
    return(Excel()) 
  })
}
# ==============================================================================