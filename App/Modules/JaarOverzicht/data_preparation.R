###############################################################################
# Name: Data preparation (function)
# FUCTION: prepare data for usage, clean dataset, remove un-neccessairy values
###############################################################################

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
                             alles,
                             jaar = NULL,
                             kwartaal = NULL) {
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
    
    # Jaar Toevoegen -----------------------------------------------------------
    Excel$Jaar <- as.factor(format(as.Date(Excel$Datum), format='%Y'))
    # Kwartaal toevoegen -------------------------------------------------------
    Excel$Kwartaal <- quarters(as.Date(Excel$Datum), abbreviate(FALSE))
    # Dag Toevoegen ------------------------------------------------------------
    Excel$Dag <- factor(format(as.Date(Excel$Datum), format = "%u"), levels = c(1:7))
    levels(Excel$Dag) <- c("ma", "di", "wo", "do", "vr", "za", "zo")
    # Week toevoegen -----------------------------------------------------------
    Excel$Week <- ISOweek(as.Date(Excel$Datum))
    # Maand toevoegen ----------------------------------------------------------
    Excel$Maand <- factor(format(as.Date(Excel$Datum), format = "%m"), levels = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12"))
    levels(Excel$Maand) <- c("jan", "feb", "mrt", "apr", "mei","jun","jul","aug","sep","okt","nov","dec")
    # Persconferentie toevoegen ------------------------------------------------
    Excel$Persconferentie <- NA
    for (i in 1:length(Excel$"Datum PC")) {
      if (is.na(Excel$"Datum PC"[[i]])) {
        Excel$Persconferentie[[i]] <- "Nee"
      } else {
        Excel$Persconferentie[[i]] <- "Ja"
      }
    }
  # Fixing Mistakes ------------------------------------------------------------
    # Verzender ----------------------------------------------------------------
    # Excel$Verzender <- gsub("extern", "Extern", Excel$Verzender, ignore.case = FALSE)
    # Excel$Verzender <- gsub("gouverneur", "Gouverneur", Excel$Verzender, ignore.case = FALSE)
    # Excel$Verzender <- gsub("persdienst", "Persdienst", Excel$Verzender, ignore.case = FALSE)
    # Excel$Verzender <- gsub("provincie", "Provincie", Excel$Verzender, ignore.case = FALSE)
    
    # Pu bij Pb; Persreturn; Alleen web; TV -----------------------------------
    for (i in c("Persreturn", "Alleen web")) ({
      Excel[[i]] <- gsub("ja", "Ja", Excel[[i]], ignore.case = FALSE)
      Excel[[i]] <- gsub("nee", "Nee", Excel[[i]], ignore.case = FALSE)
    })
    # # Beleid ------------------------------------------------------------------
    # Excel$Beleid <- gsub("economie", "Economie", Excel$Beleid, ignore.case = FALSE)
    # Excel$Beleid <- gsub("gouverneur", "Gouverneur", Excel$Beleid, ignore.case = FALSE)
    # Excel$Beleid <- gsub("leefmilieu", "Leefmilieu", Excel$Beleid, ignore.case = FALSE)
    # Excel$Beleid <- gsub("mobiliteit", "Mobiliteit", Excel$Beleid, ignore.case = FALSE)
    # Excel$Beleid <- gsub("Onderwijs en educatie", "Onderwijs en Educatie", Excel$Beleid, ignore.case = FALSE)
    # Excel$Beleid <- gsub("provinciebestuur", "Provinciebestuur", Excel$Beleid, ignore.case = FALSE)
    # Excel$Beleid <- gsub("ruimte", "Ruimte", Excel$Beleid, ignore.case = FALSE)
    # Excel$Beleid <- gsub("Vrije tijd", "Vrije Tijd", Excel$Beleid, ignore.case = FALSE)
    
    # Deelbeleid --------------------------------------------------------------
    # Excel$"Deelbeleid" <- gsub("De Warande", "de Warande", Excel$"Deelbeleid", ignore.case = FALSE)
    # Excel$"Deelbeleid" <- gsub("Economie, innovatie en samenleving", "Economie, Innovatie en Samenleving", Excel$"Deelbeleid", ignore.case = FALSE)
    # Excel$"Deelbeleid" <- gsub("Economie, Innovatie en samenleving", "Economie, Innovatie en Samenleving", Excel$"Deelbeleid", ignore.case = FALSE)
    # Excel$"Deelbeleid" <- gsub("Economie, innovatie en Samenleving", "Economie, Innovatie en Samenleving", Excel$"Deelbeleid", ignore.case = FALSE)
    # Excel$"Deelbeleid" <- gsub("Regionaal Landschappen", "Regionale Landschappen", Excel$"Deelbeleid", ignore.case = FALSE)
    # Excel$"Deelbeleid" <- gsub("Toerisme provincie Antwerpen", "Toerisme Provincie Antwerpen", Excel$"Deelbeleid", ignore.case = FALSE)
    # Soort -------------------------------------------------------------------
    # Excel$Soort <- gsub("persbericht", "Persbericht", Excel$Soort, ignore.case = FALSE)
    # Excel$Soort <- gsub("agendatip", "Agendatip", Excel$Soort, ignore.case = FALSE)
    # Excel$Soort <- gsub("persagenda", "Persagenda", Excel$Soort, ignore.case = FALSE)
    # Excel$Soort <- gsub("activiteitenkalender", "Activiteitenkalender", Excel$Soort, ignore.case = FALSE)
    # Excel$Soort <- gsub("evenementenkalender", "Evenementenkalender", Excel$Soort, ignore.case = FALSE)
  # As factor -----------------------------------------------------------------
    Excel$Beleid <- factor(Excel$Beleid, c("Economie", "Gouverneur", "Leefmilieu", "Mobiliteit", "Onderwijs en Educatie", "Provinciebestuur", "Ruimte", "Vrije Tijd"))
    Excel$Verzender <- factor(Excel$Verzender, c("Provincie", "Persdienst", "Gouverneur", "Extern"))
  # Split & return ------------------------------------------------------------
    if(alles){
      return(Excel)
    } else {
      if(!(jaar() %in% levels(as.factor(Excel$Jaar)))) {
        stop("Geslecteerd Jaar niet gevonden in data!")
      } else {
        Persstatistiek <- split.data.frame(Excel, Excel$Jaar)
        Persstatistiek <- Persstatistiek[[jaar()]]
        if(grepl(kwartaal(), "Jaar")) {
          return(Persstatistiek)
        } else {
          if(!(kwartaal() %in% levels(as.factor(Persstatistiek$Kwartaal)))) {
            stop("Geselecteerd kwartaal niet gevonden voor geselecteerd jaar!")
          } else {
            Persstatistiek <- split.data.frame(Persstatistiek, Persstatistiek$Kwartaal)
            return(Persstatistiek[[kwartaal()]])
          }
        }

      }
    }
  })
}