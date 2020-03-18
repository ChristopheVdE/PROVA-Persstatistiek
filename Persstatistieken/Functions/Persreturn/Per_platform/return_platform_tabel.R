return.platform.tabel <- function(data) {

  # Preparation --------------------------------------------------------
  df.return.platform <- reactive({
    
    # Create table persreturn algemeen -------------------------------
    Algemeen <- split(data(), data()$Persreturn)
    Algemeen <- Algemeen$Ja
    Algemeen <- data.frame(table(Algemeen$Beleid, Algemeen$Persreturn))
    colnames(Algemeen) <- c("Beleid", "Algemeen", "Freq")
    Algemeen$Algemeen <- "Algemeen"
    
    # create table: TV
    TV <- split(data(), data()$TV)
    TV <- TV$Ja
    TV <- data.frame(table(TV$Beleid, TV$TV))
    colnames(TV) <- c("Beleid", "TV", "Freq")
    TV$TV <- "TV"
    
    # create Table: Web
    Web <- split(data(), data()$"Alleen web")
    Web <- Web$Ja
    Web$Ja <- "Web"
    Web <- data.frame(table(Web$Beleid, Web$"Alleen web"))
    colnames(Web) <- c("Beleid", "Alleen web", "Freq")
    Web$"Alleen web" <- "Alleen web"
    
    # Merge dataframes
    persreturn <- data.frame(Beleid = TV$Beleid,
                             Platform = c(Algemeen$Algemeen, TV$TV, Web$"Alleen web"),
                             Persreturn = c(Algemeen$Freq, TV$Freq, Web$Freq))
    return(persreturn)
  })
  
  # Table --------------------------------------------------------------
  return.platform.table <- reactive({
    temp <- split(df.return.platform(), df.return.platform()$Platform)
    temp <- data.frame(Beleid = c("Economie", "Gouverneur", "Leefmilieu", "Mobiliteit", "Onderwijs en Educatie", "Provinciebestuur", "Ruimte", "Vrije Tijd"),
                       Algemeen = temp$Algemeen$Persreturn,
                       Web = temp$"Alleen web"$Persreturn,
                       TV = temp$TV$Persreturn
    )
    colnames(temp) <- c("Beleid", "Persreturn: Algemeen", "Persreturn: Alleen web", "Persreturn: TV")
    return(temp)
  })
  return(return.platform.table)
}