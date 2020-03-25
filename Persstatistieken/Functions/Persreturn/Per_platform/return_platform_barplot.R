return.platform.barplot <- function(data) {
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
  
  # Barplot ------------------------------------------------------------
  return.platform.barplot <- reactive({
    # Specify color pallete
    colors <- brewer.pal(8,"Pastel2")
    # Create plot
    ggplot(data=df.return.platform(), aes(x=Beleid, y=Persreturn, fill=Platform)) +
      geom_bar(position = "dodge", stat='identity') +
      xlab("Beleid") +
      ylab("Aantal") +
      ggtitle("Persreturn per medium") +
      geom_text(aes(label=Persreturn),
                position=position_dodge(0.9), vjust=0) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_fill_manual(values=colors)
  })
  return(return.platform.barplot)
}