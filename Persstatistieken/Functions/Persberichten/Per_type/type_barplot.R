bericht.type.barplot <- function(data) {

  # Preparation --------------------------------------------------------
  df.berichten.type <- reactive({
    berichten <- data.frame(table(data()$Beleid, data()$Soort))
    colnames(berichten) <- c("Beleid", "Type", "Freq")
    berichten$Type <- as.factor(berichten$Type)
    
    # Add in possible missing "Type"
    for(i in c("Activiteitenkalender", "Agendatip", "Evenementenkalender", "Persagenda", "Persbericht", "Persuitnodiging")) {
      if(!(i %in% levels(berichten$Type))) {
        temp <- data.frame(
          Beleid = c("Economie", "Gouverneur", "Leefmilieu", "Mobiliteit", "Onderwijs en Educatie", "Provinciebestuur", "Ruimte", "Vrije Tijd"),
          Type = i,
          Freq = 0
        )
        berichten <- rbind(berichten, temp)
      }
    }
    # Return dataset  
    return(berichten)
  })
  
  # Barplot ------------------------------------------------------------
  berichten.type.barplot <- reactive({
    # Specify color pallete
    colors <- brewer.pal(8,"Pastel2")
    
    # Create plot
    ggplot(data=df.berichten.type(), aes(x=Beleid, y=Freq, fill=Type)) +
      geom_bar(position = "dodge", stat='identity') +
      xlab("Beleid") +
      ylab("Aantal") +
      ggtitle("Persberichten per Type") +
      geom_text(aes(label=Freq),
                position=position_dodge(0.9), vjust=0) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_fill_manual(values=colors)
  })
  return(berichten.type.barplot)
}