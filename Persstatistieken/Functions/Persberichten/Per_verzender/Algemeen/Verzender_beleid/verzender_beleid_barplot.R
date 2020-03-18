bericht.verzender.alg.beleid.barplot <- function(data) {
  
  # Preparation --------------------------------------------------------
  df.berichten.verzender.beleid <- reactive({
    berichten <- data.frame(table(data()$Beleid, data()$Verzender))
    colnames(berichten) <- c("Beleid", "Verzender", "Freq")
    
    # Add in possible missing "Verzender"
    for(i in c("Persdienst", "Provincie", "Gouverneur", "Extern")) {
      if(!(i %in% levels(berichten$Verzender))) {
        temp <- data.frame(
          Beleid = c("Economie", "Gouverneur", "Leefmilieu", "Mobiliteit", "Onderwijs en Educatie", "Provinciebestuur", "Ruimte", "Vrije Tijd"),
          Verzender = i,
          Freq = 0
        )
        berichten <- rbind(berichten, temp)
      }
    }
    # Return dataset
    return(berichten)
  })
  
  # Barplot ------------------------------------------------------------
  berichten.verzender.beleid.barplot <- reactive({
    # Specify color pallete
    colors <- brewer.pal(8,"Pastel2")
    # Create plot
    ggplot(data=df.berichten.verzender.beleid(), aes(x=Verzender, y=Freq, fill=Beleid)) +
      geom_bar(position = "dodge", stat='identity') +
      xlab("Verzender") +
      ylab("Aantal") +
      ggtitle("Persberichten per Verzender") +
      geom_text(aes(label=Freq),
                position=position_dodge(0.9), vjust=0) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_fill_manual(values=colors)
  })
  return(berichten.verzender.beleid.barplot)
}

