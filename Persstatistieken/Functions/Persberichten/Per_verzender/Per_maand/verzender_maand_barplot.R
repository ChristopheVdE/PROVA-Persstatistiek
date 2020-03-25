bericht.verzender.maand.barplot <- function(data, verzender) {

  # Preparation ------------------------------------------------------
  df.berichten.verzender.maand <- reactive({
    berichten <- data.frame(table(data()$Verzender, data()$Maand))
    colnames(berichten) <- c("Verzender", "Maand", "Freq")
    # Add in possible missing "Verzender"
    for(i in c("Persdienst", "Provincie", "Gouverneur", "Extern")) {
      if(!(i %in% levels(berichten$Verzender))) {
        temp <- data.frame(
          Maand = levels(berichten$Maand),
          Verzender = i,
          Freq = 0
        )
        berichten <- rbind(berichten, temp)
      }
    }
    # Split on month
    berichten <- split(berichten, berichten$Verzender)
    
    # Return dataset
    return(berichten[[verzender()]])
  })

  # Barplot ----------------------------------------------------------
  berichten.verzender.maand.barplot <- reactive({
    # Specify color pallete
    colors <- c(brewer.pal(8,"Pastel2"), brewer.pal(9, "Pastel1"))
    # Create plot
    ggplot(data=df.berichten.verzender.maand(), aes(x=Maand, y=Freq, fill=Maand)) +
      geom_bar(position = "dodge", stat='identity') +
      xlab("Maand") +
      ylab("Aantal") +
      ggtitle(paste0("Persberichten per Maand: ", verzender())) +
      geom_text(aes(label=Freq),
                position=position_dodge(0.9), vjust=0) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_fill_manual(values=colors)
  })
  return(berichten.verzender.maand.barplot)
}