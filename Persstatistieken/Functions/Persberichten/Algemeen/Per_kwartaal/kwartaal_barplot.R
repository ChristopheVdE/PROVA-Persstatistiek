bericht.alg.kwartaal.barplot <- function(data) {
  
  # Preparation ------------------------------------------------------
  df.berichten.Kwartaal <-  reactive({
    berichten <- data.frame(table(data()$Kwartaal))
    colnames(berichten) <- c("Kwartaal", "Freq")
    berichten$Kwartaal <- factor(berichten$Kwartaal, c("Q1", "Q2", "Q3", "Q4"))
    for (i in c("Q1", "Q2", "Q3", "Q4")) {
      if (!(i %in% berichten$Kwartaal)) {
        berichten <- rbind(berichten, c(i, 0))
      }
    }
    return(berichten)
  })
  
  # Barplot ----------------------------------------------------------
  berichten.kwartaal.barplot <- reactive({
    # Specify color pallete
    colors <- c(brewer.pal(8,"Pastel2"), brewer.pal(9, "Pastel1"))
    # Create plot
    ggplot(data=df.berichten.Kwartaal(), aes(x=Kwartaal, y=Freq, fill=Kwartaal)) +
      geom_bar(position = "dodge", stat='identity') +
      xlab("Kwartaal") +
      ylab("Aantal") +
      ggtitle("Persberichten per Kwartaal") +
      geom_text(aes(label=Freq),
                position=position_dodge(0.9), vjust=0) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_fill_manual(values=colors)
  })
  
  return(berichten.kwartaal.barplot)
  
}