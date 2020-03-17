bericht.alg.maand.barplot <- function(data) {
  
  # Preparation ------------------------------------------------------
  df.berichten.Maand <- reactive({
    berichten <- data.frame(table(data()$Maand))
    colnames(berichten) <- c("Maand", "Freq")
    berichten$Maand <- factor(berichten$Maand, levels = c("jan", "feb", "mrt", "apr", "mei", "jun", "jul", "aug", "sep", "okt", "nov", "dec"))
    return(berichten)
  })
  
  # Barplot (Per Maand) --------------------------------------------
  berichten.barplot.maand <- reactive({
    # Specify color pallete
    colors <- c(brewer.pal(8,"Pastel2"), brewer.pal(9, "Pastel1"))
    # Create plot
    ggplot(data=df.berichten.Maand(), aes(x=Maand, y=Freq, fill=Maand)) +
      geom_bar(position = "dodge", stat='identity') +
      xlab("Maand") +
      ylab("Aantal") +
      ggtitle("Persberichten per Maand") +
      geom_text(aes(label=Freq),
                position=position_dodge(0.9), vjust=0) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_fill_manual(values=colors)
  })
  
  return(berichten.barplot.maand)
}