bericht.beleid.maand.barplot <- function(data, beleid) {

  # Preparation ----------------------------------------------------
  df.berichten.Maand.totaal.per.Beleid <-  reactive({
    berichten <- data.frame(table(data()$Beleid, data()$Maand))
    colnames(berichten) <- c("Beleid", "Maand", "Freq")
    berichten$Maand <- factor(berichten$Maand, levels = c("jan", "feb", "mrt", "apr", "mei", "jun", "jul", "aug", "sep", "okt", "nov", "dec"))
    split(berichten, berichten$Beleid)
  })
  
  # Barplot (Per Maand) ---------------------------------------------
  barplot.berichten.Maand.totaal.per.Beleid <- reactive({
    # Specify color pallete
    colors <- c(brewer.pal(8,"Pastel2"), brewer.pal(9, "Pastel1"))
    # Create plot
    ggplot(data=df.berichten.Maand.totaal.per.Beleid()[[beleid()]], aes(x=Maand, y=Freq, fill=Maand)) +
      geom_bar(position = "dodge", stat='identity') +
      xlab("Maand") +
      ylab("Aantal") +
      ggtitle(paste0("Persberichten: Maand per Beleid (", paste0(beleid(), ")"))) +
      geom_text(aes(label=Freq),
                position=position_dodge(0.9), vjust=0) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 0, hjust = 0.5)) +
      scale_fill_manual(values=colors)
  })
  
  return(barplot.berichten.Maand.totaal.per.Beleid)
  
}




