bericht.alg.beleid.barplot <- function(data) {
  
  # Preparation ------------------------------------------------------
  df.bericht.beleid <- reactive({
    # Create dataframe for barplot --------------------------------------------
    bericht.beleid <- data.frame(table(data()$Beleid))
    
    # Rename columns ----------------------------------------------------------
    colnames(bericht.beleid) <- c("Beleid","Persberichten")
    return(bericht.beleid)
  })
  
  # Define color pallete --------------------------------------
  colors <- c(brewer.pal(8,"Pastel2"), brewer.pal(9, "Pastel1"))
  
  # Barplot (Per beleid) --------------------------------------------
  berichten.barplot.maand <- reactive( 
    ggplot(data=df.bericht.beleid(), aes(x=Beleid, y=Persberichten, fill=Beleid)) +
    geom_bar(position = "dodge", stat='identity') +
    xlab("Beleid") +
    ylab("Aantal") +
    ggtitle("Persberichten per beleid") +
    geom_text(aes(label=Persberichten),
              position=position_dodge(0.9), vjust=0) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_fill_manual(values=colors)
  )
    
  return(berichten.barplot.maand)
  
}