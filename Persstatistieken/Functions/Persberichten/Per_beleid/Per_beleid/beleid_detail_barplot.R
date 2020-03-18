bericht.beleid.beleid.barplot <- function(data, beleid) {

  # Preparation ---------------------------------------------------------
  df.persberichten.beleid.detail <- reactive({
    Persberichten <- split(data(), data()$Beleid)
    Persberichten <- data.frame(table(Persberichten[[beleid()]]$"Detail beleid"))
    colnames(Persberichten) <- c("Detail","Persberichten")
    return(Persberichten)
  })
  
  # Define color pallete --------------------------------------
  colors <- c(brewer.pal(8,"Pastel2"), brewer.pal(9, "Pastel1"))
  
  # Create barplot --------------------------------------
  persberichten.beleid.barplot <- reactive({
    ggplot(data=df.persberichten.beleid.detail(), aes(x=Detail, y=Persberichten, fill=Detail)) +
      geom_bar(position = "dodge", stat='identity') +
      xlab("Detail") +
      ylab("Aantal") +
      ggtitle(paste("Persberichten: ", beleid())) +
      geom_text(aes(label=Persberichten),
                position=position_dodge(0.9), vjust=0) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_fill_manual(values=colors)
  })
  
  return(persberichten.beleid.barplot)
  
}