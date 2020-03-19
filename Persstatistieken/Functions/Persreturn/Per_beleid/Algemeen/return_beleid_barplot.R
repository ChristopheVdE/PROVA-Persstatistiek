persreturn.beleid.barplot <- function(data) {
  # Preparation --------------------------------------------------------
  df.return.beleid <- reactive({
    # Create dataframe for barplot ---------------------------------
    return.beleid <- data.frame(table(data()$Beleid,
                                      data()$Persreturn))
    # Rename columns -----------------------------------------------
    colnames(return.beleid) <- c("Beleid", "Persreturn", "Freq")
    return(return.beleid)
  })
  
  # Define color pallete --------------------------------------
  colors <- brewer.pal(9,"Set1")
  
  # Barplot ------------------------------------------------------------
  barplot <- reactive({
    ggplot(data=df.return.beleid(), aes(x=Beleid, y=Freq, fill=Persreturn)) +
      geom_bar(position = "dodge", stat='identity') +
      xlab("Detail Beleid") +
      ylab("Aantal") +
      ggtitle("Persreturn per beleid") +
      geom_text(aes(label=Freq),
                position=position_dodge(0.9), vjust=0) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_fill_manual(values=colors[2:1])
  })
  return(barplot)
}