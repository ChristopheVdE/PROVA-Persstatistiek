persreturn.deelbeleid.barplot <- function(data, beleid) {
  # Preparation --------------------------------------------------------
  df.Persreturn.beleid.detail <- reactive({
    Persreturn <- split(data(), data()$Beleid)
    
    for (i in levels(data()$Beleid)) ({
      Persreturn[[i]] <- data.frame(table(Persreturn[[i]]$"Detail beleid", Persreturn[[i]]$Persreturn))
      Persreturn[[i]] <- cbind(Beleid = i, Persreturn[[i]])
      colnames(Persreturn[[i]]) <- c("Beleid","Detail", "Persreturn", "Freq")
      
      # Fix missing freq
      temp <- Persreturn[[i]]
      temp$Persreturn <- as.factor(temp$Persreturn)
      if(!("Ja" %in% levels(temp$Persreturn))) {
        df.Ja <- Persreturn[[i]]
        df.Ja$Persreturn <- "Ja"
        df.Ja$Freq <- 0
        Persreturn[[i]] <- rbind(df.Ja, Persreturn[[i]])
      } else if(!("Nee" %in% levels(temp$Persreturn))) {
        df.Nee <- Persreturn[[i]]
        df.Nee$Persreturn <- "Nee"
        df.Nee$Freq <- 0
        Persreturn[[i]] <- rbind(df.Nee, Persreturn[[i]])
      }
    })
    return(Persreturn[[beleid()]])
  })
  
  # Define color pallete --------------------------------------
  colors <- brewer.pal(9,"Set1")
  
  # Create Barplot --------------------------------------------
  persreturn.deelbeleid.barplot <- reactive({
      ggplot(data=df.Persreturn.beleid.detail(), aes(x=Detail, y=Freq, fill=Persreturn)) +
        geom_bar(position = "dodge", stat='identity') +
        xlab("Detail beleid") +
        ylab("Aantal") +
        ggtitle(paste0("Persreturn: ", beleid())) +
        geom_text(aes(label=Freq),
                  position=position_dodge(0.9), vjust=0) +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        scale_fill_manual(values=colors[2:1])
  })
  return(persreturn.deelbeleid.barplot)
}