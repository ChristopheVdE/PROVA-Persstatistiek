library(shiny)
library(ggplot2)
library(RColorBrewer)

# SERVER function ---------------------------------------------
Persberichten.beleid.barplot <- function(dataframe, plottitle, type) {
  # Define color pallete --------------------------------------
  colors <- c(brewer.pal(8,"Pastel2"), brewer.pal(9, "Pastel1"))
  
  # Create Barplot --------------------------------------------
  persberichten.beleid.barplot <- reactive({
    if ("Beleid"==type()) {
      ggplot(data=dataframe(), aes(x=Beleid, y=Persberichten, fill=Beleid)) +
        geom_bar(position = "dodge", stat='identity') +
        xlab("Beleid") +
        ylab("Aantal") +
        ggtitle(plottitle()) +
        geom_text(aes(label=Persberichten),
                  position=position_dodge(0.9), vjust=0) +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        scale_fill_manual(values=colors)
    } else if ("Detail" == type()) {
      ggplot(data=dataframe(), aes(x=Detail, y=Persberichten, fill=Detail)) +
        geom_bar(position = "dodge", stat='identity') +
        xlab("Detail") +
        ylab("Aantal") +
        ggtitle(plottitle()) +
        geom_text(aes(label=Persberichten),
                  position=position_dodge(0.9), vjust=0) +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        scale_fill_manual(values=colors)
    }
  })
}