library(shiny)
library(ggplot2)
library(RColorBrewer)


# Function: Persreturn per beleid ---------------------------------------------
Persreturn.beleid.barplot <- function(dataframe, plottitle, type) {
  # Define color pallete --------------------------------------
  colors <- brewer.pal(9,"Set1")
  
  # Create Barplot --------------------------------------------
  persreturn.beleid.barplot <- reactive({
    if ("Beleid"==type()) {
      ggplot(data=dataframe(), aes(x=Beleid, y=Freq, fill=Persreturn)) +
        geom_bar(position = "dodge", stat='identity') +
        xlab("Detail Beleid") +
        ylab("Aantal") +
        ggtitle(plottitle()) +
        geom_text(aes(label=Freq),
                  position=position_dodge(0.9), vjust=0) +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        scale_fill_manual(values=colors[2:1])
    } else if ("Detail" == type()) {
      ggplot(data=dataframe(), aes(x=Detail, y=Freq, fill=Persreturn)) +
        geom_bar(position = "dodge", stat='identity') +
        xlab("Detail beleid") +
        ylab("Aantal") +
        ggtitle(plottitle()) +
        geom_text(aes(label=Freq),
                  position=position_dodge(0.9), vjust=0) +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        scale_fill_manual(values=colors[2:1])
    }
  })
}