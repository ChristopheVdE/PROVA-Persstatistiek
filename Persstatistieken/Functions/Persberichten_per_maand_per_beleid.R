library(shiny)
library(ggplot2)
library(RColorBrewer)


# Function: Persreturn per beleid ---------------------------------------------
Persberichten.beleid.maand <- function(dataframe, beleid) {
  barplot.berichten.Maand.totaal.per.Beleid <- reactive({
    # Specify color pallete
    colors <- c(brewer.pal(8,"Pastel2"), brewer.pal(9, "Pastel1"))
    # Create plot
    ggplot(data=dataframe()[[beleid()]], aes(x=Beleid, y=Freq, fill=Maand)) +
      geom_bar(position = "dodge", stat='identity') +
      xlab("Beleid") +
      ylab("Aantal") +
      ggtitle(paste0("Persberichten: Maand per Beleid (", paste0(beleid(), ")"))) +
      geom_text(aes(label=Freq),
                position=position_dodge(0.9), vjust=0) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 0, hjust = 0.5)) +
      scale_fill_manual(values=colors)
  })
}