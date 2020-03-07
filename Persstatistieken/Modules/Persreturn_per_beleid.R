library(shiny)
library(ggplot2)
library(RColorBrewer)


# UI function -------------------------------------------------
Persreturn.beleidOutput <- function(id, label = "barplot") {
  # Create namespace function using the provided id
  ns <- NS(id)
  
  tabBox(
    title = "test",
    width = 12,
    tabPanel("Barplot", plotOutput(ns("barplot"))),
    tabPanel("Tabel", tableOutput(ns("tabel")))
  )
}

# SERVER function ---------------------------------------------
Persreturn.beleid <- function(input, output, session, data, column, plottitle) {
  # Define color pallete --------------------------------------
  colors <- c(brewer.pal(8,"Pastel2"), brewer.pal(9, "Pastel1"))
  
  # Create Table ----------------------------------------------
  output$tabel <- renderTable({
    data()
  })
  
  # Create Barplot --------------------------------------------
  output$plot <- renderPlot({
    ggplot(data=dataframe()$column, aes(x=`Detail beleid`, y=`Aantal Persberichten`, fill=`Detail beleid`)) +
      geom_bar(position = "dodge", stat='identity') +
      xlab("Detail beleid") +
      ylab("Aantal") +
      ggtitle(plottitle) +
      geom_text(aes(label=`Aantal Persberichten`),
                position=position_dodge(0.9), vjust=0) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_fill_manual(values=colors)
  })
}