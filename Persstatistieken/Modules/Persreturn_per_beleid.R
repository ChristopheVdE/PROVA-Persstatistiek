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
Persreturn.beleid <- function(input, output, session, dataframe, plottitle, type) {
  # Define color pallete --------------------------------------
  colors <- c(brewer.pal(8,"Pastel2"), brewer.pal(9, "Pastel1"))
  
  # Create Table ----------------------------------------------
  output$tabel <- renderTable({
    dataframe()
  })
  
  # Create Barplot --------------------------------------------
  output$barplot <- renderPlot({
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
  })
  
  
  
  
  # 
  # reactive({
  #   # Barplot "Beleid" ----------------------------------------
  #   if (grep("Beleid", type()) {
  #     output$barplot <- renderPlot({
  #         ggplot(data=dataframe(), aes(x=Beleid, y=`Aantal Persberichten`, fill=Beleid)) +
  #           geom_bar(position = "dodge", stat='identity') +
  #           xlab("Beleid") +
  #           ylab("Aantal") +
  #           ggtitle(c("Persreturn per beleid:")) +
  #           geom_text(aes(label=`Aantal Persberichten`),
  #                     position=position_dodge(0.9), vjust=0) +
  #           theme_bw() +
  #           theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  #           scale_fill_manual(values=colors)
  #       })
  #     })
  #   # Barplot "Detail beleid" ---------------------------------
  #   } else {
  #     output$barplot <- renderPlot({
  #       ggplot(data=dataframe(), aes(x=`Detail beleid`, y=`Aantal Persberichten`, fill=`Detail beleid`)) +
  #         geom_bar(position = "dodge", stat='identity') +
  #         xlab("Detail beleid") +
  #         ylab("Aantal") +
  #         ggtitle(plottitle()) +
  #         geom_text(aes(label=`Aantal Persberichten`),
  #                   position=position_dodge(0.9), vjust=0) +
  #         theme_bw() +
  #         theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  #         scale_fill_manual(values=colors)
  #     })
  #   }
  # })
}