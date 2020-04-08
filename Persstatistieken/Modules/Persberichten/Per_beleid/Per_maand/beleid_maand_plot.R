###############################################################################
# MODULE: Persberichten - Algemeen: Beleid per maand
###############################################################################

library(shiny)
library(ggplot2)
library(RColorBrewer)
library(scales)

# UI ==========================================================================
bericht.beleidOutput <- function(id, plottitle, Xaxis) {
  ns <- NS(id)
  tabPanel(
    "Opties",
    fluidRow(
      column(
        width = 6,
        textInput(ns("title"), label = "Plot title", value = plottitle, placeholder = "Plot titel"),
        textInput(ns("Xaxis"), label = "X-as naam", value = Xaxis, placeholder = Xaxis),
        textInput(ns("Yaxis"), label = "Y-as naam", value = "Aantal", placeholder = "Aantal")
      ),
      column(
        width = 6,
        selectInput(ns("type"), label = "Plot type", choices = c("Barplot", "Taartdiagram"), selected = "Barplot"),
        selectInput(ns("inhoud"), label = "Plot type", choices = c("Aantal", "Procentueel"), selected = "Aantal"),
        checkboxInput(ns("Xlabels"), label = "As labels (X-as)", value = FALSE),
        checkboxInput(ns("legend"), label = "Legende", value = TRUE)
      )
    )
  )
}

# SERVER ======================================================================
bericht.beleid <- function(input, output, session, Id, data, Xaxis, Fill, beleid = NULL) {

  # Data preparation ---------------------------------------------------------- 
  df.berichten <-  reactive({
    
    # Create basic dataframe --------------------------------------------------
    source("D:/Documenten/GitHub/Persstatistiek/Persstatistieken/Modules/Functies/dataframe_prep.R")
    berichten <- df.prep(Id, data, Xaxis, beleid)

    # Calculate percentages ---------------------------------------------------
    source("D:/Documenten/GitHub/Persstatistiek/Persstatistieken/Modules/Functies/percentages.R")
    berichten <- data.frame(berichten, 
                            "Procentueel" = calc_percentages(berichten))
  })
  
  # Define color pallete ------------------------------------------------------
  colors <- c(brewer.pal(8,"Pastel2"), brewer.pal(9, "Pastel1"))
  
  # Plot (beleid per maand) ---------------------------------------------------
  berichten.plot <- reactive(
    # Barplot -----------------------------------------------------------------
    if (input$type == "Barplot") {
      source("D:/Documenten/GitHub/Persstatistiek/Persstatistieken/Modules/Functies/simple_barplot.R")
      simple_barplot(data = df.berichten, 
                     Xaxis = Xaxis,
                     Fill = Fill,
                     visual = input$inhoud, 
                     title = input$title, 
                     Xtitle = input$Xaxis, 
                     Ytitle = input$Yaxis, 
                     Xlabels = input$Xlabels, 
                     legend = input$legend, 
                     colors = colors)
    } 
    # Piechart ----------------------------------------------------------------
    else {
      source("D:/Documenten/GitHub/Persstatistiek/Persstatistieken/Modules/Functies/simple_piechart.R")
      simple_piechart(data = df.berichten, 
                      Fill = Fill,
                      visual = input$inhoud, 
                      title = input$title, 
                      Xtitle = input$Xaxis, 
                      Ytitle = input$Yaxis, 
                      Xlabels = input$Xlabels, 
                      legend = input$legend, 
                      colors = colors)
    }
  )

  # Table ---------------------------------------------------------------------
  tabel <- reactive(
             rbind(df.berichten(), c(beleid, "Totaal", sum(df.berichten()$Persberichten), 100))
           )
  
  return(list(plot = berichten.plot, tabel = tabel))
  }




