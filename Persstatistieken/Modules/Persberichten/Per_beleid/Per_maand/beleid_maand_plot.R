###############################################################################
# MODULE: Persberichten - Algemeen: Beleid per maand
###############################################################################

library(shiny)
library(ggplot2)
library(RColorBrewer)
library(scales)

# UI ==========================================================================
bericht.beleid.maandOutput <- function(id, plottitle) {
  ns <- NS(id)
  tabPanel(
    "Opties",
    fluidRow(
      column(
        width = 6,
        textInput(ns("title"), label = "Plot title", value = plottitle, placeholder = "Plot titel"),
        textInput(ns("Xaxis"), label = "X-as naam", value = "Maand", placeholder = "Maand"),
        textInput(ns("Yaxis"), label = "Y-as naam", value = "Aantal", placeholder = "Aantal")
      ),
      column(
        width = 6,
        selectInput(ns("type"), label = "Plot type", choices = c("Barplot", "Taartdiagram"), selected = "Barplot"),
        selectInput(ns("inhoud"), label = "Plot type", choices = c("Aantal", "Procentueel"), selected = "Aantal"),
        checkboxInput(ns("Xlabels"), label = "As labels (X-as)", value = TRUE),
        checkboxInput(ns("legend"), label = "Legende", value = TRUE)
      )
    )
  )
}

# SERVER ======================================================================
bericht.beleid.maand <- function(input, output, session, data, beleid) {

  # Data preparation ---------------------------------------------------------- 
  df.berichten.Maand.totaal.per.Beleid <-  reactive({
    
    # Create basic dataframe --------------------------------------------------
    berichten <- data.frame(table(data()$Beleid, data()$Maand))
    colnames(berichten) <- c("Beleid", "Maand", "Persberichten")
    berichten$Maand <- factor(berichten$Maand, levels = c("jan", "feb", "mrt", "apr", "mei", "jun", "jul", "aug", "sep", "okt", "nov", "dec", "Totaal"))
    berichten <- split(berichten, berichten$Beleid)
    berichten <- berichten[[beleid]]
    
    # Calculate percentages ---------------------------------------------------
    source("D:/Documenten/GitHub/Persstatistiek/Persstatistieken/Modules/Functies/percentages.R")
    berichten <- data.frame(berichten, 
                            "Procentueel" = calc_percentages(berichten))
  })
  
  # Define color pallete ------------------------------------------------------
  colors <- c(brewer.pal(8,"Pastel2"), brewer.pal(9, "Pastel1"))
  
  # Plot (beleid per maand) ---------------------------------------------------
  berichten.plot.beleid <- reactive(
    # Barplot -----------------------------------------------------------------
    if (input$type == "Barplot") {
      source("D:/Documenten/GitHub/Persstatistiek/Persstatistieken/Modules/Functies/simple_barplot.R")
      simple_barplot(data = df.berichten.Maand.totaal.per.Beleid, 
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
      simple_piechart(df.berichten.Maand.totaal.per.Beleid, 
                      input$inhoud, 
                      input$title, 
                      input$Xaxis, 
                      input$Yaxis, 
                      input$Xlabels, 
                      input$legend, 
                      colors)
    }
  )

  # Table ---------------------------------------------------------------------
  tabel <- reactive(
    rbind(df.berichten.Maand.totaal.per.Beleid(),
          c(beleid,
           "Totaal",
           sum(df.berichten.Maand.totaal.per.Beleid()$Persberichten),
           100
          )
        )
  )
  
  return(list(plot = berichten.plot.beleid, tabel = tabel))
  }




