###############################################################################
# MODULE: Data visualisation (plot and table creation)
###############################################################################

library(shiny)
library(ggplot2)
library(RColorBrewer)
library(scales)

# UI ==========================================================================
data.visualOutput <- function(id, plottitle, Xaxis) {
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
data.visual <- function(input, output, session, Id, data, Xaxis, Fill, beleid = NULL) {

  # Data preparation ---------------------------------------------------------- 
    df.berichten <- reactive(
    # Algemeen Kwartaal -------------------------------------------------------
      if (Id == "alg.kwartaal") {
      # Create dummy dataframe
        berichten <- data.frame(
          Kwartaal = factor(c("Q1", "Q2", "Q3", "Q4"), c("Q1", "Q2", "Q3", "Q4", "Totaal")),
          Persberichten = 0
        )
      # Create table
        temp <- data.frame(table(data()$Kwartaal))
        colnames(temp) <- c("Kwartaal", "Persberichten")
      # Update values of dummy dataframe
        for (i in temp$Kwartaal) {
          berichten$Persberichten[grepl(i, berichten$Kwartaal)] <- temp$Persberichten[grepl(i, temp$Kwartaal)]
        }
      # Return df
        berichten
      }
    # Algemeen Maand ------------------------------------------------------------
      else if (Id == "alg.maand") {
      # Create table
        berichten <- data.frame(table(data()$Maand))
        colnames(berichten) <- c("Maand", "Persberichten")
      # Fix month names (and sorting in tables)
        for (i in 0:length(levels(berichten$Maand))) {
          levels(berichten$Maand)[i] <- month.abb[i]
        }
      # Add "Totaal to levels"
        berichten$Maand <- factor(berichten$Maand, levels = c(month.abb, "Totaal"))
      # Return df
        berichten
      } 
    # Algemeen Beleid -----------------------------------------------------------
      else if (Id == "alg.beleid") {
      # Create table
        berichten <- data.frame(table(data()$Beleid))
        colnames(berichten) <- c("Beleid","Persberichten")
      # Return df
        berichten
      } 
    # Per beleid: Maand ---------------------------------------------------------
      else if (Id == "beleid.maand") {
        berichten <- data.frame(table(data()$Beleid, data()$Maand))
        colnames(berichten) <- c("Beleid", "Maand", "Persberichten")
        for (i in 0:length(levels(berichten$Maand))) {
          levels(berichten$Maand)[i] <- month.abb[i]
        }
        berichten$Maand <- factor(berichten$Maand, levels = c(month.abb, "Totaal"))
        berichten <- split(berichten, berichten$Beleid)
        berichten <- berichten[[beleid]]
      }
      # Per beleid: Deelbeleid ----------------------------------------------------
      else if (Id == "beleid.beleid") {
        berichten <- split(data(), data()$Beleid)
        berichten <- data.frame("Beleid" = beleid, table(berichten[[beleid]]$"Deelbeleid"))
        colnames(berichten) <- c("Beleid", "Deelbeleid","Persberichten")
        levels(berichten$Deelbeleid) <- c(levels(berichten$Deelbeleid), "Totaal")
      } 
      # Verzender Algemeen - Verzender --------------------------------------------
      else if (Id == "verzender.alg.verzender") {
        berichten <- data.frame(table(data()$Verzender))
        colnames(berichten) <- c("Verzender", "Persberichten")
        for(i in c("Persdienst", "Provincie", "Gouverneur", "Extern")) {
          if(!(i %in% levels(berichten$Verzender))) {
            temp <- data.frame(
              Verzender = i,
              Persberichten = 0
            )
            berichten <- rbind(berichten, temp)
          }
        }
      } 
      # Verzender Algemeen - Beleid -----------------------------------------------
      else if (Id == "verzender.alg.beleid") {
        berichten <- data.frame(table(data()$Beleid, data()$Verzender))
        colnames(berichten) <- c("Beleid", "Verzender", "Persberichten")
        for(i in c("Persdienst", "Provincie", "Gouverneur", "Extern")) {
          if(!(i %in% levels(berichten$Verzender))) {
            temp <- data.frame(
              Beleid = c("Economie", "Gouverneur", "Leefmilieu", "Mobiliteit", "Onderwijs en Educatie", "Provinciebestuur", "Ruimte", "Vrije Tijd"),
              Verzender = i,
              Persberichten = 0
            )
            berichten <- rbind(berichten, temp)
          }
        }
      }
      # Verzender: Maand ----------------------------------------------------------
      else if (Id == "verzender.maand") {
        berichten <- data.frame(table(data()$Verzender, data()$Maand))
        colnames(berichten) <- c("Verzender", "Maand", "Persberichten")
        for(i in c("Persdienst", "Provincie", "Gouverneur", "Extern")) {
          if(!(i %in% levels(berichten$Verzender))) {
            temp <- data.frame(
              Maand = levels(berichten$Maand),
              Verzender = i,
              Persbreichten = 0
            )
            berichten <- rbind(berichten, temp)
          }
        }
        berichten <- split(berichten, berichten$Verzender)
        berichten <- berichten[[verzender()]]
      } 
      # Type ----------------------------------------------------------------------
      else if (Id == "type") {
        berichten <- data.frame(table(data()$Beleid, data()$Soort))
        colnames(berichten) <- c("Beleid", "Type", "Persbereichten")
        berichten$Type <- as.factor(berichten$Type)
        for(i in c("Activiteitenkalender", "Agendatip", "Evenementenkalender", "Persagenda", "Persbericht", "Persuitnodiging")) {
          if(!(i %in% levels(berichten$Type))) {
            temp <- data.frame(
              Beleid = c("Economie", "Gouverneur", "Leefmilieu", "Mobiliteit", "Onderwijs en Educatie", "Provinciebestuur", "Ruimte", "Vrije Tijd"),
              Type = i,
              Persberichten = 0
            )
            berichten <- rbind(berichten, temp)
          }
        }
      }
      

    # # Calculate percentages ---------------------------------------------------
    # source("D:/Documenten/GitHub/Persstatistiek/Persstatistieken/Modules/Functies/percentages.R")
    # berichten <- data.frame(berichten,
    #                         "Procentueel" = calc_percentages(berichten))
  )
  
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
            if (is.null(beleid)) {
              df.berichten()
              # rbind(df.berichten(), c(beleid, "Totaal", sum(df.berichten()$Persberichten), 100))
            } else {
              rbind(df.berichten(), c(beleid, "Totaal", sum(df.berichten()$Persberichten), 100))
            }
           )
  
  return(list(plot = berichten.plot, tabel = tabel))
  }




