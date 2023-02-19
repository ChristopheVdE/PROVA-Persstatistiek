###############################################################################
# MODULE: Data visualisation (plot and table creation)
###############################################################################

# LOAD PACKAGES ===============================================================
library(scales)
# =============================================================================

# LOAD MODULES & FUNCTIONS ====================================================
source("./Modules/JaarOverzicht/percentages.R")
source("./Modules/JaarOverzicht/Plots/simple_barplot.R")
source("./Modules/JaarOverzicht/Plots/simple_piechart.R")
# =============================================================================

# UI ==========================================================================
data.visualOutput <- function(id, plottitle, Xaxis, Xlabels, Legende = TRUE, Piechart = TRUE) {
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
        selectInput(ns("type.aantal"), label = "Plot type - Aantal", choices = (if(Piechart == TRUE) {c("Barplot", "Taartdiagram")} else {c("Barplot")}), selected = "Barplot"),
        selectInput(ns("type.procent"), label = "Plot type - Procentueel", choices = (if(Piechart == TRUE) {c("Barplot", "Taartdiagram")} else {c("Barplot")}), selected = "Barplot"),
        checkboxInput(ns("Xlabels"), label = "As labels (X-as)", value = Xlabels),
        checkboxInput(ns("legend"), label = "Legende", value = Legende)
      )
    ),
    fluidRow(
      tags$hr(),
      column(width = 12,
             textInput(ns("uitleg"), label ="Typ hier uitleg bij grafiek/ tabel:", value = "", placeholder = "Uitleg bij grafiek", width = "100%")
      )
    )
  )
}

# SERVER ======================================================================
DataVisual.PrMedium <- function(input, output, session, data, Xaxis, Fill, colours) {
  
  # Data preparation ---------------------------------------------------------- 
  df.berichten <- reactive({
      # Create table: persreturn algemeen
      Algemeen <- split(data(), data()$Persreturn)
      Algemeen <- Algemeen$Ja
      Algemeen <- data.frame(table(Algemeen$Beleid, Algemeen$Persreturn))
      colnames(Algemeen) <- c("Beleid", "Algemeen", "Freq")
      Algemeen$Algemeen <- "Algemeen"
      
      # Create table: TV
      TV <- split(data(), data()$TV)
      TV <- TV$Ja
      TV <- data.frame(table(TV$Beleid, TV$TV))
      colnames(TV) <- c("Beleid", "TV", "Freq")
      TV$TV <- "TV"
      
      # Create Table: Web
      Web <- split(data(), data()$"Alleen web")
      Web <- Web$Ja
      Web$Ja <- "Web"
      Web <- data.frame(table(Web$Beleid, Web$"Alleen web"))
      colnames(Web) <- c("Beleid", "Alleen web", "Freq")
      Web$"Alleen web" <- "Alleen web"
      
      # Merge dataframes
      berichten <- data.frame(Beleid = TV$Beleid,
                              Medium = c(Algemeen$Algemeen, TV$TV, Web$"Alleen web"),
                              Aantal = c(Algemeen$Freq, TV$Freq, Web$Freq))
      # Add percentages
      berichten <- data.frame(berichten[order(berichten$Beleid),], "Procentueel" = calc_percentages("return.medium", berichten))
      # Return
      return(berichten)
    }
  )
  
  # Plot ----------------------------------------------------------------------
  berichten.plot <- reactive({
    plots <- list("Aantal" = NA, "Procent" = NA)
    for (inhoud in c("Aantal", "Procent")) {
      # Barplot -----------------------------------------------------------------
      if ((inhoud == "Aantal" && input$type.aantal == "Barplot") || (inhoud == "Procent" && input$type.procent == "Barplot")) {
        plots[[inhoud]] <- simple_barplot(Id = "return.medium",
                                          data = df.berichten, 
                                          Xaxis = Xaxis,
                                          Fill = Fill,
                                          visual = inhoud, 
                                          title = input$title, 
                                          Xtitle = input$Xaxis, 
                                          Ytitle = input$Yaxis, 
                                          Xlabels = input$Xlabels, 
                                          legend = input$legend, 
                                          colors = colours)
      } 
      # Taartdiagram ------------------------------------------------------------
      else if ((inhoud == "Aantal" && input$type.aantal == "Taartdiagram") || (inhoud == "Procent" && input$type.procent == "Taartdiagram")) {
        plots[[inhoud]] <- simple_piechart(Id = "return.medium",
                                           data = df.berichten,
                                           Fill = Fill,
                                           visual = inhoud,
                                           title = input$title,
                                           Xtitle = input$Xaxis,
                                           Ytitle = input$Yaxis,
                                           Xlabels = input$Xlabels,
                                           legend = input$legend,
                                           colors = colours)
      }
    }
    return(plots)
  })
  # Table ---------------------------------------------------------------------
  tabel <- reactive({
    # Persreturn - medium -----------------------------------------------------
      temp <- split(df.berichten(), df.berichten()$Medium)
      temp <- data.frame(
        Beleid = levels(df.berichten()$Beleid),
        Algemeen = temp$Algemeen$Aantal,
        Web = temp$"Alleen web"$Aantal,
        TV = temp$TV$Aantal
      )
      return(temp)
    }
  )
  
# Add plots/data to corresponding Global collector -----------------------------
  ToAppend <- list(
    medium.plot.aantal = reactive(berichten.plot()$Aantal),
    medium.plot.procent = reactive(berichten.plot()$Procent),
    medium.tabel =  reactive(tabel()), 
    medium.uitleg = reactive(input$uitleg))
  
  Persreturn.medium <<- append(Persreturn.medium, ToAppend)
  
  

# Return -----------------------------------------------------------------------
  return(list(plot.aantal = reactive(berichten.plot()$Aantal), plot.procent = reactive(berichten.plot()$Procent), tabel = tabel, uitleg = reactive(input$uitleg)))
}




