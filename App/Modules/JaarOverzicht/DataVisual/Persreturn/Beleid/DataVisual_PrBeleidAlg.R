###############################################################################
# MODULE: Data visualisation (plot and table creation)
###############################################################################

# LOAD PACKAGES ===============================================================
library(scales)
library(janitor)
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

# SERVER =======================================================================
DataVisual.PrBeleidAlg <- function(input, output, session, data, colours) {

df.berichten <- reactive({
# Tabel ------------------------------------------------------------------------ 
  # Create table
  berichten <- data.frame(table(data()$Beleid, data()$Persreturn))
  colnames(berichten) <- c("Beleid", "Persreturn", "Aantal")
  # Add percentages
  berichten <- data.frame(berichten[order(berichten$Beleid),], "Procentueel" = calc_percentages("return.beleid.alg", berichten))
  return(berichten)
})
# Plot ----------------------------------------------------------------------
berichten.plot <- reactive({
      plots <- list("Aantal" = NA, "Procent" = NA)
      for (inhoud in c("Aantal", "Procent")) {
    # Barplot -----------------------------------------------------------------
        if ((inhoud == "Aantal" && input$type.aantal == "Barplot") || (inhoud == "Procent" && input$type.procent == "Barplot")) {
          plots[[inhoud]] <- simple_barplot(Id = 'return.beleid.alg',
                                       data = df.berichten,
                                       Xaxis = "Beleid",
                                       Fill = "Persreturn",
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
          plots[[inhoud]] <- simple_piechart(Id = 'return.beleid.alg',
                                             data = df.berichten,
                                             Fill = "Persreturn",
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

# Tabel overzichtelijk maken ---------------------------------------------------
berichten.tabel <- reactive({
  berichten <- split(df.berichten(), df.berichten()$Persreturn)
  berichten <- data.frame(
    Beleid = levels(df.berichten()$Beleid),
    Ja = berichten$Ja$Aantal,
    'Ja%' = berichten$Ja$Procent,
    Nee = berichten$Nee$Aantal,
    'Nee%' = berichten$Nee$Procent
  )
  colnames(berichten) <- c("Beleid", "Persreturn", '%', "Geen persreturn", '%')
  
  return(berichten)
})

# Add plots/data to corresponding Global collector -----------------------------
ToAppend <- list(
  algemeen.plot.aantal = reactive(berichten.plot()$Aantal),
  algemeen.plot.procent = reactive(berichten.plot()$Procent),
  algemeen.tabel =  reactive(berichten.tabel()), 
  algemeen.uitleg = reactive(input$uitleg))

Persreturn.beleid <<- append(Persreturn.beleid, ToAppend)

# Return -----------------------------------------------------------------------
return(list(plot.aantal = reactive(berichten.plot()$Aantal), 
            plot.procent = reactive(berichten.plot()$Procent), 
            tabel = reactive(berichten.tabel()), 
            uitleg = reactive(input$uitleg)))

}





