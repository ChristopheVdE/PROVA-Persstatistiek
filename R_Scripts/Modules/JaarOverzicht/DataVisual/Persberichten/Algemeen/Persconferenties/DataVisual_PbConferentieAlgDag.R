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
DataVisual.PbConferentieAlgDag <- function(input, output, session, data, colours) {

# Tabel ------------------------------------------------------------------------
df.berichten <- reactive({
  # Data preparation -----------------------------------------------------
  temp <- split(data(), data()$Persconferentie)
  temp <- temp$Ja
  for (i in colnames(temp)) {
    if (!(i %in% c("Kwartaal", "Datum PC", "Beleid", "Persconferentie"))) {
      temp[[i]] <- NULL
    }
  }
  
  # Dag -----------------------------------------------------------------
    # Dag Toevoegen
    temp$Dag <- factor(format(as.Date(temp$"Datum PC"), format = "%u"), levels = c(1:7))
    levels(temp$Dag) <- c("ma", "di", "wo", "do", "vr", "za", "zo")
    # Create table
    berichten <- data.frame(table(temp$Dag))
    colnames(berichten) <- c("Dag", "Persberichten")
    # Add percentages
    berichten <- data.frame(berichten, "Procentueel" = calc_percentages("conferentie.alg.dag", berichten))
    # Return df
    colnames(berichten) <- c("Dag", "Persconferenties", "Procentueel")
    return(berichten)
})

# Plot ----------------------------------------------------------------------
berichten.plot <- reactive({
      plots <- list("Aantal" = NA, "Procent" = NA)
      for (inhoud in c("Aantal", "Procent")) {
    # Barplot -----------------------------------------------------------------
        if ((inhoud == "Aantal" && input$type.aantal == "Barplot") || (inhoud == "Procent" && input$type.procent == "Barplot")) {
          plots[[inhoud]] <- simple_barplot(Id = 'conferentie.alg.dag',
                                       data = df.berichten,
                                       Xaxis = "Dag",
                                       Fill = "Dag",
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
          plots[[inhoud]] <- simple_piechart(Id = 'conferentie.alg.dag',
                                             data = df.berichten,
                                             Fill = "Dag",
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

# Totaal toevoegen aan tabel ---------------------------------------------------
berichten.tabel <- reactive({
  adorn_totals(df.berichten(),"row")
})

# Return -----------------------------------------------------------------------
return(list(plot.aantal = reactive(berichten.plot()$Aantal), 
            plot.procent = reactive(berichten.plot()$Procent), 
            tabel = reactive(berichten.tabel()), 
            uitleg = reactive(input$uitleg)))

}





