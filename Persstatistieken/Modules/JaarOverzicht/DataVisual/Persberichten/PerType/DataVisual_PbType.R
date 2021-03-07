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

# SERVER =======================================================================
DataVisual.PbType <- function(input, output, session, data, colours, beleid, datadeelbeleid) {

# Data ------------------------------------------------------------------------ 
df.berichten <- reactive({
    # Create tabel
    berichten <- data.frame(table(data()$Beleid, data()$Soort))
    colnames(berichten) <- c("Beleid", "Type", "Persberichten")
    # Add missing "Type"
    for(i in c("Activiteitenkalender", "Agendatip", "Evenementenkalender", "Persagenda", "Persbericht", "Persuitnodiging")) {
      if(!(i %in% levels(as.factor(berichten$Type)))) {
        temp <- data.frame(
          Beleid = c(levels(as.factor(datadeelbeleid()$Beleid))),
          Type = i,
          Persberichten = 0
        )
        berichten <- rbind(berichten, temp)
      }
    }
    # Add percentages
    berichten <- data.frame(berichten[order(berichten$Beleid),], "Procentueel" = calc_percentages("type", berichten))
    # Return
    return(berichten)
})

# Plot -------------------------------------------------------------------------
berichten.plot <- reactive({
      plots <- list("Aantal" = NA, "Procent" = NA)
      for (inhoud in c("Aantal", "Procent")) {
    # Barplot -----------------------------------------------------------------
        if ((inhoud == "Aantal" && input$type.aantal == "Barplot") || (inhoud == "Procent" && input$type.procent == "Barplot")) {
          plots[[inhoud]] <- simple_barplot(Id = 'type',
                                       data = df.berichten,
                                       Xaxis = "Beleid",
                                       Fill = "Type",
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
          plots[[inhoud]] <- simple_piechart(Id = 'type',
                                             data = df.berichten,
                                             Fill = "Type",
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

# Create cleaner table for display ---------------------------------------------
table.berichten <- reactive({
  berichten <- split(df.berichten(), df.berichten()$Type)
  berichten <- data.frame(
    Beleid = levels(df.berichten()$Beleid),
    Agendatip = berichten$Agendatip$Persberichten, 
    Evenementenkalender = berichten$Evenementenkalender$Persberichten,
    Persagenda = berichten$Persagenda$Persberichten,
    Persbericht = berichten$Persbericht$Persberichten,
    Persuitnodiging = berichten$Persuitnodiging$Persberichten
  )
  return(berichten)
})

  # Return --------------------------------------------------------------------
return(list(plot.aantal = reactive(berichten.plot()$Aantal), 
            plot.procent = reactive(berichten.plot()$Procent), 
            tabel = reactive(table.berichten()), 
            uitleg = reactive(input$uitleg)))

}





