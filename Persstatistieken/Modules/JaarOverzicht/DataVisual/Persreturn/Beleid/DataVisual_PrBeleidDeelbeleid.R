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
DataVisual.PrDeelbeleid <- function(input, output, session, data, colours, beleid, datadeelbeleid) {

df.berichten <- reactive({
  # Tabel ------------------------------------------------------------------------ 
    # Aanmaak tabel berichten per deelbeleid
      berichten <- split(data(), data()$Beleid)
      berichten <- data.frame("Beleid" = beleid, table(berichten[[beleid]]$"Deelbeleid", berichten[[beleid]]$Persreturn))
      colnames(berichten) <- c("Beleid", "Deelbeleid","Persreturn", "Aantal")

    # Ontberkende deelbeleiden toevoegen aan tabel
      for (i in datadeelbeleid()) {
        if (!(i %in% levels(as.factor(berichten$Deelbeleid)))) {
          temp <- data.frame("Beleid"=beleid, "Deelbeleid"=i,"Persreturn"=c('Ja','Nee'), Aantal=0)
          berichten<- rbind(berichten, temp)
        }
      }

    # Add percentages
      berichten <- data.frame(berichten[order(berichten$Deelbeleid),], "Procentueel" = calc_percentages("return.beleid.beleid", berichten))
      return(berichten)
})
# Plot ----------------------------------------------------------------------
berichten.plot <- reactive({
      plots <- list("Aantal" = NA, "Procent" = NA)
      for (inhoud in c("Aantal", "Procent")) {
    # Barplot -----------------------------------------------------------------
        if ((inhoud == "Aantal" && input$type.aantal == "Barplot") || (inhoud == "Procent" && input$type.procent == "Barplot")) {
          plots[[inhoud]] <- simple_barplot(Id = 'return.beleid.beleid',
                                       data = df.berichten,
                                       Xaxis = "Deelbeleid",
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
          plots[[inhoud]] <- simple_piechart(Id = 'return.beleid.beleid',
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

# Clean table for display ------------------------------------------------------
berichten.tabel <- reactive({
  temp <- split.data.frame(df.berichten(), df.berichten()$Persreturn)
  berichten <- data.frame(
    Beleid = beleid,
    Deelbeleid = levels(as.factor(df.berichten()$Deelbeleid)),
    Ja = temp$Ja$Aantal,
    'Ja%' = temp$Ja$Procentueel,
    Nee = temp$Nee$Aantal,
    'Nee%' = temp$Nee$Procentueel
  )
  colnames(berichten) <- c("Beleid", "Deelbeleid", "Persreturn", '%',  "Geen persreturn", '%')
  return(berichten)
})


# Return -----------------------------------------------------------------------
return(list(plot.aantal = reactive(berichten.plot()$Aantal), 
            plot.procent = reactive(berichten.plot()$Procent), 
            tabel = reactive(berichten.tabel()), 
            uitleg = reactive(input$uitleg)))

}





