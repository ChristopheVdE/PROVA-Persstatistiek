###############################################################################
# MODULE: Data visualisation (plot and table creation)
###############################################################################

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
DataVisual.PbDeelbeleid <- function(input, output, session, data, colours, beleid, datadeelbeleid) {

df.berichten <- reactive({
  # Tabel ------------------------------------------------------------------------ 
    # Aanmaak tabel berichten per deelbeleid
      berichten <- split(data(), data()$Beleid)
      berichten <- data.frame("Beleid" = beleid, table(berichten[[beleid]]$"Deelbeleid"))
      colnames(berichten) <- c("Beleid", "Deelbeleid","Persberichten")

    # Ontberkende deelbeleiden toevoegen aan tabel
      for (i in datadeelbeleid()) {
        if (!(i %in% levels(as.factor(berichten$Deelbeleid)))) {
          temp <- data.frame("Beleid"=beleid, "Deelbeleid"=i,"Persberichten"=0)
          berichten<- rbind(berichten, temp)
        }
      }

    # Add percentages
      berichten <- data.frame(berichten, "Procentueel" = calc_percentages("beleid.beleid", berichten))
      
      return(berichten)
})
# Plot ----------------------------------------------------------------------
berichten.plot <- reactive({
      plots <- list("Aantal" = NA, "Procent" = NA)
      for (inhoud in c("Aantal", "Procent")) {
    # Barplot -----------------------------------------------------------------
        if ((inhoud == "Aantal" && input$type.aantal == "Barplot") || (inhoud == "Procent" && input$type.procent == "Barplot")) {
          plots[[inhoud]] <- simple_barplot(Id = 'beleid.beleid',
                                       data = df.berichten,
                                       Xaxis = "Deelbeleid",
                                       Fill = "Deelbeleid",
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
          plots[[inhoud]] <- simple_piechart(Id = 'beleid.beleid',
                                             data = df.berichten,
                                             Fill = "Deelbeleid",
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



# Add plots/data to corresponding Global collector -----------------------------
beleidconv <- tolower(gsub(' ', '', beleid))
if(beleidconv == 'onderwijseneducatie') {beleidconv <- 'onderwijs'}

ToAppend <- list(
  plot.aantal = reactive(berichten.plot()$Aantal),
  plot.procent = reactive(berichten.plot()$Procent),
  tabel =  reactive(berichten.tabel()), 
  uitleg = reactive(input$uitleg))
  
names(ToAppend) <- c(
  paste0(beleidconv, '.plot.aantal'),
  paste0(beleidconv, '.plot.procent'),
  paste0(beleidconv, '.tabel'),
  paste0(beleidconv, '.uitleg'))
  
Persberichten.beleid.beleid <<- append(Persberichten.beleid.beleid, ToAppend)

# Return -----------------------------------------------------------------------
return(list(plot.aantal = reactive(berichten.plot()$Aantal), 
            plot.procent = reactive(berichten.plot()$Procent), 
            tabel = reactive(berichten.tabel()), 
            uitleg = reactive(input$uitleg)))

}





