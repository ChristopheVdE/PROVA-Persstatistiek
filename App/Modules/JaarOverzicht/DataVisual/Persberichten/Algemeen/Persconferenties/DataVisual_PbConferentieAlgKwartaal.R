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
DataVisual.PbConferentieAlgKwartaal <- function(input, output, session, data, colours, beleid) {

df.berichten <- reactive({
# Tabel ------------------------------------------------------------------------ 
  # Data preparation -----------------------------------------------------
    temp <- split(data(), data()$Persconferentie)
    temp <- temp$Ja
    for (i in colnames(temp)) {
      if (!(i %in% c("Kwartaal", "Datum PC", "Beleid", "Persconferentie"))) {
        temp[[i]] <- NULL
      }
    }
  # Kwartaal ------------------------------------------------------------
    # Create dummy dataframe
    berichten <- data.frame(
      Kwartaal = factor(c("Q1", "Q2", "Q3", "Q4"), c("Q1", "Q2", "Q3", "Q4")),
      Persberichten = 0
    )
    # Create table
    temp <- data.frame(table(temp$Kwartaal))
    colnames(temp) <- c("Kwartaal", "Persberichten")
    # Update values of dummy dataframe
    for (i in temp$Kwartaal) {
      berichten$Persberichten[grepl(i, berichten$Kwartaal)] <- temp$Persberichten[grepl(i, temp$Kwartaal)]
    }
    # Add percentages
    berichten <- data.frame(berichten, "Procentueel" = calc_percentages("conferentie.alg.kwartaal", berichten))
    # Return df
    colnames(berichten) <- c("Kwartaal", "Persconferenties", "Procentueel")
    return(berichten)
})
# Plot ----------------------------------------------------------------------
berichten.plot <- reactive({
      plots <- list("Aantal" = NA, "Procent" = NA)
      for (inhoud in c("Aantal", "Procent")) {
    # Barplot -----------------------------------------------------------------
        if ((inhoud == "Aantal" && input$type.aantal == "Barplot") || (inhoud == "Procent" && input$type.procent == "Barplot")) {
          plots[[inhoud]] <- simple_barplot(Id = 'conferentie.alg.kwartaal',
                                       data = df.berichten,
                                       Xaxis = "Kwartaal",
                                       Fill = "Kwartaal",
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
          plots[[inhoud]] <- simple_piechart(Id = 'conferentie.alg.kwartaal',
                                             data = df.berichten,
                                             Fill = "KWartaal",
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
ToAppend <- list(
  kwartaal.plot.aantal = reactive(berichten.plot()$Aantal),
  kwartaal.plot.procent = reactive(berichten.plot()$Procent),
  kwartaal.tabel =  reactive(berichten.tabel()), 
  kwartaal.uitleg = reactive(input$uitleg))

Persconferenties.alg <<- append(Persconferenties.alg, ToAppend)


# Return -----------------------------------------------------------------------
return(list(plot.aantal = reactive(berichten.plot()$Aantal), 
            plot.procent = reactive(berichten.plot()$Procent), 
            tabel = reactive(berichten.tabel()), 
            uitleg = reactive(input$uitleg)))

}





