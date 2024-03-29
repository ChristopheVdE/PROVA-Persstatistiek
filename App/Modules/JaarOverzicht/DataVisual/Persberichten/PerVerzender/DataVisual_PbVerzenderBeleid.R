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
DataVisual.PbVerzenderBeleid <- function(input, output, session, data, colours, verzender, datadeelbeleid) {

# Tabel ------------------------------------------------------------------------ 
df.berichten <- reactive({
    # Create table
    berichten <- data.frame(table(data()$Beleid, data()$Verzender))
    colnames(berichten) <- c("Beleid", "Verzender", "Persberichten")
    # Add missing "Verzender"
    for(i in c("Persdienst", "Provincie", "Gouverneur", "Extern")) {
      if(!(i %in% levels(berichten$Verzender))) {
        temp <- data.frame(
          Beleid = levels(as.factor(datadeelbeleid$Beleid)),
          Verzender = i,
          Persberichten = 0
        )
        berichten <- rbind(berichten, temp)
      }
    }
    # Add percentages
    berichten <- data.frame(berichten, "Procentueel" = calc_percentages("verzender.alg.beleid", berichten))
    # Return
    return(berichten)
})



# Plot ----------------------------------------------------------------------
berichten.plot <- reactive({
      plots <- list("Aantal" = NA, "Procent" = NA)
      for (inhoud in c("Aantal", "Procent")) {
    # Barplot -----------------------------------------------------------------
        if ((inhoud == "Aantal" && input$type.aantal == "Barplot") || (inhoud == "Procent" && input$type.procent == "Barplot")) {
          plots[[inhoud]] <- simple_barplot(Id = 'verzender.alg.beleid',
                                       data = df.berichten,
                                       Xaxis = "Verzender",
                                       Fill = "Beleid",
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
          plots[[inhoud]] <- simple_piechart(Id = 'verzender.alg.beleid',
                                             data = df.berichten,
                                             Fill = "Beleid",
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

# Create clean tabel for display -----------------------------------------------
berichten.tabel <- reactive({
  # Reformat table -------------------------------------------------------------

  berichten <- split(df.berichten(), df.berichten()$Verzender)
  berichten <- data.frame(Beleid = levels(df.berichten()$Beleid),
                          Persdienst = berichten$Persdienst$Persberichten,
                          Provincie = berichten$Provincie$Persberichten,
                          Gouverneur = berichten$Gouverneur$Persberichten,
                          Extern = berichten$Extern$Persberichten)
  # Totaal toevoegen aan tabel -------------------------------------------------
  adorn_totals(df.berichten(),"row")
  
  return(berichten)
})

# Add plots/data to corresponding Global collector -----------------------------
ToAppend <- list(
  beleid.plot.aantal = reactive(berichten.plot()$Aantal),
  beleid.plot.procent = reactive(berichten.plot()$Procent),
  beleid.tabel =  reactive(berichten.tabel()), 
  beleid.uitleg = reactive(input$uitleg))

Persberichten.verzender.alg <<- append(Persberichten.verzender.alg, ToAppend)

# Return --------------------------------------------------------------------
return(list(plot.aantal = reactive(berichten.plot()$Aantal), 
            plot.procent = reactive(berichten.plot()$Procent), 
            tabel = reactive(berichten.tabel()), 
            uitleg = reactive(input$uitleg)))

}





