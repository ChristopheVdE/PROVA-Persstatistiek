###############################################################################
# MODULE: Data visualisation (plot and table creation)
###############################################################################

# UI ==========================================================================DataVisual.
DataVisual.PbVerzenderDeelbeleidlOutput <- function(id, plottitle, Xaxis, Xlabels, Legende = TRUE, Piechart = TRUE) {
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
        checkboxInput(ns("Xlabels"), label = "As labels (X-as)", value = TRUE),
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
DataVisual.PbVerzenderDeelbeleid <- function(input, output, session, data, colours, beleid, datadeelbeleid) {

df.berichten <- reactive({
  # Tabel ----------------------------------------------------------------------
    # Aanmaak tabel berichten per deelbeleid
      berichten <- split(data(), data()$Beleid)
      berichten <- data.frame("Beleid" = beleid, table(berichten[[beleid]]$"Verzender", berichten[[beleid]]$"Deelbeleid"))
      colnames(berichten) <- c("Beleid", "Verzender", "Deelbeleid","Persberichten")

    # Ontberkende deelbeleiden toevoegen aan tabel
      for (i in datadeelbeleid()) {
        if (!(i %in% levels(as.factor(berichten$Deelbeleid)))) {
          temp <- data.frame("Beleid"=beleid, "Verzender"=c('Provincie', 'Persdienst', 'Gouverneur', 'Extern'), "Deelbeleid"=i,"Persberichten"=0)
          berichten<- rbind(berichten, temp)
        }
      }

  # # # Percentages ----------------------------------------------------------------
  #     # Split dataframe on "Deelbeleid
  #     df.split <- split(berichten, berichten$Deelbeleid)
  #     # Create empty list
  #     column <- NULL
  #     # Calculate percentages
  #     for (deelbeleid in levels(as.factor(berichten$Deelbeleid))) {
  #       percentages <- NULL
  #       for (i in 1:length(df.split[[deelbeleid]]$Aantal)) {
  #         percentages <- c(percentages,
  #                          round(as.numeric(df.split[[deelbeleid]]$Aantal[[i]] / sum(df.split[[deelbeleid]]$Aantal) * 100), digits = 2))
  #       }
  #       column <- c(column, percentages)
  #     }
  #     # # Fix NA values
  #     # for(i in 1:length(column)) {
  #     #   if (is.na(column[[i]])) {
  #     #     column[[i]] <- 0
  #     #   }
  #     # }
  #     # Add percentages to table
  #     berichten <- data.frame(berichten[order(berichten$Deelbeleid),], 'Procentueel' = column)
      
      return(berichten)
})
# Plot -------------------------------------------------------------------------
berichten.plot <- reactive({
      plots <- list("Aantal" = NA, "Procent" = NA)
      for (inhoud in c("Aantal", "Procent")) {
        # Barplot -----------------------------------------------------------------
        if ((inhoud == "Aantal" && input$type.aantal == "Barplot") || (inhoud == "Procent" && input$type.procent == "Barplot")) {
          plots[[inhoud]] <- simple_barplot(Id = "bericht.verzender.beleid",
                                            data = df.berichten, 
                                            Xaxis = "Deelbeleid",
                                            Fill = "Verzender",
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
          plots[[inhoud]] <- simple_piechart(Id = "bericht.verzender.beleid",
                                             data = df.berichten,
                                             Fill = "Verzender",
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

# Tabel proper weergeven -------------------------------------------------------
berichten.tabel <- reactive ({
  berichten <- split.data.frame(df.berichten(), df.berichten()$Verzender)
  berichten <- data.frame(
    Beleid = beleid,
    Deelbeleid = levels(as.factor(df.berichten()$Deelbeleid)),
    Provincie = berichten$Provincie$Persberichten,
    Persdienst = berichten$Persdienst$Persberichten,
    Gouverneur = berichten$Gouverneur$Persberichten,
    Extern = berichten$Extern$Persberichten
  )
  adorn_totals(berichten, 'row')
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

Persberichten.verzender.beleid <<- append(Persberichten.verzender.beleid, ToAppend)


# Return -----------------------------------------------------------------------
return(list(plot.aantal = reactive(berichten.plot()$Aantal), 
            plot.procent = reactive(berichten.plot()$Procent), 
            tabel = reactive(berichten.tabel()), 
            uitleg = reactive(input$uitleg)))

}





