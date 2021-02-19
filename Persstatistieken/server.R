###############################################################################
# SHINY APP (Persstatistiek): SERVER
###############################################################################

# PACKAGES =====================================================================
library(knitr)
library(RColorBrewer)
library(ggplot2)
library(ISOweek)
library(DT)
# ------------------------------------------------------------------------------
source("./Modules/BasisData/ophalen_basisdata.R")
source("./Modules/JaarOverzicht/data_preparation.R")
source("./Modules/JaarOverzicht/data_visualisation.R")
# ==============================================================================

server <- function(input, output, session) {
  
# INPUT PROCESSING =============================================================
  # Colors ---------------------------------------------------------------------
  colours <- reactive(c(input$colour1, 
                        input$colour2,
                        input$colour3,
                        input$colour4,
                        input$colour5,
                        input$colour6,
                        input$colour7,
                        input$colour8,
                        input$colour9,
                        input$colour10,
                        input$colour11,
                        input$colour12,
                        input$colour13,
                        input$colour14))
  return.colours <- reactive(c(input$return.colour1, 
                               input$return.colour2))
  # BASIC data -----------------------------------------------------------------
    # Inlezen ------------------------------------------------------------------
   
    AlleDeelbeleiden <- getbasisdata(file = reactive(input$file$datapath),
                                         sheet = reactive(input$basisSheet),
                                         datarange = reactive(input$rangeDeelbeleid)
      
    )
    # Update pickerInput -------------------------------------------------------
    observe({
      Deelbeleiden <- split.data.frame(AlleDeelbeleiden(), AlleDeelbeleiden()$Beleid)
      # Economie
      updatePickerInput(
         session = session,
         inputId = 'Economie.ActieveDeelbeleiden',
         choices = Deelbeleiden$Economie$Deelbeleid,
         selected = Deelbeleiden$Economie$Deelbeleid
      )
      # browser()
      # Gouverneur
      updatePickerInput(
        session = session,
        inputId = 'Gouverneur.ActieveDeelbeleiden',
        choices = Deelbeleiden$Gouverneur$Deelbeleid,
        selected = Deelbeleiden$Gouverneur$Deelbeleid
      )
      # Leefmilieu
      updatePickerInput(
        session = session,
        inputId = 'Leefmilieu.ActieveDeelbeleiden',
        choices = Deelbeleiden$Leefmilieu$Deelbeleid,
        selected = Deelbeleiden$Leefmilieu$Deelbeleid
      )
      # Mobiliteit
      updatePickerInput(
        session = session,
        inputId = 'Mobiliteit.ActieveDeelbeleiden',
        choices = Deelbeleiden$Mobiliteit$Deelbeleid,
        selected = Deelbeleiden$Mobiliteit$Deelbeleid
      )
      # Onderwijs en Educatie
      updatePickerInput(
        session = session,
        inputId = 'Onderwijs.ActieveDeelbeleiden',
        choices = Deelbeleiden$`Onderwijs en Educatie`$Deelbeleid,
        selected = Deelbeleiden$`Onderwijs en Educatie`$Deelbeleid
      )
      # Provinciebestuur
      updatePickerInput(
        session = session,
        inputId = 'Provinciebestuur.ActieveDeelbeleiden',
        choices = Deelbeleiden$Provinciebestuur$Deelbeleid,
        selected = Deelbeleiden$Provinciebestuur$Deelbeleid
      )
      # Ruimte
      updatePickerInput(
        session = session,
        inputId = 'Ruimte.ActieveDeelbeleiden',
        choices = Deelbeleiden$Ruimte$Deelbeleid,
        selected = Deelbeleiden$Ruimte$Deelbeleid
      )
      # Vrije Tijd
      updatePickerInput(
        session = session,
        inputId = 'VrijeTijd.ActieveDeelbeleiden',
        choices = Deelbeleiden$`Vrije Tijd`$Deelbeleid,
        selected = Deelbeleiden$`Vrije Tijd`$Deelbeleid
      )
    })
    # Render Table -------------------------------------------------------------
    output$AlleDeelbeleiden <- DT::renderDataTable({
      AlleDeelbeleiden()
    })
    
  # MAIN data: ALL -------------------------------------------------------------
    # Inlezen + Corrigeren
    PersstatistiekFull <- data.preparation(file = reactive(input$file$datapath),
                                       sheet = reactive(input$sheet),
                                       headers = reactive(input$headers),
                                       manual.beleid = reactive(input$col.beleid),
                                       manual.deelbeleid = reactive(input$col.detail),
                                       manual.verzender = reactive(input$col.verzender),
                                       manual.type = reactive(input$col.type),
                                       manual.return.alg = reactive(input$col.return.algemeen),
                                       manual.return.web = reactive(input$col.return.web),
                                       manual.return.tv = reactive(input$col.return.tv),
                                       manual.datum = reactive(input$col.datum),
                                       manual.persconferentie = reactive(input$col.persconferentie),
                                       alles = TRUE)
    # Render Table
    output$PersstatistiekFull <- DT::renderDataTable({
      PersstatistiekFull()
    })
  # MAIN data: 1 YEAR ----------------------------------------------------------
    # Inlezen + Corrigeren
    Persstatistiek <- data.preparation(file = reactive(input$file$datapath),
                                       sheet = reactive(input$sheet),
                                       headers = reactive(input$headers),
                                       manual.beleid = reactive(input$col.beleid),
                                       manual.deelbeleid = reactive(input$col.detail),
                                       manual.verzender = reactive(input$col.verzender),
                                       manual.type = reactive(input$col.type),
                                       manual.return.alg = reactive(input$col.return.algemeen),
                                       manual.return.web = reactive(input$col.return.web),
                                       manual.return.tv = reactive(input$col.return.tv),
                                       manual.datum = reactive(input$col.datum),
                                       manual.persconferentie = reactive(input$col.persconferentie),
                                       alles = FALSE,
                                       jaar = reactive(input$jaar),
                                       kwartaal = reactive(input$kwartaal))
    # Render Table
    output$Persstatistiek <- DT::renderDataTable({
      Persstatistiek()
    })
# ==============================================================================
  
# PERSBERICHTEN ================================================================
    # ALGEMEEN -----------------------------------------------------------------
      # Persberichten ----------------------------------------------------------
        # Per Kwartaal ---------------------------------------------------------
          persberichten.alg.kwartaal <- callModule(data.visual, "bericht.alg.kwartaal", Id = "alg.kwartaal", data = Persstatistiek, Xaxis = "Kwartaal", Fill = "Kwartaal", colours = colours)      
          # Plot - aantal
            output$persberichten.alg.kwartaal.plot.aantal <- renderPlot(
              persberichten.alg.kwartaal$plot.aantal()
            )
            # Plot - aantal
            output$persberichten.alg.kwartaal.plot.procent <- renderPlot(
              persberichten.alg.kwartaal$plot.procent()
            )
          # Tabel
            output$persberichten.alg.kwartaal.tabel <- DT::renderDataTable(
              persberichten.alg.kwartaal$tabel()
            )
        # Per Maand ------------------------------------------------------------
            persberichten.alg.maand <- callModule(data.visual, "bericht.alg.maand", Id = "alg.maand", data = Persstatistiek, Xaxis = "Maand", Fill = "Maand", colours = colours)
          # Plot - aantal
            output$persberichten.alg.maand.plot.aantal <- renderPlot(
              persberichten.alg.maand$plot.aantal()
            )
            # Plot - procent
            output$persberichten.alg.maand.plot.procent <- renderPlot(
              persberichten.alg.maand$plot.procent()
            )
          # Tabel
            output$persberichten.alg.maand.tabel <- DT::renderDataTable(
              persberichten.alg.maand$tabel()
            )
        # Per Dag --------------------------------------------------------------
            persberichten.alg.dag <- callModule(data.visual, "bericht.alg.dag", Id = "alg.dag", data = Persstatistiek, Xaxis = "Dag", Fill = "Dag", colours = colours)
          # Plot - aantal
            output$persberichten.alg.dag.plot.aantal <- renderPlot(
              persberichten.alg.dag$plot.aantal()
            )
          # Plot - procent
            output$persberichten.alg.dag.plot.procent <- renderPlot(
              persberichten.alg.dag$plot.procent()
            )
          # Tabel
            output$persberichten.alg.dag.tabel <- DT::renderDataTable(
              persberichten.alg.dag$tabel()
            )
        # Per Week -------------------------------------------------------------
            persberichten.alg.week <- callModule(data.visual, "bericht.alg.week", Id = "alg.week", data = Persstatistiek, Xaxis = "Week", Fill = "Week", colours = colours)
            # Plot - aantal
            output$persberichten.alg.week.plot.aantal <- renderPlot(
              persberichten.alg.week$plot.aantal()
            )
            # Plot - procent
            output$persberichten.alg.week.plot.procent <- renderPlot(
              persberichten.alg.week$plot.procent()
            )
            # Tabel
            output$persberichten.alg.week.tabel <- DT::renderDataTable(
              persberichten.alg.week$tabel()
            )
        # Per Beleid -----------------------------------------------------------
            persberichten.alg.beleid <- callModule(data.visual, "bericht.alg.beleid", Id = "alg.beleid", data = Persstatistiek, Xaxis = "Beleid", Fill = "Beleid", colours = colours)
          # Plot - aantal
            output$persberichten.alg.beleid.plot.aantal <- renderPlot(
              persberichten.alg.beleid$plot.aantal()
            )
          # Plot
            output$persberichten.alg.beleid.plot.procent <- renderPlot(
              persberichten.alg.beleid$plot.procent()
            )
          # Tabel
            output$persberichten.alg.beleid.tabel <- DT::renderDataTable(
              persberichten.alg.beleid$tabel()
            )
      # Persconferenties -------------------------------------------------------
        # Per Kwartaal ---------------------------------------------------------
            persconferenties.alg.kwartaal <- callModule(data.visual, "conferentie.alg.kwartaal", Id = "conferentie.alg.kwartaal", data = Persstatistiek, Xaxis = "Kwartaal", Fill = "Kwartaal", colours = colours)      
          # Plot - aantal
            output$persconferenties.alg.kwartaal.plot.aantal <- renderPlot(
              persconferenties.alg.kwartaal$plot.aantal()
            )
          # Plot - aantal
            output$persconferenties.alg.kwartaal.plot.procent <- renderPlot(
              persconferenties.alg.kwartaal$plot.procent()
            )
          # Tabel
            output$persconferenties.alg.kwartaal.tabel <- DT::renderDataTable(
              persconferenties.alg.kwartaal$tabel()
            )
        # Per Maand ------------------------------------------------------------
            persconferenties.alg.maand <- callModule(data.visual, "conferentie.alg.maand", Id = "conferentie.alg.maand", data = Persstatistiek, Xaxis = "Maand", Fill = "Maand", colours = colours)
          # Plot - aantal
            output$persconferenties.alg.maand.plot.aantal <- renderPlot(
              persconferenties.alg.maand$plot.aantal()
            )
          # Plot - procent
            output$persconferenties.alg.maand.plot.procent <- renderPlot(
              persconferenties.alg.maand$plot.procent()
            )
          # Tabel
            output$persconferenties.alg.maand.tabel <- DT::renderDataTable(
              persconferenties.alg.maand$tabel()
            )
        # Per Dag --------------------------------------------------------------
            persconferenties.alg.dag <- callModule(data.visual, "conferentie.alg.dag", Id = "conferentie.alg.dag", data = Persstatistiek, Xaxis = "Dag", Fill = "Dag", colours = colours)
          # Plot - aantal
            output$persconferenties.alg.dag.plot.aantal <- renderPlot(
              persconferenties.alg.dag$plot.aantal()
            )
          # Plot - procent
            output$persconferenties.alg.dag.plot.procent <- renderPlot(
              persconferenties.alg.dag$plot.procent()
            )
          # Tabel
            output$persconferenties.alg.dag.tabel <- DT::renderDataTable(
              persconferenties.alg.dag$tabel()
            )
        # Per Week -------------------------------------------------------------
            persconferenties.alg.week <- callModule(data.visual, "conferentie.alg.week", Id = "conferentie.alg.week", data = Persstatistiek, Xaxis = "Week", Fill = "Week", colours = colours)
          # Plot - aantal
            output$persconferenties.alg.week.plot.aantal <- renderPlot(
              persconferenties.alg.week$plot.aantal()
            )
          # Plot - procent
            output$persconferenties.alg.week.plot.procent <- renderPlot(
              persconferenties.alg.week$plot.procent()
            )
          # Tabel
            output$persconferenties.alg.week.tabel <- DT::renderDataTable(
              persconferenties.alg.week$tabel()
            )
        # Per Beleid -----------------------------------------------------------
            persconferenties.alg.beleid <- callModule(data.visual, "conferentie.alg.beleid", Id = "conferentie.alg.beleid", data = Persstatistiek, Xaxis = "Beleid", Fill = "Beleid", colours = colours)
          # Plot - aantal
            output$persconferenties.alg.beleid.plot.aantal <- renderPlot(
              persconferenties.alg.beleid$plot.aantal()
            )
          # Plot
            output$persconferenties.alg.beleid.plot.procent <- renderPlot(
              persconferenties.alg.beleid$plot.procent()
            )
          # Tabel
            output$persconferenties.alg.beleid.tabel <- DT::renderDataTable(
              persconferenties.alg.beleid$tabel()
            )
    # PER BELEID ---------------------------------------------------------------
      # Per Maand --------------------------------------------------------------
        # Economie -------------------------------------------------------------
        persberichten.beleid.maand.economie <- callModule(data.visual, "bericht.beleid.maand.plot.aantal.economie", Id = "beleid.maand" ,data = Persstatistiek, Xaxis = "Maand", Fill = "Maand", colours = colours, beleid = "Economie")
          # Plot - aantal
          output$persberichten.beleid.maand.economie.plot.aantal <- renderPlot(
            persberichten.beleid.maand.economie$plot.aantal()
          )
          # Plot - procent
          output$persberichten.beleid.maand.economie.plot.procent <- renderPlot(
            persberichten.beleid.maand.economie$plot.procent()
          )
          # Table
          output$persberichten.beleid.maand.economie.tabel <- DT::renderDataTable(
            persberichten.beleid.maand.economie$tabel()
          )
        # Gouverneur -----------------------------------------------------------
        persberichten.beleid.maand.gouverneur <- callModule(data.visual, "bericht.beleid.maand.plot.aantal.gouverneur", Id = "beleid.maand" , data = Persstatistiek, Xaxis = "Maand", Fill = "Maand", colours = colours, beleid = "Gouverneur")
          # Plot - aantal
          output$persberichten.beleid.maand.gouverneur.plot.aantal <- renderPlot(
            persberichten.beleid.maand.gouverneur$plot.aantal()
          )
          # Plot - procent
          output$persberichten.beleid.maand.gouverneur.plot.procent <- renderPlot(
            persberichten.beleid.maand.gouverneur$plot.procent()
          )
          # Table
          output$persberichten.beleid.maand.gouverneur.tabel <- DT::renderDataTable(
            persberichten.beleid.maand.gouverneur$tabel()
          )
          
        # Leefmilieu -----------------------------------------------------------
        persberichten.beleid.maand.leefmilieu <- callModule(data.visual, "bericht.beleid.maand.plot.aantal.leefmilieu", Id = "beleid.maand" , data = Persstatistiek, Xaxis = "Maand", Fill = "Maand", colours = colours, beleid = "Leefmilieu")
          # Plot - aantal
          output$persberichten.beleid.maand.leefmilieu.plot.aantal <- renderPlot(
            persberichten.beleid.maand.leefmilieu$plot.aantal()
          )
          # Plot - procent
          output$persberichten.beleid.maand.leefmilieu.plot.procent <- renderPlot(
            persberichten.beleid.maand.leefmilieu$plot.procent()
          )
          # Table
          output$persberichten.beleid.maand.leefmilieu.tabel <- DT::renderDataTable(
            persberichten.beleid.maand.leefmilieu$tabel()
          )
        # Mobiliteit -----------------------------------------------------------
        persberichten.beleid.maand.mobiliteit <- callModule(data.visual, "bericht.beleid.maand.plot.aantal.mobiliteit", Id = "beleid.maand" , data = Persstatistiek, Xaxis = "Maand", Fill = "Maand", colours = colours, beleid = "Mobiliteit")
          # Plot - aantal
          output$persberichten.beleid.maand.mobiliteit.plot.aantal <- renderPlot(
            persberichten.beleid.maand.mobiliteit$plot.aantal()
          )
          # Plot - procentueel
          output$persberichten.beleid.maand.mobiliteit.plot.procent <- renderPlot(
            persberichten.beleid.maand.mobiliteit$plot.procent()
          )
          # Table
          output$persberichten.beleid.maand.mobiliteit.tabel <- DT::renderDataTable(
            persberichten.beleid.maand.mobiliteit$tabel()
          )
        # Onderwijs en Educatie-------------------------------------------------
        persberichten.beleid.maand.onderwijs <- callModule(data.visual, "bericht.beleid.maand.plot.aantal.onderwijs", Id = "beleid.maand" , data = Persstatistiek, Xaxis = "Maand", Fill = "Maand", colours = colours, beleid = "Onderwijs en Educatie")
          # Plot - aantal
          output$persberichten.beleid.maand.onderwijs.plot.aantal <- renderPlot(
            persberichten.beleid.maand.onderwijs$plot.aantal()
          )
          # Plot- procentueel
          output$persberichten.beleid.maand.onderwijs.plot.procent <- renderPlot(
            persberichten.beleid.maand.onderwijs$plot.procent()
          )
          # Tabel
          output$persberichten.beleid.maand.onderwijs.tabel <- DT::renderDataTable(
            persberichten.beleid.maand.onderwijs$tabel()
          )
        # Provinciebestuur -----------------------------------------------------
        persberichten.beleid.maand.provinciebestuur <- callModule(data.visual, "bericht.beleid.maand.plot.aantal.provinciebestuur", Id = "beleid.maand" , data = Persstatistiek, Xaxis = "Maand", Fill = "Maand", colours = colours, beleid = "Provinciebestuur")
          # Plot  - aantal
          output$persberichten.beleid.maand.provinciebestuur.plot.aantal <- renderPlot(
            persberichten.beleid.maand.provinciebestuur$plot.aantal()
          )
          # Plot  - procentueel
          output$persberichten.beleid.maand.provinciebestuur.plot.procent <- renderPlot(
            persberichten.beleid.maand.provinciebestuur$plot.procent()
          )
          # Table
          output$persberichten.beleid.maand.provinciebestuur.tabel <- DT::renderDataTable(
            persberichten.beleid.maand.provinciebestuur$tabel()
          )
        # Ruimte ---------------------------------------------------------------
        persberichten.beleid.maand.ruimte <- callModule(data.visual, "bericht.beleid.maand.plot.aantal.ruimte", Id = "beleid.maand" , data = Persstatistiek, Xaxis = "Maand", Fill = "Maand", colours = colours, beleid = "Ruimte")
          # Plot  - antal
          output$persberichten.beleid.maand.ruimte.plot.aantal <- renderPlot(
            persberichten.beleid.maand.ruimte$plot.aantal()
          )
          # Plot  - procentueel
          output$persberichten.beleid.maand.ruimte.plot.procent <- renderPlot(
            persberichten.beleid.maand.ruimte$plot.procent()
          )
          # Table
          output$persberichten.beleid.maand.ruimte.tabel <- DT::renderDataTable(
            persberichten.beleid.maand.ruimte$tabel()
          )
        # Vrije Tijd -----------------------------------------------------------
        persberichten.beleid.maand.vrijetijd <- callModule(data.visual, "bericht.beleid.maand.plot.aantal.vrijetijd", Id = "beleid.maand" , data = Persstatistiek, Xaxis = "Maand", Fill = "Maand", colours = colours, beleid = "Vrije Tijd")
          # Plot - aantal 
          output$persberichten.beleid.maand.vrijetijd.plot.aantal <- renderPlot(
            persberichten.beleid.maand.vrijetijd$plot.aantal()
          )
          # Plot  - procentueel
          output$persberichten.beleid.maand.vrijetijd.plot.procent <- renderPlot(
            persberichten.beleid.maand.vrijetijd$plot.procent()
          )
          # Table
          output$persberichten.beleid.maand.vrijetijd.tabel <- DT::renderDataTable(
            persberichten.beleid.maand.vrijetijd$tabel()
          )

      # Per Deelbeleid ---------------------------------------------------------
        # Economie -------------------------------------------------------------
        persberichten.beleid.beleid.economie <- callModule(data.visual, "bericht.beleid.beleid.plot.aantal.economie", Id = "beleid.beleid" , Persstatistiek, Xaxis = "Deelbeleid", Fill = "Deelbeleid", colours = colours, beleid = "Economie", datadeelbeleid = reactive(input$Economie.ActieveDeelbeleiden))
          # Plot - aantal
          output$persberichten.beleid.beleid.economie.plot.aantal <- renderPlot(
            persberichten.beleid.beleid.economie$plot.aantal()
          )
          # Plot - procent
          output$persberichten.beleid.beleid.economie.plot.procent <- renderPlot(
            persberichten.beleid.beleid.economie$plot.procent()
          )
          # Table
          output$persberichten.beleid.beleid.economie.tabel <- DT::renderDataTable(
            persberichten.beleid.beleid.economie$tabel()
          )
        # Gouverneur -----------------------------------------------------------
        persberichten.beleid.beleid.gouverneur <- callModule(data.visual, "bericht.beleid.beleid.plot.aantal.gouverneur", Id = "beleid.beleid" , Persstatistiek, Xaxis = "Deelbeleid", Fill = "Deelbeleid", colours = colours, beleid = "Gouverneur", datadeelbeleid = reactive(input$Gouverneur.ActieveDeelbeleiden))
          # Plot  -aantal
          output$persberichten.beleid.beleid.gouverneur.plot.aantal <- renderPlot(
            persberichten.beleid.beleid.gouverneur$plot.aantal()
          )
          # Plot  - procent
          output$persberichten.beleid.beleid.gouverneur.plot.procent <- renderPlot(
            persberichten.beleid.beleid.gouverneur$plot.procent()
          )
          # Table
          output$persberichten.beleid.beleid.gouverneur.tabel <- DT::renderDataTable(
            persberichten.beleid.beleid.gouverneur$tabel()
          )
        # Leefmilieu -----------------------------------------------------------
        persberichten.beleid.beleid.leefmilieu <- callModule(data.visual, "bericht.beleid.beleid.plot.aantal.leefmilieu", Id = "beleid.beleid" , Persstatistiek, Xaxis = "Deelbeleid", Fill = "Deelbeleid", colours = colours, beleid = "Leefmilieu", datadeelbeleid = reactive(input$Leefmilieu.ActieveDeelbeleiden))
          # Plot -aantal
          output$persberichten.beleid.beleid.leefmilieu.plot.aantal <- renderPlot(
            persberichten.beleid.beleid.leefmilieu$plot.aantal()
          )
          # Plot - procent
          output$persberichten.beleid.beleid.leefmilieu.plot.procent <- renderPlot(
            persberichten.beleid.beleid.leefmilieu$plot.procent()
          )
          # Table
          output$persberichten.beleid.beleid.leefmilieu.tabel <- DT::renderDataTable(
            persberichten.beleid.beleid.leefmilieu$tabel()
          )
        # Mobiliteit -----------------------------------------------------------
        persberichten.beleid.beleid.mobiliteit <- callModule(data.visual, "bericht.beleid.beleid.plot.aantal.mobiliteit", Id = "beleid.beleid" , Persstatistiek, Xaxis = "Deelbeleid", Fill = "Deelbeleid", colours = colours, beleid = "Mobiliteit", datadeelbeleid = reactive(input$Mobiliteit.ActieveDeelbeleiden))
          # Plot - aantal 
          output$persberichten.beleid.beleid.mobiliteit.plot.aantal <- renderPlot(
            persberichten.beleid.beleid.mobiliteit$plot.aantal()
          )
          # Plot  - procent
          output$persberichten.beleid.beleid.mobiliteit.plot.procent <- renderPlot(
            persberichten.beleid.beleid.mobiliteit$plot.procent()
          )
          # Table
          output$persberichten.beleid.beleid.mobiliteit.tabel <- DT::renderDataTable(
            persberichten.beleid.beleid.mobiliteit$tabel()
          )
        # Onderwijs en Educatie ------------------------------------------------
        persberichten.beleid.beleid.onderwijs <- callModule(data.visual, "bericht.beleid.beleid.plot.aantal.onderwijs", Id = "beleid.beleid" , Persstatistiek, Xaxis = "Deelbeleid", Fill = "Deelbeleid", colours = colours, beleid = "Onderwijs en Educatie", datadeelbeleid = reactive(input$Onderwijs.ActieveDeelbeleiden))
          # Plot -aantal 
          output$persberichten.beleid.beleid.onderwijs.plot.aantal <- renderPlot(
            persberichten.beleid.beleid.onderwijs$plot.aantal()
          )
          # Plot  - procent
          output$persberichten.beleid.beleid.onderwijs.plot.procent <- renderPlot(
            persberichten.beleid.beleid.onderwijs$plot.procent()
          )
          # Tabel
          output$persberichten.beleid.beleid.onderwijs.tabel <- DT::renderDataTable(
            persberichten.beleid.beleid.onderwijs$tabel()
          )
        # Provinciebestuur -----------------------------------------------------
        persberichten.beleid.beleid.provinciebestuur <- callModule(data.visual, "bericht.beleid.beleid.plot.aantal.provinciebestuur", Id = "beleid.beleid" , Persstatistiek, Xaxis = "Deelbeleid", Fill = "Deelbeleid", colours = colours, beleid = "Provinciebestuur", datadeelbeleid = reactive(input$Provinciebestuur.ActieveDeelbeleiden))
          # Plot  - aantal
          output$persberichten.beleid.beleid.provinciebestuur.plot.aantal <- renderPlot(
            persberichten.beleid.beleid.provinciebestuur$plot.aantal()
          )
          # Plot  - procent
          output$persberichten.beleid.beleid.provinciebestuur.plot.procent <- renderPlot(
            persberichten.beleid.beleid.provinciebestuur$plot.procent()
          )
          # Table
          output$persberichten.beleid.beleid.provinciebestuur.tabel <- DT::renderDataTable(
            persberichten.beleid.beleid.provinciebestuur$tabel()
          )
        # Ruimte ---------------------------------------------------------------
        persberichten.beleid.beleid.ruimte <- callModule(data.visual, "bericht.beleid.beleid.plot.aantal.ruimte", Id = "beleid.beleid" , Persstatistiek, Xaxis = "Deelbeleid", Fill = "Deelbeleid", colours = colours, beleid = "Ruimte", datadeelbeleid = reactive(input$Ruimte.ActieveDeelbeleiden))
          # Plot  - aantal
          output$persberichten.beleid.beleid.ruimte.plot.aantal <- renderPlot(
            persberichten.beleid.beleid.ruimte$plot.aantal()
          )
          # Plot  - procent
          output$persberichten.beleid.beleid.ruimte.plot.procent <- renderPlot(
            persberichten.beleid.beleid.ruimte$plot.procent()
          )
          # Table
          output$persberichten.beleid.beleid.ruimte.tabel <- DT::renderDataTable(
            persberichten.beleid.beleid.ruimte$tabel()
        )
        # Vrije Tijd -----------------------------------------------------------
        persberichten.beleid.beleid.vrijetijd <- callModule(data.visual, "bericht.beleid.beleid.plot.aantal.vrijetijd", Id = "beleid.beleid" , Persstatistiek, Xaxis = "Deelbeleid", Fill = "Deelbeleid", colours = colours, beleid = "Vrije Tijd", datadeelbeleid = reactive(input$VrijeTijd.ActieveDeelbeleiden))
          # Plot  - aantal
          output$persberichten.beleid.beleid.vrijetijd.plot.aantal <- renderPlot(
            persberichten.beleid.beleid.vrijetijd$plot.aantal()
          )
          # Plot  - procent
          output$persberichten.beleid.beleid.vrijetijd.plot.procent <- renderPlot(
            persberichten.beleid.beleid.vrijetijd$plot.procent()
          )
          # Table
          output$persberichten.beleid.beleid.vrijetijd.tabel <- DT::renderDataTable(
            persberichten.beleid.beleid.vrijetijd$tabel()
          )
    # PER VERZENDER ------------------------------------------------------------
      # Algemeen ---------------------------------------------------------------
        # Totaal per Verzender -------------------------------------------------
          persberichten.verzender.alg.totaal <- callModule(data.visual, "bericht.verzender.alg.totaal", Id = "verzender.alg.verzender" , Persstatistiek, Xaxis = "Verzender", Fill = "Verzender", colours = colours)
          # Plot - aantal
          output$persberichten.verzender.alg.totaal.plot.aantal <- renderPlot(
            persberichten.verzender.alg.totaal$plot.aantal()
          )
          # Plot - procent
          output$persberichten.verzender.alg.totaal.plot.procent <- renderPlot(
            persberichten.verzender.alg.totaal$plot.procent()
          )
          # Tabel
          output$persberichten.verzender.alg.totaal.tabel <- DT::renderDataTable(
            persberichten.verzender.alg.totaal$tabel()
          )
        # Beleid per Verzender -------------------------------------------------
          persberichten.verzender.alg.beleid <- callModule(data.visual, "bericht.verzender.alg.beleid", Id = "verzender.alg.beleid" , Persstatistiek, Xaxis = "Verzender", Fill = "Beleid", colours = colours)
          # Plot - aantal
          
          output$persberichten.verzender.alg.beleid.plot.aantal <- renderPlot(
            persberichten.verzender.alg.beleid$plot.aantal()
          )
          # Plot - procent
          output$persberichten.verzender.alg.beleid.plot.procent <- renderPlot(
            persberichten.verzender.alg.beleid$plot.procent()
          )
          # Tabel
          output$persberichten.verzender.alg.beleid.tabel <- DT::renderDataTable(
            persberichten.verzender.alg.beleid$tabel()
          )
      # Per Maand --------------------------------------------------------------
        # Persdienst -----------------------------------------------------------
          persberichten.verzender.maand.persdienst <- callModule(data.visual, "bericht.verzender.maand.persdienst", Id = "verzender.maand" , Persstatistiek, Xaxis = "Maand", Fill = "Maand", colours = colours, verzender = "Persdienst")
          # Plot - aantal
          output$persberichten.verzender.maand.persdienst.plot.aantal <- renderPlot(
            persberichten.verzender.maand.persdienst$plot.aantal()
          )
          # Plot - procent
          output$persberichten.verzender.maand.persdienst.plot.procent <- renderPlot(
            persberichten.verzender.maand.persdienst$plot.procent()
          )
          # Tabel
          output$persberichten.verzender.maand.persdienst.tabel <- DT::renderDataTable(
            persberichten.verzender.maand.persdienst$tabel()
          )
        # Provincie ------------------------------------------------------------
          persberichten.verzender.maand.provincie <- callModule(data.visual, "bericht.verzender.maand.provincie", Id = "verzender.maand" , Persstatistiek, Xaxis = "Maand", Fill = "Maand", colours = colours, verzender = "Provincie")
          # Plot - aantal
          output$persberichten.verzender.maand.provincie.plot.aantal <- renderPlot(
            persberichten.verzender.maand.provincie$plot.aantal()
          )
          # Plot - procent
          output$persberichten.verzender.maand.provincie.plot.procent <- renderPlot(
            persberichten.verzender.maand.provincie$plot.procent()
          )
          # Tabel
          output$persberichten.verzender.maand.provincie.tabel <- DT::renderDataTable(
            persberichten.verzender.maand.provincie$tabel()
          )
        # Gouverneur -----------------------------------------------------------
          persberichten.verzender.maand.gouverneur <- callModule(data.visual, "bericht.verzender.maand.gouverneur", Id = "verzender.maand" , Persstatistiek, Xaxis = "Maand", Fill = "Maand", colours = colours, verzender = "Gouverneur")
          # Plot - aantal
          output$persberichten.verzender.maand.gouverneur.plot.aantal <- renderPlot(
            persberichten.verzender.maand.gouverneur$plot.aantal()
          )
          # Plot - procent
          output$persberichten.verzender.maand.gouverneur.plot.procent <- renderPlot(
            persberichten.verzender.maand.gouverneur$plot.procent()
          )
          # Tabel
          output$persberichten.verzender.maand.gouverneur.tabel <- DT::renderDataTable(
            persberichten.verzender.maand.gouverneur$tabel()
          )
        # Extern ---------------------------------------------------------------
          persberichten.verzender.maand.extern <- callModule(data.visual, "bericht.verzender.maand.extern", Id = "verzender.maand" , Persstatistiek, Xaxis = "Maand", Fill = "Maand", colours = colours, verzender = "Extern")
          # Plot - aantal
          output$persberichten.verzender.maand.extern.plot.aantal <- renderPlot(
            persberichten.verzender.maand.extern$plot.aantal()
          )
          # Plot - procent
          output$persberichten.verzender.maand.extern.plot.procent <- renderPlot(
            persberichten.verzender.maand.extern$plot.procent()
          )
          # Tabel
          output$persberichten.verzender.maand.extern.tabel <- DT::renderDataTable(
            persberichten.verzender.maand.extern$tabel()
          )
    # PER TYPE -----------------------------------------------------------------
      persberichten.type <- callModule(data.visual, "bericht.type", Id = "type" , Persstatistiek, Xaxis = "Beleid", Fill = "Type", colours = colours)
      # Plot - aantal
      output$persberichten.type.plot.aantal <- renderPlot(
        persberichten.type$plot.aantal()
      )
      # Plot - procent
      output$persberichten.type.plot.procent <- renderPlot(
        persberichten.type$plot.procent()
      )
      # Tabel
      output$persberichten.type.tabel <- DT::renderDataTable(
        persberichten.type$tabel()
      )
  # ============================================================================
  
  # PERSRETURN =================================================================
    # PER BELEID ---------------------------------------------------------------
      # Algemeen ---------------------------------------------------------------
      persreturn.beleid.alg <- callModule(data.visual, "return.beleid.alg", Id = "return.beleid.alg" , Persstatistiek, Xaxis = "Beleid", Fill = "Persreturn", colours = return.colours)
        # Plot - aantal
        output$persreturn.beleid.alg.plot.aantal <- renderPlot(
          persreturn.beleid.alg$plot.aantal()
        )
        # Plot - procent
        output$persreturn.beleid.alg.plot.procent <- renderPlot(
          persreturn.beleid.alg$plot.procent()
        )
        # Tabel 
        output$persreturn.beleid.alg.tabel <- DT::renderDataTable(
          persreturn.beleid.alg$tabel()
        )
      # Deelbeleid -------------------------------------------------------------
        # Economie -------------------------------------------------------------
        persreturn.beleid.beleid.economie <- callModule(data.visual, "return.beleid.beleid.economie", Id = "return.beleid.beleid" , Persstatistiek, Xaxis = "Deelbeleid", Fill = "Persreturn", colours = return.colours, beleid = "Economie", datadeelbeleid = reactive(input$Economie.ActieveDeelbeleiden))
          # Plot - aantal
          output$persreturn.beleid.beleid.economie.plot.aantal <- renderPlot(
            persreturn.beleid.beleid.economie$plot.aantal()
          )
          # Plot - procent
          output$persreturn.beleid.beleid.economie.plot.procent <- renderPlot(
            persreturn.beleid.beleid.economie$plot.procent()
          )
          # Table
          output$persreturn.beleid.beleid.economie.tabel <- DT::renderDataTable(
            persreturn.beleid.beleid.economie$tabel()
          )
        # Gouverneur -----------------------------------------------------------
        persreturn.beleid.beleid.gouverneur <- callModule(data.visual, "return.beleid.beleid.gouverneur", Id = "return.beleid.beleid" , Persstatistiek, Xaxis = "Deelbeleid", Fill = "Persreturn", colours = return.colours, beleid = "Gouverneur", datadeelbeleid =reactive(input$Gouverneur.ActieveDeelbeleiden))
          # Plot  - antal
          output$persreturn.beleid.beleid.gouverneur.plot.aantal <- renderPlot(
            persreturn.beleid.beleid.gouverneur$plot.aantal()
          )
          # Plot - procent 
          output$persreturn.beleid.beleid.gouverneur.plot.procent <- renderPlot(
            persreturn.beleid.beleid.gouverneur$plot.procent()
          )
          # Table
          output$persreturn.beleid.beleid.gouverneur.tabel <- DT::renderDataTable(
            persreturn.beleid.beleid.gouverneur$tabel()
          )
        # Leefmilieu -----------------------------------------------------------
        persreturn.beleid.beleid.leefmilieu <- callModule(data.visual, "return.beleid.beleid.leefmilieu", Id = "return.beleid.beleid" , Persstatistiek, Xaxis = "Deelbeleid", Fill = "Persreturn", colours = return.colours, beleid = "Leefmilieu", datadeelbeleid = reactive(input$Leefmilieu.ActieveDeelbeleiden))
          # Plot - aantal
          output$persreturn.beleid.beleid.leefmilieu.plot.aantal <- renderPlot(
            persreturn.beleid.beleid.leefmilieu$plot.aantal()
          )
          # Plot - aprocnet
          output$persreturn.beleid.beleid.leefmilieu.plot.procent <- renderPlot(
            persreturn.beleid.beleid.leefmilieu$plot.procent()
          )
          # Table
          output$persreturn.beleid.beleid.leefmilieu.tabel <- DT::renderDataTable(
            persreturn.beleid.beleid.leefmilieu$tabel()
          )
        # Mobiliteit -----------------------------------------------------------
        persreturn.beleid.beleid.mobiliteit <- callModule(data.visual, "return.beleid.beleid.mobiliteit", Id = "return.beleid.beleid" , Persstatistiek, Xaxis = "Deelbeleid", Fill = "Persreturn", colours = return.colours, beleid = "Mobiliteit", datadeelbeleid=reactive(input$Mobiliteit.ActieveDeelbeleiden))
          # Plot  - aantal
          output$persreturn.beleid.beleid.mobiliteit.plot.aantal <- renderPlot(
            persreturn.beleid.beleid.mobiliteit$plot.aantal()
          )
          # Plot  - procent
          output$persreturn.beleid.beleid.mobiliteit.plot.procent <- renderPlot(
            persreturn.beleid.beleid.mobiliteit$plot.procent()
          )
          # Table
          output$persreturn.beleid.beleid.mobiliteit.tabel <- DT::renderDataTable(
            persreturn.beleid.beleid.mobiliteit$tabel()
          )
        # Onderwijs en Educatie ------------------------------------------------
        persreturn.beleid.beleid.onderwijs <- callModule(data.visual, "return.beleid.beleid.onderwijs", Id = "return.beleid.beleid" , Persstatistiek, Xaxis = "Deelbeleid", Fill = "Persreturn", colours = return.colours, beleid = "Onderwijs en Educatie", datadeelbeleid = reactive(input$Onderwijs.ActieveDeelbeleiden))
          # Plot  -aantal
          output$persreturn.beleid.beleid.onderwijs.plot.aantal <- renderPlot(
            persreturn.beleid.beleid.onderwijs$plot.aantal()
          )
          # Plot  - procent
          output$persreturn.beleid.beleid.onderwijs.plot.procent <- renderPlot(
            persreturn.beleid.beleid.onderwijs$plot.procent()
          )
          # Tabel
          output$persreturn.beleid.beleid.onderwijs.tabel <- DT::renderDataTable(
            persreturn.beleid.beleid.onderwijs$tabel()
          )
        # Provinciebestuur -----------------------------------------------------
        persreturn.beleid.beleid.provinciebestuur <- callModule(data.visual, "return.beleid.beleid.provinciebestuur", Id = "return.beleid.beleid" , Persstatistiek, Xaxis = "Deelbeleid", Fill = "Persreturn", colours = return.colours, beleid = "Provinciebestuur", datadeelbeleid = reactive(input$Provinciebestuur.ActieveDeelbeleiden))
          # Plot  - aantal
          output$persreturn.beleid.beleid.provinciebestuur.plot.aantal <- renderPlot(
            persreturn.beleid.beleid.provinciebestuur$plot.aantal()
          )
          # Plot  - procent
          output$persreturn.beleid.beleid.provinciebestuur.plot.procent <- renderPlot(
            persreturn.beleid.beleid.provinciebestuur$plot.procent()
          )
          # Table
          output$persreturn.beleid.beleid.provinciebestuur.tabel <- DT::renderDataTable(
            persreturn.beleid.beleid.provinciebestuur$tabel()
          )
        # Ruimte ---------------------------------------------------------------
        persreturn.beleid.beleid.ruimte <- callModule(data.visual, "return.beleid.beleid.ruimte", Id = "return.beleid.beleid" , Persstatistiek, Xaxis = "Deelbeleid", Fill = "Persreturn", colours = return.colours, beleid = "Ruimte", datadeelbeleid = reactive(input$Ruimte.ActieveDeelbeleiden))
          # Plot  - aantal
          output$persreturn.beleid.beleid.ruimte.plot.aantal <- renderPlot(
            persreturn.beleid.beleid.ruimte$plot.aantal()
          )
          # Plot  - procent
          output$persreturn.beleid.beleid.ruimte.plot.procent <- renderPlot(
            persreturn.beleid.beleid.ruimte$plot.procent()
          )
          # Table
          output$persreturn.beleid.beleid.ruimte.tabel <- DT::renderDataTable(
            persreturn.beleid.beleid.ruimte$tabel()
          )
        # Vrije Tijd -----------------------------------------------------------
        persreturn.beleid.beleid.vrijetijd <- callModule(data.visual, "return.beleid.beleid.vrijetijd", Id = "return.beleid.beleid" , Persstatistiek, Xaxis = "Deelbeleid", Fill = "Persreturn", colours = return.colours, beleid = "Vrije Tijd", datadeelbeleid = reactive(input$VrijeTijd.ActieveDeelbeleiden))
          # Plot  - aantal
          output$persreturn.beleid.beleid.vrijetijd.plot.aantal <- renderPlot(
            persreturn.beleid.beleid.vrijetijd$plot.aantal()
          )
          # Plot  - procent
          output$persreturn.beleid.beleid.vrijetijd.plot.procent <- renderPlot(
            persreturn.beleid.beleid.vrijetijd$plot.procent()
          )
          # Table
          output$persreturn.beleid.beleid.vrijetijd.tabel <- DT::renderDataTable(
            persreturn.beleid.beleid.vrijetijd$tabel()
          )
    # PER MEDIUM ---------------------------------------------------------------
      persreturn.medium <- callModule(data.visual, "return.medium", Id = "return.medium" , Persstatistiek, Xaxis = "Beleid", Fill = "Medium", colours = colours)
      # Plot - aantal
      output$persreturn.medium.plot.aantal <- renderPlot(
        persreturn.medium$plot.aantal()
      )
      # Plot - procent
      output$persreturn.medium.plot.procent <- renderPlot(
        persreturn.medium$plot.procent()
      )
      # Tabel
      output$persreturn.medium.tabel <- DT::renderDataTable(
        persreturn.medium$tabel()
      )
  # ============================================================================
      
  # HTML RAPPORT AANMAAK =======================================================
  output$report <- downloadHandler(
    
    # For PDF output, change this to "report.pdf"
    filename = reactive(paste0("Persstatistiek_", paste0(input$jaar, paste0("_", paste0(input$kwartaal, ".html"))))),
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).

      tempReport <- file.path(tempdir(), "JaarVolledig.Rmd")
      file.copy("./Reports/JaarVolledig.Rmd", tempReport, overwrite = TRUE)
      
      
    # VARIABLE COLLECTION FOR MARKDOWN (HTML) ----------------------------------
      # Persberichten ----------------------------------------------------------
        # Algemeen -------------------------------------------------------------
          # Persberichten ------------------------------------------------------
          Persberichten.alg <- list(
            kwartaal.plot.aantal = reactive(persberichten.alg.kwartaal$plot.aantal()),
            kwartaal.plot.procent = reactive(persberichten.alg.kwartaal$plot.procent()),
            kwartaal.tabel = reactive(persberichten.alg.kwartaal$tabel()),
            kwartaal.uitleg = reactive(persberichten.alg.kwartaal$uitleg()),
            maand.plot.aantal = reactive(persberichten.alg.maand$plot.aantal()),
            maand.plot.procent = reactive(persberichten.alg.maand$plot.procent()),
            maand.tabel = reactive(persberichten.alg.maand$tabel()),
            maand.uitleg = reactive(persberichten.alg.maand$uitleg()),
            dag.plot.aantal = reactive(persberichten.alg.dag$plot.aantal()),
            dag.plot.procent = reactive(persberichten.alg.dag$plot.procent()),
            dag.tabel = reactive(persberichten.alg.dag$tabel()),
            dag.uitleg = reactive(persberichten.alg.dag$uitleg()),
            week.plot.aantal = reactive(persberichten.alg.week$plot.aantal()),
            week.plot.procent = reactive(persberichten.alg.week$plot.procent()),
            week.tabel = reactive(persberichten.alg.week$tabel()),
            week.uitleg = reactive(persberichten.alg.week$uitleg()),
            beleid.plot.aantal = reactive(persberichten.alg.beleid$plot.aantal()),
            beleid.plot.procent = reactive(persberichten.alg.beleid$plot.procent()),
            beleid.tabel = reactive(persberichten.alg.beleid$tabel()),
            beleid.uitleg= reactive(persberichten.alg.beleid$uitleg())
          )
          # Persconferenties ---------------------------------------------------
          Persconferenties.alg <- list(
            kwartaal.plot.aantal = reactive(persconferenties.alg.kwartaal$plot.aantal()),
            kwartaal.plot.procent = reactive(persconferenties.alg.kwartaal$plot.procent()),
            kwartaal.tabel = reactive(persconferenties.alg.kwartaal$tabel()),
            kwartaal.uitleg = reactive(persconferenties.alg.kwartaal$uitleg()),
            maand.plot.aantal = reactive(persconferenties.alg.maand$plot.aantal()),
            maand.plot.procent = reactive(persconferenties.alg.maand$plot.procent()),
            maand.tabel = reactive(persconferenties.alg.maand$tabel()),
            maand.uitleg = reactive(persconferenties.alg.maand$uitleg()),
            dag.plot.aantal = reactive(persconferenties.alg.dag$plot.aantal()),
            dag.plot.procent = reactive(persconferenties.alg.dag$plot.procent()),
            dag.tabel = reactive(persconferenties.alg.dag$tabel()),
            dag.uitleg = reactive(persconferenties.alg.dag$uitleg()),
            week.plot.aantal = reactive(persconferenties.alg.week$plot.aantal()),
            week.plot.procent = reactive(persconferenties.alg.week$plot.procent()),
            week.tabel = reactive(persconferenties.alg.week$tabel()),
            week.uitleg = reactive(persconferenties.alg.week$uitleg()),
            beleid.plot.aantal = reactive(persconferenties.alg.beleid$plot.aantal()),
            beleid.plot.procent = reactive(persconferenties.alg.beleid$plot.procent()),
            beleid.tabel = reactive(persconferenties.alg.beleid$tabel()),
            beleid.uitleg = reactive(persconferenties.alg.beleid$uitleg())
          )
        # Per beleid -----------------------------------------------------------
          # Maand --------------------------------------------------------------
          Persberichten.beleid.maand <- list(economie.plot.aantal = reactive(persberichten.beleid.maand.economie$plot.aantal()),
                                             economie.plot.procent = reactive(persberichten.beleid.maand.economie$plot.procent()),
                                             economie.tabel = reactive(persberichten.beleid.maand.economie$tabel()),
                                             economie.uitleg = reactive(persberichten.beleid.maand.economie$uitleg()),
                                             gouverneur.plot.aantal = reactive(persberichten.beleid.maand.gouverneur$plot.aantal()),
                                             gouverneur.plot.procent = reactive(persberichten.beleid.maand.gouverneur$plot.procent()),
                                             gouverneur.tabel = reactive(persberichten.beleid.maand.gouverneur$tabel()),
                                             gouverneur.uitleg = reactive(persberichten.beleid.maand.gouverneur$uitleg()),
                                             leefmilieu.plot.aantal = reactive(persberichten.beleid.maand.leefmilieu$plot.aantal()),
                                             leefmilieu.plot.procent = reactive(persberichten.beleid.maand.leefmilieu$plot.procent()),
                                             leefmilieu.tabel = reactive(persberichten.beleid.maand.leefmilieu$tabel()),
                                             leefmilieu.uitleg = reactive(persberichten.beleid.maand.leefmilieu$uitleg()),
                                             mobiliteit.plot.aantal = reactive(persberichten.beleid.maand.mobiliteit$plot.aantal()),
                                             mobiliteit.plot.procent = reactive(persberichten.beleid.maand.mobiliteit$plot.procent()),
                                             mobiliteit.tabel = reactive(persberichten.beleid.maand.mobiliteit$tabel()),
                                             mobiliteit.uitleg = reactive(persberichten.beleid.maand.mobiliteit$uitleg()),
                                             onderwijs.plot.aantal = reactive(persberichten.beleid.maand.onderwijs$plot.aantal()),
                                             onderwijs.plot.procent = reactive(persberichten.beleid.maand.onderwijs$plot.procent()),
                                             onderwijs.tabel = reactive(persberichten.beleid.maand.onderwijs$tabel()),
                                             onderwijs.uitleg = reactive(persberichten.beleid.maand.onderwijs$uitleg()),
                                             provinciebestuur.plot.aantal = reactive(persberichten.beleid.maand.provinciebestuur$plot.aantal()),
                                             provinciebestuur.plot.procent = reactive(persberichten.beleid.maand.provinciebestuur$plot.procent()),
                                             provinciebestuur.tabel = reactive(persberichten.beleid.maand.provinciebestuur$tabel()),
                                             provinciebestuur.uitleg = reactive(persberichten.beleid.maand.provinciebestuur$uitleg()),
                                             ruimte.plot.aantal = reactive(persberichten.beleid.maand.ruimte$plot.aantal()),
                                             ruimte.plot.procent = reactive(persberichten.beleid.maand.ruimte$plot.procent()),
                                             ruimte.tabel = reactive(persberichten.beleid.maand.ruimte$tabel()),
                                             ruimte.uitleg = reactive(persberichten.beleid.maand.ruimte$uitleg()),
                                             vrijetijd.plot.aantal = reactive(persberichten.beleid.maand.vrijetijd$plot.aantal()),
                                             vrijetijd.plot.procent = reactive(persberichten.beleid.maand.vrijetijd$plot.procent()),
                                             vrijetijd.tabel = reactive(persberichten.beleid.maand.vrijetijd$tabel()),
                                             vrijetijd.uitleg = reactive(persberichten.beleid.maand.vrijetijd$uitleg()))
          # Beleid -------------------------------------------------------------
          Persberichten.beleid.beleid <- list(economie.plot.aantal = reactive(persberichten.beleid.beleid.economie$plot.aantal()),
                                              economie.plot.procent = reactive(persberichten.beleid.beleid.economie$plot.procent()),
                                              economie.tabel = reactive(persberichten.beleid.beleid.economie$tabel()),
                                              economie.uitleg = reactive(persberichten.beleid.beleid.economie$uitleg()),
                                              gouverneur.plot.aantal = reactive(persberichten.beleid.beleid.gouverneur$plot.aantal()),
                                              gouverneur.plot.procent = reactive(persberichten.beleid.beleid.gouverneur$plot.procent()),
                                              gouverneur.tabel = reactive(persberichten.beleid.beleid.gouverneur$tabel()),
                                              gouverneur.uitleg = reactive(persberichten.beleid.beleid.gouverneur$uitleg()),
                                              leefmilieu.plot.aantal = reactive(persberichten.beleid.beleid.leefmilieu$plot.aantal()),
                                              leefmilieu.plot.procent = reactive(persberichten.beleid.beleid.leefmilieu$plot.procent()),
                                              leefmilieu.tabel = reactive(persberichten.beleid.beleid.leefmilieu$tabel()),
                                              leefmilieu.uitleg = reactive(persberichten.beleid.beleid.leefmilieu$uitleg()),
                                              mobiliteit.plot.aantal = reactive(persberichten.beleid.beleid.mobiliteit$plot.aantal()),
                                              mobiliteit.plot.procent = reactive(persberichten.beleid.beleid.mobiliteit$plot.procent()),
                                              mobiliteit.tabel = reactive(persberichten.beleid.beleid.mobiliteit$tabel()),
                                              mobiliteit.uitleg = reactive(persberichten.beleid.beleid.mobiliteit$uitleg()),
                                              onderwijs.plot.aantal = reactive(persberichten.beleid.beleid.onderwijs$plot.aantal()),
                                              onderwijs.plot.procent = reactive(persberichten.beleid.beleid.onderwijs$plot.procent()),
                                              onderwijs.tabel = reactive(persberichten.beleid.beleid.onderwijs$tabel()),
                                              onderwijs.uitleg = reactive(persberichten.beleid.beleid.onderwijs$uitleg()),
                                              provinciebestuur.plot.aantal = reactive(persberichten.beleid.beleid.provinciebestuur$plot.aantal()),
                                              provinciebestuur.plot.procent = reactive(persberichten.beleid.beleid.provinciebestuur$plot.procent()),
                                              provinciebestuur.tabel = reactive(persberichten.beleid.beleid.provinciebestuur$tabel()),
                                              provinciebestuur.uitleg = reactive(persberichten.beleid.beleid.provinciebestuur$uitleg()),
                                              ruimte.plot.aantal = reactive(persberichten.beleid.beleid.ruimte$plot.aantal()),
                                              ruimte.plot.procent = reactive(persberichten.beleid.beleid.ruimte$plot.procent()),
                                              ruimte.tabel = reactive(persberichten.beleid.beleid.ruimte$tabel()),
                                              ruimte.uitleg = reactive(persberichten.beleid.beleid.ruimte$uitleg()),
                                              vrijetijd.plot.aantal = reactive(persberichten.beleid.beleid.vrijetijd$plot.aantal()),
                                              vrijetijd.plot.procent = reactive(persberichten.beleid.beleid.vrijetijd$plot.procent()),
                                              vrijetijd.tabel = reactive(persberichten.beleid.beleid.vrijetijd$tabel()),
                                              vrijetijd.uitleg = reactive(persberichten.beleid.beleid.vrijetijd$uitleg()))
        # Verzender ------------------------------------------------------------
          # Algemeen -----------------------------------------------------------
          Persberichten.verzender.alg <- list(totaal.plot.aantal = reactive(persberichten.verzender.alg.totaal$plot.aantal()),
                                              totaal.plot.procent = reactive(persberichten.verzender.alg.totaal$plot.procent()),
                                              totaal.tabel = reactive(persberichten.verzender.alg.totaal$tabel()),
                                              totaal.uitleg = reactive(persberichten.verzender.alg.totaal$uitleg()),
                                              beleid.plot.aantal = reactive(persberichten.verzender.alg.beleid$plot.aantal()),
                                              beleid.plot.procent = reactive(persberichten.verzender.alg.beleid$plot.procent()),
                                              beleid.tabel = reactive(persberichten.verzender.alg.beleid$plot.aantal()),
                                              beleid.uitleg = reactive(persberichten.verzender.alg.beleid$uitleg()))
          # Per maand ----------------------------------------------------------
          Persberichten.verzender.maand <- list(persdienst.plot.aantal = reactive(persberichten.verzender.maand.persdienst$plot.aantal()),
                                                persdienst.plot.procent = reactive(persberichten.verzender.maand.persdienst$plot.procent()),
                                                persdienst.tabel = reactive(persberichten.verzender.maand.persdienst$tabel()),
                                                persdienst.uitleg = reactive(persberichten.verzender.maand.persdienst$uitleg()),
                                                provincie.plot.aantal = reactive(persberichten.verzender.maand.provincie$plot.aantal()),
                                                provincie.plot.procent= reactive(persberichten.verzender.maand.provincie$plot.procent()),
                                                provincie.tabel = reactive(persberichten.verzender.maand.provincie$tabel()),
                                                provincie.uitleg = reactive(persberichten.verzender.maand.provincie$uitleg()),
                                                gouverneur.plot.aantal = reactive(persberichten.verzender.maand.gouverneur$plot.aantal()),
                                                gouverneur.plot.procent = reactive(persberichten.verzender.maand.gouverneur$plot.procent()),
                                                gouverneur.tabel = reactive(persberichten.verzender.maand.gouverneur$tabel()),
                                                gouverneur.uitleg = reactive(persberichten.verzender.maand.gouverneur$uitleg()),
                                                extern.plot.aantal = reactive(persberichten.verzender.maand.extern$plot.aantal()),
                                                extern.plot.procent = reactive(persberichten.verzender.maand.extern$plot.procent()),
                                                extern.tabel = reactive(persberichten.verzender.maand.extern$tabel()),
                                                extern.uitleg = reactive(persberichten.verzender.maand.extern$uitleg()))
        # Type -----------------------------------------------------------------
        Persberichten.type <- list(type.plot.aantal = reactive(persberichten.type$plot.aantal()),
                                   type.plot.procent = reactive(persberichten.type$plot.procent()),
                                   type.tabel = reactive(persberichten.type$tabel()),
                                   type.uitleg = reactive(persberichten.type$uitleg()))
      # Persreturn -------------------------------------------------------------
        # Beleid ---------------------------------------------------------------
        Persreturn.beleid <- list(algemeen.plot.aantal = reactive(persreturn.beleid.alg$plot.aantal()),
                                  algemeen.plot.procent = reactive(persreturn.beleid.alg$plot.procent()),
                                  algemeen.tabel = reactive(persreturn.beleid.alg$tabel()),
                                  algemeen.uitleg = reactive(persreturn.beleid.alg$uitleg()),
                                  economie.plot.aantal = reactive(persreturn.beleid.beleid.economie$plot.aantal()),
                                  economie.plot.procent = reactive(persreturn.beleid.beleid.economie$plot.procent()),
                                  economie.tabel = reactive(persreturn.beleid.beleid.economie$tabel()),
                                  economie.uitleg = reactive(persreturn.beleid.beleid.economie$uitleg()),
                                  gouverneur.plot.aantal = reactive(persreturn.beleid.beleid.gouverneur$plot.aantal()),
                                  gouverneur.plot.procent = reactive(persreturn.beleid.beleid.gouverneur$plot.procent()),
                                  gouverneur.tabel = reactive(persreturn.beleid.beleid.gouverneur$tabel()),
                                  gouverneur.uitleg = reactive(persreturn.beleid.beleid.gouverneur$uitleg()),
                                  leefmilieu.plot.aantal = reactive(persreturn.beleid.beleid.leefmilieu$plot.aantal()),
                                  leefmilieu.plot.procent = reactive(persreturn.beleid.beleid.leefmilieu$plot.procent()),
                                  leefmilieu.tabel = reactive(persreturn.beleid.beleid.leefmilieu$tabel()),
                                  leefmilieu.uitleg = reactive(persreturn.beleid.beleid.leefmilieu$uitleg()),
                                  mobiliteit.plot.aantal = reactive(persreturn.beleid.beleid.mobiliteit$plot.aantal()),
                                  mobiliteit.plot.procent = reactive(persreturn.beleid.beleid.mobiliteit$plot.procent()),
                                  mobiliteit.tabel = reactive(persreturn.beleid.beleid.mobiliteit$tabel()),
                                  mobiliteit.uitleg = reactive(persreturn.beleid.beleid.mobiliteit$uitleg()),
                                  onderwijs.plot.aantal = reactive(persreturn.beleid.beleid.onderwijs$plot.aantal()),
                                  onderwijs.plot.procent = reactive(persreturn.beleid.beleid.onderwijs$plot.procent()),
                                  onderwijs.tabel = reactive(persreturn.beleid.beleid.onderwijs$tabel()),
                                  onderwijs.uitleg = reactive(persreturn.beleid.beleid.onderwijs$uitleg()),
                                  provinciebestuur.plot.aantal = reactive(persreturn.beleid.beleid.provinciebestuur$plot.aantal()),
                                  provinciebestuur.plot.procent = reactive(persreturn.beleid.beleid.provinciebestuur$plot.procent()),
                                  provinciebestuur.tabel = reactive(persreturn.beleid.beleid.provinciebestuur$tabel()),
                                  provinciebestuur.uitleg = reactive(persreturn.beleid.beleid.provinciebestuur$uitleg()),
                                  ruimte.plot.aantal = reactive(persreturn.beleid.beleid.ruimte$plot.aantal()),
                                  ruimte.plot.procent = reactive(persreturn.beleid.beleid.ruimte$plot.procent()),
                                  ruimte.tabel = reactive(persreturn.beleid.beleid.ruimte$tabel()),
                                  ruimte.uitleg = reactive(persreturn.beleid.beleid.ruimte$uitleg()),
                                  vrijetijd.plot.aantal = reactive(persreturn.beleid.beleid.vrijetijd$plot.aantal()),
                                  vrijetijd.plot.procent = reactive(persreturn.beleid.beleid.vrijetijd$plot.procent()),
                                  vrijetijd.tabel = reactive(persreturn.beleid.beleid.vrijetijd$tabel()),
                                  vrijetijd.uitleg = reactive(persreturn.beleid.beleid.vrijetijd$uitleg()))
        # Medium ---------------------------------------------------------------
        Persreturn.medium <- list(medium.plot.aantal = reactive(persreturn.medium$plot.aantal()),
                                  medium.plot.procent = reactive(persreturn.medium$plot.procent()),
                                  medium.tabel = reactive(persreturn.medium$tabel()),
                                  medium.uitleg = reactive(persreturn.medium$uitleg()))
  
    
    # Set up parameters to pass to Rmd document --------------------------------
      params <- list(titel = paste("Persstatistiek:", paste(input$jaar, input$kwartaal, sep= " - ")),
                     Persberichten.alg = Persberichten.alg,
                     Persconferenties.alg = Persconferenties.alg,
                     Persberichten.beleid.maand = Persberichten.beleid.maand,
                     Persberichten.beleid.beleid = Persberichten.beleid.beleid,
                     Persberichten.verzender.alg = Persberichten.verzender.alg,
                     Persberichten.verzender.maand = Persberichten.verzender.maand,
                     Persberichten.type = Persberichten.type,
                     Persreturn.beleid = Persreturn.beleid,
                     Persreturn.medium = Persreturn.medium,
                     rendered_by_shiny = TRUE)
    
    
    # Knit HTML document -------------------------------------------------------  
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      req(input$file$datapath)
      withProgress(
        message = 'Rapport samenstellen',
        min = 0,
        max = 34,
        value = 0,
        { rmarkdown::render(tempReport, 
                            output_file = file,
                            params = params,
                            envir = new.env(parent = globalenv())
        
                     )
        }
      )
    }
  )
  # ============================================================================
}