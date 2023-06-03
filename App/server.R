###############################################################################
# SHINY APP (Persstatistiek): SERVER
###############################################################################

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
    # Inlezen + Corrigeren -----------------------------------------------------
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
    # Render Table -------------------------------------------------------------
    output$PersstatistiekFull <- DT::renderDataTable({
      PersstatistiekFull()
    })
  # MAIN data: 1 YEAR ----------------------------------------------------------
    # Update selectie mogelijkheden --------------------------------------------
    observe({
      updateSelectInput(
        session = session,
        inputId = 'jaar',
        label = "Jaar", 
        choices = levels(PersstatistiekFull()$Jaar),
        selected = max(levels(PersstatistiekFull()$Jaar))
      )
    })
    observe({
      updateSelectInput(
        session = session,
        inputId = 'kwartaal',
        label = "Selecteer kwartaal:",
        choices = c(levels(as.factor(split.data.frame(PersstatistiekFull(), PersstatistiekFull()$Jaar)[[input$jaar]][["Kwartaal"]])), 'Jaar'),   #c("Q1", "Q2", "Q3", "Q4", "Jaar"),
        selected = "Jaar"
      )
    })

    # Inlezen + Corrigeren -----------------------------------------------------
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
    # Render Table -------------------------------------------------------------
    output$Persstatistiek <- DT::renderDataTable({
      Persstatistiek()
    })
# ==============================================================================

# GESELECTEERD JAAR ============================================================  
  # PERSBERICHTEN ==============================================================
    # ALGEMEEN -----------------------------------------------------------------
      # Persberichten ----------------------------------------------------------
        # Per Kwartaal ---------------------------------------------------------
          persberichten.alg.kwartaal <- callModule(DataVisual.PbBerichtAlgKwartaal, "bericht.alg.kwartaal", data = Persstatistiek, colours = colours)      
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
            persberichten.alg.maand <- callModule(DataVisual.PbBerichtAlgMaand, "bericht.alg.maand", data = Persstatistiek, colours = colours)
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
        # Per WeekDag --------------------------------------------------------------
            persberichten.alg.dag <- callModule(DataVisual.PbBerichtAlgDag, "bericht.alg.dag", data = Persstatistiek, colours = colours)
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
            persberichten.alg.week <- callModule(DataVisual.PbBerichtAlgWeek, "bericht.alg.week", data = Persstatistiek, colours = colours)
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
            persberichten.alg.beleid <- callModule(DataVisual.PbBerichtAlgBeleid, "bericht.alg.beleid", data = Persstatistiek, colours = colours)
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
            persconferenties.alg.kwartaal <- callModule(DataVisual.PbConferentieAlgKwartaal, "conferentie.alg.kwartaal", data = Persstatistiek, colours = colours)      
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
            persconferenties.alg.maand <- callModule(DataVisual.PbConferentieAlgMaand, "conferentie.alg.maand", data = Persstatistiek, colours = colours)
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
            persconferenties.alg.dag <- callModule(DataVisual.PbConferentieAlgDag, "conferentie.alg.dag", data = Persstatistiek, colours = colours)
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
            persconferenties.alg.week <- callModule(DataVisual.PbConferentieAlgWeek, "conferentie.alg.week", data = Persstatistiek, colours = colours)
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
            persconferenties.alg.beleid <- callModule(DataVisual.PbConferentieAlgBeleid, "conferentie.alg.beleid", data = Persstatistiek, colours = colours)
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
        persberichten.beleid.maand.economie <- callModule(DataVisual.PbBeleidMaand, "bericht.beleid.maand.plot.aantal.economie", data = Persstatistiek, colours = colours, beleid = "Economie")
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
            persberichten.beleid.maand.economie$tabel(),
            rownames = FALSE
          )
        # Gouverneur -----------------------------------------------------------
        persberichten.beleid.maand.gouverneur <- callModule(DataVisual.PbBeleidMaand, "bericht.beleid.maand.plot.aantal.gouverneur", data = Persstatistiek, colours = colours, beleid = "Gouverneur")
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
            persberichten.beleid.maand.gouverneur$tabel(),
            rownames = FALSE
          )
          
        # Leefmilieu -----------------------------------------------------------
        persberichten.beleid.maand.leefmilieu <- callModule(DataVisual.PbBeleidMaand, "bericht.beleid.maand.plot.aantal.leefmilieu", data = Persstatistiek, colours = colours, beleid = "Leefmilieu")
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
            persberichten.beleid.maand.leefmilieu$tabel(),
            rownames = FALSE
          )
        # Mobiliteit -----------------------------------------------------------
        persberichten.beleid.maand.mobiliteit <- callModule(DataVisual.PbBeleidMaand, "bericht.beleid.maand.plot.aantal.mobiliteit", data = Persstatistiek, colours = colours, beleid = "Mobiliteit")
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
            persberichten.beleid.maand.mobiliteit$tabel(),
            rownames = FALSE
          )
        # Onderwijs en Educatie-------------------------------------------------
        persberichten.beleid.maand.onderwijs <- callModule(DataVisual.PbBeleidMaand, "bericht.beleid.maand.plot.aantal.onderwijs", data = Persstatistiek, colours = colours, beleid = "Onderwijs en Educatie")
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
            persberichten.beleid.maand.onderwijs$tabel(),
            rownames = FALSE
          )
        # Provinciebestuur -----------------------------------------------------
        persberichten.beleid.maand.provinciebestuur <- callModule(DataVisual.PbBeleidMaand, "bericht.beleid.maand.plot.aantal.provinciebestuur", data = Persstatistiek, colours = colours, beleid = "Provinciebestuur")
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
            persberichten.beleid.maand.provinciebestuur$tabel(),
            rownames = FALSE
          )
        # Ruimte ---------------------------------------------------------------
        persberichten.beleid.maand.ruimte <- callModule(DataVisual.PbBeleidMaand, "bericht.beleid.maand.plot.aantal.ruimte", data = Persstatistiek, colours = colours, beleid = "Ruimte")
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
            persberichten.beleid.maand.ruimte$tabel(),
            rownames = FALSE
          )
        # Vrije Tijd -----------------------------------------------------------
        persberichten.beleid.maand.vrijetijd <- callModule(DataVisual.PbBeleidMaand, "bericht.beleid.maand.plot.aantal.vrijetijd", data = Persstatistiek, colours = colours, beleid = "Vrije Tijd")
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
            persberichten.beleid.maand.vrijetijd$tabel(),
            rownames = FALSE
          )

      # Per Deelbeleid ---------------------------------------------------------
        # Economie -------------------------------------------------------------
        persberichten.beleid.beleid.economie <- callModule(DataVisual.PbDeelbeleid, "bericht.beleid.beleid.plot.aantal.economie", Persstatistiek, colours = colours, beleid = "Economie", datadeelbeleid = reactive(input$Economie.ActieveDeelbeleiden))
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
        persberichten.beleid.beleid.gouverneur <- callModule(DataVisual.PbDeelbeleid, "bericht.beleid.beleid.plot.aantal.gouverneur", Persstatistiek, colours = colours, beleid = "Gouverneur", datadeelbeleid = reactive(input$Gouverneur.ActieveDeelbeleiden))
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
        persberichten.beleid.beleid.leefmilieu <- callModule(DataVisual.PbDeelbeleid, "bericht.beleid.beleid.plot.aantal.leefmilieu", Persstatistiek, colours = colours, beleid = "Leefmilieu", datadeelbeleid = reactive(input$Leefmilieu.ActieveDeelbeleiden))
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
        persberichten.beleid.beleid.mobiliteit <- callModule(DataVisual.PbDeelbeleid, "bericht.beleid.beleid.plot.aantal.mobiliteit", Persstatistiek, colours = colours, beleid = "Mobiliteit", datadeelbeleid = reactive(input$Mobiliteit.ActieveDeelbeleiden))
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
        persberichten.beleid.beleid.onderwijs <- callModule(DataVisual.PbDeelbeleid, "bericht.beleid.beleid.plot.aantal.onderwijs", Persstatistiek, colours = colours, beleid = "Onderwijs en Educatie", datadeelbeleid = reactive(input$Onderwijs.ActieveDeelbeleiden))
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
        persberichten.beleid.beleid.provinciebestuur <- callModule(DataVisual.PbDeelbeleid, "bericht.beleid.beleid.plot.aantal.provinciebestuur", Persstatistiek, colours = colours, beleid = "Provinciebestuur", datadeelbeleid = reactive(input$Provinciebestuur.ActieveDeelbeleiden))
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
        persberichten.beleid.beleid.ruimte <- callModule(DataVisual.PbDeelbeleid, "bericht.beleid.beleid.plot.aantal.ruimte", Persstatistiek, colours = colours, beleid = "Ruimte", datadeelbeleid = reactive(input$Ruimte.ActieveDeelbeleiden))
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
        persberichten.beleid.beleid.vrijetijd <- callModule(DataVisual.PbDeelbeleid, "bericht.beleid.beleid.plot.aantal.vrijetijd", Persstatistiek, colours = colours, beleid = "Vrije Tijd", datadeelbeleid = reactive(input$VrijeTijd.ActieveDeelbeleiden))
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
          persberichten.verzender.alg.totaal <- callModule(DataVisual.PbVerzenderVerzender, "bericht.verzender.alg.totaal", Persstatistiek, colours = colours)
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
          persberichten.verzender.alg.beleid <- callModule(DataVisual.PbVerzenderBeleid, "bericht.verzender.alg.beleid", Persstatistiek, colours = colours, datadeelbeleid=reactive(AlleDeelbeleiden))
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
          persberichten.verzender.maand.persdienst <- callModule(DataVisual.PbVerzenderMaand, "bericht.verzender.maand.persdienst", Persstatistiek, colours = colours, verzender = "Persdienst")
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
            persberichten.verzender.maand.persdienst$tabel(),
            rownames = FALSE
          )
        # Provincie ------------------------------------------------------------
          persberichten.verzender.maand.provincie <- callModule(DataVisual.PbVerzenderMaand, "bericht.verzender.maand.provincie", Persstatistiek, colours = colours, verzender = "Provincie")
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
            persberichten.verzender.maand.provincie$tabel(),
            rownames = FALSE
          )
        # Gouverneur -----------------------------------------------------------
          persberichten.verzender.maand.gouverneur <- callModule(DataVisual.PbVerzenderMaand, "bericht.verzender.maand.gouverneur", Persstatistiek, colours = colours, verzender = "Gouverneur")
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
            persberichten.verzender.maand.gouverneur$tabel(),
            rownames = FALSE
          )
        # Extern ---------------------------------------------------------------
          persberichten.verzender.maand.extern <- callModule(DataVisual.PbVerzenderMaand, "bericht.verzender.maand.extern", Persstatistiek, colours = colours, verzender = "Extern")
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
            persberichten.verzender.maand.extern$tabel(),
            rownames = FALSE
          )
      # Per Deelbeleid ---------------------------------------------------------
        # Economie -------------------------------------------------------------
          persberichten.verzender.beleid.economie <- callModule(DataVisual.PbVerzenderDeelbeleid, "bericht.verzender.beleid.plot.aantal.economie", Persstatistiek, colours = colours, beleid = "Economie", datadeelbeleid = reactive(input$Economie.ActieveDeelbeleiden))
          # Plot - aantal
          output$persberichten.verzender.beleid.economie.plot.aantal <- renderPlot(
            persberichten.verzender.beleid.economie$plot.aantal()
          )
          # Plot - procent
          output$persberichten.verzender.beleid.economie.plot.procent <- renderPlot(
            persberichten.verzender.beleid.economie$plot.procent()
          )
          # Table
          output$persberichten.verzender.beleid.economie.tabel <- DT::renderDataTable(
            persberichten.verzender.beleid.economie$tabel()
          )
        # Gouverneur -----------------------------------------------------------
          persberichten.verzender.beleid.gouverneur <- callModule(DataVisual.PbVerzenderDeelbeleid, "bericht.verzender.beleid.plot.aantal.gouverneur", Persstatistiek, colours = colours, beleid = "Gouverneur", datadeelbeleid = reactive(input$Gouverneur.ActieveDeelbeleiden))
          # Plot  -aantal
          output$persberichten.verzender.beleid.gouverneur.plot.aantal <- renderPlot(
            persberichten.verzender.beleid.gouverneur$plot.aantal()
          )
          # Plot  - procent
          output$persberichten.verzender.beleid.gouverneur.plot.procent <- renderPlot(
            persberichten.verzender.beleid.gouverneur$plot.procent()
          )
          # Table
          output$persberichten.verzender.beleid.gouverneur.tabel <- DT::renderDataTable(
            persberichten.verzender.beleid.gouverneur$tabel()
          )
        # Leefmilieu -----------------------------------------------------------
          persberichten.verzender.beleid.leefmilieu <- callModule(DataVisual.PbVerzenderDeelbeleid, "bericht.verzender.beleid.plot.aantal.leefmilieu", Persstatistiek, colours = colours, beleid = "Leefmilieu", datadeelbeleid = reactive(input$Leefmilieu.ActieveDeelbeleiden))
          # Plot -aantal
          output$persberichten.verzender.beleid.leefmilieu.plot.aantal <- renderPlot(
            persberichten.verzender.beleid.leefmilieu$plot.aantal()
          )
          # Plot - procent
          output$persberichten.verzender.beleid.leefmilieu.plot.procent <- renderPlot(
            persberichten.verzender.beleid.leefmilieu$plot.procent()
          )
          # Table
          output$persberichten.verzender.beleid.leefmilieu.tabel <- DT::renderDataTable(
            persberichten.verzender.beleid.leefmilieu$tabel()
          )
        # Mobiliteit -----------------------------------------------------------
          persberichten.verzender.beleid.mobiliteit <- callModule(DataVisual.PbVerzenderDeelbeleid, "bericht.verzender.beleid.plot.aantal.mobiliteit", Persstatistiek, colours = colours, beleid = "Mobiliteit", datadeelbeleid = reactive(input$Mobiliteit.ActieveDeelbeleiden))
          # Plot - aantal 
          output$persberichten.verzender.beleid.mobiliteit.plot.aantal <- renderPlot(
            persberichten.verzender.beleid.mobiliteit$plot.aantal()
          )
          # Plot  - procent
          output$persberichten.verzender.beleid.mobiliteit.plot.procent <- renderPlot(
            persberichten.verzender.beleid.mobiliteit$plot.procent()
          )
          # Table
          output$persberichten.verzender.beleid.mobiliteit.tabel <- DT::renderDataTable(
            persberichten.verzender.beleid.mobiliteit$tabel()
          )
        # Onderwijs en Educatie ------------------------------------------------
          persberichten.verzender.beleid.onderwijs <- callModule(DataVisual.PbVerzenderDeelbeleid, "bericht.verzender.beleid.plot.aantal.onderwijs", Persstatistiek, colours = colours, beleid = "Onderwijs en Educatie", datadeelbeleid = reactive(input$Onderwijs.ActieveDeelbeleiden))
          # Plot -aantal 
          output$persberichten.verzender.beleid.onderwijs.plot.aantal <- renderPlot(
            persberichten.verzender.beleid.onderwijs$plot.aantal()
          )
          # Plot  - procent
          output$persberichten.verzender.beleid.onderwijs.plot.procent <- renderPlot(
            persberichten.verzender.beleid.onderwijs$plot.procent()
          )
          # Tabel
          output$persberichten.verzender.beleid.onderwijs.tabel <- DT::renderDataTable(
            persberichten.verzender.beleid.onderwijs$tabel()
          )
        # Provinciebestuur -----------------------------------------------------
          persberichten.verzender.beleid.provinciebestuur <- callModule(DataVisual.PbVerzenderDeelbeleid, "bericht.verzender.beleid.plot.aantal.provinciebestuur", Persstatistiek, colours = colours, beleid = "Provinciebestuur", datadeelbeleid = reactive(input$Provinciebestuur.ActieveDeelbeleiden))
          # Plot  - aantal
          output$persberichten.verzender.beleid.provinciebestuur.plot.aantal <- renderPlot(
            persberichten.verzender.beleid.provinciebestuur$plot.aantal()
          )
          # Plot  - procent
          output$persberichten.verzender.beleid.provinciebestuur.plot.procent <- renderPlot(
            ppersberichten.verzender.beleid.provinciebestuur$plot.procent()
          )
          # Table
          output$persberichten.verzender.beleid.provinciebestuur.tabel <- DT::renderDataTable(
            persberichten.verzender.beleid.provinciebestuur$tabel()
          )
        # Ruimte ---------------------------------------------------------------
          persberichten.verzender.beleid.ruimte <- callModule(DataVisual.PbVerzenderDeelbeleid, "bericht.verzender.beleid.plot.aantal.ruimte", Persstatistiek, colours = colours, beleid = "Ruimte", datadeelbeleid = reactive(input$Ruimte.ActieveDeelbeleiden))
          # Plot  - aantal
          output$persberichten.verzender.beleid.ruimte.plot.aantal <- renderPlot(
            persberichten.verzender.beleid.ruimte$plot.aantal()
          )
          # Plot  - procent
          output$persberichten.verzender.beleid.ruimte.plot.procent <- renderPlot(
            persberichten.verzender.beleid.ruimte$plot.procent()
          )
          # Table
          output$persberichten.verzender.beleid.ruimte.tabel <- DT::renderDataTable(
            persberichten.verzender.beleid.ruimte$tabel()
          )
        # Vrije Tijd -----------------------------------------------------------
          persberichten.verzender.beleid.vrijetijd <- callModule(DataVisual.PbVerzenderDeelbeleid, "bericht.verzender.beleid.plot.aantal.vrijetijd", Persstatistiek, colours = colours, beleid = "Vrije Tijd", datadeelbeleid = reactive(input$VrijeTijd.ActieveDeelbeleiden))
          # Plot  - aantal
          output$persberichten.verzender.beleid.vrijetijd.plot.aantal <- renderPlot(
            persberichten.verzender.beleid.vrijetijd$plot.aantal()
          )
          # Plot  - procent
          output$persberichten.verzender.beleid.vrijetijd.plot.procent <- renderPlot(
            persberichten.verzender.beleid.vrijetijdd$plot.procent()
          )
          # Table
          output$persberichten.verzender.beleid.vrijetijd.tabel <- DT::renderDataTable(
            persberichten.verzender.beleid.vrijetijd$tabel()
          )
    # PER TYPE -----------------------------------------------------------------
      persberichten.type <- callModule(DataVisual.PbType, "bericht.type", Persstatistiek, colours = colours, datadeelbeleid = reactive(AlleDeelbeleiden))
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
      persreturn.beleid.alg <- callModule(DataVisual.PrBeleidAlg, "return.beleid.alg", Persstatistiek, colours = return.colours)
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
          persreturn.beleid.alg$tabel(),
          rownames = FALSE
        )
      # Deelbeleid -------------------------------------------------------------
        # Economie -------------------------------------------------------------
        persreturn.beleid.beleid.economie <- callModule(DataVisual.PrDeelbeleid, "return.beleid.beleid.economie", Persstatistiek, colours = return.colours, beleid = "Economie", datadeelbeleid = reactive(input$Economie.ActieveDeelbeleiden))
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
            persreturn.beleid.beleid.economie$tabel(),
            rownames = FALSE
          )
        # Gouverneur -----------------------------------------------------------
        persreturn.beleid.beleid.gouverneur <- callModule(DataVisual.PrDeelbeleid, "return.beleid.beleid.gouverneur", Persstatistiek, colours = return.colours, beleid = "Gouverneur", datadeelbeleid =reactive(input$Gouverneur.ActieveDeelbeleiden))
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
            persreturn.beleid.beleid.gouverneur$tabel(),
            rownames = FALSE
          )
        # Leefmilieu -----------------------------------------------------------
        persreturn.beleid.beleid.leefmilieu <- callModule(DataVisual.PrDeelbeleid, "return.beleid.beleid.leefmilieu", Persstatistiek, colours = return.colours, beleid = "Leefmilieu", datadeelbeleid = reactive(input$Leefmilieu.ActieveDeelbeleiden))
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
            persreturn.beleid.beleid.leefmilieu$tabel(),
            rownames = FALSE
          )
        # Mobiliteit -----------------------------------------------------------
        persreturn.beleid.beleid.mobiliteit <- callModule(DataVisual.PrDeelbeleid, "return.beleid.beleid.mobiliteit", Persstatistiek, colours = return.colours, beleid = "Mobiliteit", datadeelbeleid=reactive(input$Mobiliteit.ActieveDeelbeleiden))
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
            persreturn.beleid.beleid.mobiliteit$tabel(),
            rownames = FALSE
          )
        # Onderwijs en Educatie ------------------------------------------------
        persreturn.beleid.beleid.onderwijs <- callModule(DataVisual.PrDeelbeleid, "return.beleid.beleid.onderwijs", Persstatistiek, colours = return.colours, beleid = "Onderwijs en Educatie", datadeelbeleid = reactive(input$Onderwijs.ActieveDeelbeleiden))
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
            persreturn.beleid.beleid.onderwijs$tabel(),
            rownames = FALSE
          )
        # Provinciebestuur -----------------------------------------------------
        persreturn.beleid.beleid.provinciebestuur <- callModule(DataVisual.PrDeelbeleid, "return.beleid.beleid.provinciebestuur", Persstatistiek, colours = return.colours, beleid = "Provinciebestuur", datadeelbeleid = reactive(input$Provinciebestuur.ActieveDeelbeleiden))
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
            persreturn.beleid.beleid.provinciebestuur$tabel(),
            rownames = FALSE
          )
        # Ruimte ---------------------------------------------------------------
        persreturn.beleid.beleid.ruimte <- callModule(DataVisual.PrDeelbeleid, "return.beleid.beleid.ruimte", Persstatistiek, colours = return.colours, beleid = "Ruimte", datadeelbeleid = reactive(input$Ruimte.ActieveDeelbeleiden))
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
            persreturn.beleid.beleid.ruimte$tabel(),
            rownames = FALSE
          )
        # Vrije Tijd -----------------------------------------------------------
        persreturn.beleid.beleid.vrijetijd <- callModule(DataVisual.PrDeelbeleid, "return.beleid.beleid.vrijetijd", Persstatistiek, colours = return.colours, beleid = "Vrije Tijd", datadeelbeleid = reactive(input$VrijeTijd.ActieveDeelbeleiden))
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
            persreturn.beleid.beleid.vrijetijd$tabel(),
            rownames = FALSE
          )
    # PER MEDIUM ---------------------------------------------------------------
      persreturn.medium <- callModule(DataVisual.PrMedium, "return.medium" , Persstatistiek, Xaxis = "Beleid", Fill = "Medium", colours = colours)
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
# ==============================================================================
     
# STATISTIEK ALLE JAREN ========================================================
# observe({
#   req(input$kwartaal)
#   allejaren <- data.frame('Q1' = numeric(), 'Q2'=numeric(), 'Q3'=numeric(), 'Q4'=numeric())
#   tempallejaren <- data.frame()
#   namesallejaren <- c()
#   for (i in levels(as.factor(PersstatistiekFull()$Jaar))) {
#     tempallejaren <- split.data.frame(PersstatistiekFull(), PersstatistiekFull()$Jaar)[[i]]
#     if(!(grep('Jaar',input$kwartaal))) {
#       tempallejaren <- data.frame(table(tempallejaren[[input$kwartaal]]))
#     }
#     tempallejaren <- data.frame(table(tempallejaren[["Kwartaal"]]))
#     browser()
#     namesallejaren <- c(namesallejaren, i)
#     rbind(allejaren, tempallejaren)
#     browser()
#   }
#   dfallejaren <- t(allejaren)                     # transpose
#   rownames(dfallejaren) <- namesallejaren # add colnames
# 
#   output$dfallejaren <- DT::renderDataTable(dfallejaren)
#   browser()
# })

# ==============================================================================

   
# FILE AANMAAK =================================================================
# Settings ---------------------------------------------------------------------
  report <- reactiveValues(filepath = NULL) #This creates a short-term storage location for a filepath
      
  observeEvent(input$generate, {
  # Set up parameters to pass to Rmd document --------------------------------
  params <-
    list(
      titel = paste("Persstatistiek:", paste(input$jaar, input$kwartaal, sep = " - ")),
      # TOCDepth = input$TOCDepth,
      ReportSections_Berichten = input$ReportSections_Berichten,
      ReportSections_Return = input$ReportSections_Return,
      Persberichten.alg = Persberichten.alg,
      Persconferenties.alg = Persconferenties.alg,
      Persberichten.beleid.maand = Persberichten.beleid.maand,
      Persberichten.beleid.beleid = Persberichten.beleid.beleid,
      Persberichten.verzender.alg = Persberichten.verzender.alg,
      Persberichten.verzender.maand = Persberichten.verzender.maand,
      Persberichten.verzender.beleid = Persberichten.verzender.beleid,
      Persberichten.type = Persberichten.type,
      Persreturn.beleid = Persreturn.beleid,
      Persreturn.medium = Persreturn.medium,
      rendered_by_shiny = TRUE
    )
    # --------------------------------------------------------------------------
  
    # Render document ----------------------------------------------------------
  tempReport <- file.path(tempdir(), "Reports/JaarVolledig.Rmd")
    tmp_file <- paste0(tempfile(), ".html") #Creating the temp where the .html is going to be stored
  
    withProgress(
      message = 'Rapport samenstellen',
      min = 0,
      max = 60,
      value = 0,
      { rmarkdown::render(tempReport, 
                          output_file = tmp_file,
                          params = params,
                          envir = new.env(parent = globalenv())
                          
      )
      }
    )
  # ----------------------------------------------------------------------------
    
    report$filepath <- tmp_file #Assigning in the temp file where the .pdf is located to the reactive file created above
    
  }
)      
      
# ==============================================================================
            
# FILE DOWNLOAD ================================================================
# Hide download button until report is generated -------------------------------
  output$reportbuilt <- reactive({
    return(!is.null(report$filepath))
  })
  outputOptions(output, 'reportbuilt', suspendWhenHidden= FALSE)  
  
# Download report --------------------------------------------------------------
  output$download <- downloadHandler(
    
    # This function returns a string which tells the client
    # browser what name to use when saving the file.
    filename = reactive(paste0("Persstatistiek_", paste0(input$jaar, paste0("_", paste0(input$kwartaal, ".html"))))),
    
    # This function should write data to a file given to it by
    # the argument 'file'.
    content = function(file) {
      
      file.copy(report$filepath, file)
      
    }
  )  
# ==============================================================================
  
}