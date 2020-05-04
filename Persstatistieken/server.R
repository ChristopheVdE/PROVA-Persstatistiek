###############################################################################
# SHINY APP (Persstatistiek): SERVER
###############################################################################

# PACKAGES ====================================================================
library(knitr)
library(RColorBrewer)
library(ggplot2)
# =============================================================================

server <- function(input, output) {
  
  # INPUT PROCESSING ==========================================================
  # Colours -------------------------------------------------------------------
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
  # Data Preparation ----------------------------------------------------------
  source("./Modules/Functions/data_preparation.R")
    Persstatistiek <- data.preparation(file = reactive(input$file$datapath),
                                       sheet = reactive(input$sheet),
                                       headers = reactive(input$headers),
                                       manual.headers = c(reactive(input$col.beleid), 
                                                          reactive(input$col.detail), 
                                                          reactive(input$col.kwartaal), 
                                                          reactive(input$col.verzender), 
                                                          reactive(input$col.type), 
                                                          reactive(input$col.return.algemeen), 
                                                          reactive(input$col.return.web), 
                                                          reactive(input$col.return.tv), 
                                                          reactive(input$col.maand)),
                                       kwartaal = reactive(input$kwartaal))
  # Render Original Table ---------------------------------------------------
  output$table <- renderTable({
    Persstatistiek()
  })
  # ===========================================================================
  
  # PERSBERICHTEN =============================================================
    source("./Modules/data_visualisation.R")
    # ALGEMEEN ----------------------------------------------------------------
      # Per Kwartaal ----------------------------------------------------------
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
          output$persberichten.alg.kwartaal.tabel <- renderTable(
            persberichten.alg.kwartaal$tabel()
          )
      # Per Maand -------------------------------------------------------------
        # Plot - aantal
          persberichten.alg.maand <- callModule(data.visual, "bericht.alg.maand", Id = "alg.maand", data = Persstatistiek, Xaxis = "Maand", Fill = "Maand", colours = colours)  
          output$persberichten.alg.maand.plot.aantal <- renderPlot(
            persberichten.alg.maand$plot.aantal()
          )
          # Plot - procent
          persberichten.alg.maand <- callModule(data.visual, "bericht.alg.maand", Id = "alg.maand", data = Persstatistiek, Xaxis = "Maand", Fill = "Maand", colours = colours)  
          output$persberichten.alg.maand.plot.procent <- renderPlot(
            persberichten.alg.maand$plot.procent()
          )
        # Tabel
          output$persberichten.alg.maand.tabel <- renderTable(
            persberichten.alg.maand$tabel()
          )
      # Per Beleid ------------------------------------------------------------
        # Plot - aantal
          persberichten.alg.beleid <- callModule(data.visual, "bericht.alg.beleid", Id = "alg.beleid", data = Persstatistiek, Xaxis = "Beleid", Fill = "Beleid", colours = colours)   
          output$persberichten.alg.beleid.plot.aantal <- renderPlot(
            persberichten.alg.beleid$plot.aantal()
          )
        # Plot
          persberichten.alg.beleid <- callModule(data.visual, "bericht.alg.beleid", Id = "alg.beleid", data = Persstatistiek, Xaxis = "Beleid", Fill = "Beleid", colours = colours)   
          output$persberichten.alg.beleid.plot.procent <- renderPlot(
            persberichten.alg.beleid$plot.procent()
          )
        # Tabel
          output$persberichten.alg.beleid.tabel <- renderTable(
            persberichten.alg.beleid$tabel()
          )
    # PER BELEID --------------------------------------------------------------
      # Per Maand -------------------------------------------------------------
        # Economie ------------------------------------------------------------
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
          output$persberichten.beleid.maand.economie.tabel <- renderTable(
            persberichten.beleid.maand.economie$tabel()
          )
        # Gouverneur ----------------------------------------------------------
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
          output$persberichten.beleid.maand.gouverneur.tabel <- renderTable(
            persberichten.beleid.maand.gouverneur$tabel()
          )
          
        # Leefmilieu ----------------------------------------------------------
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
          output$persberichten.beleid.maand.leefmilieu.tabel <- renderTable(
            persberichten.beleid.maand.leefmilieu$tabel()
          )
        # Mobiliteit ----------------------------------------------------------
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
          output$persberichten.beleid.maand.mobiliteit.tabel <- renderTable(
            persberichten.beleid.maand.mobiliteit$tabel()
          )
        # Onderwijs en Educatie -----------------------------------------------
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
          output$persberichten.beleid.maand.onderwijs.tabel <- renderTable(
            persberichten.beleid.maand.onderwijs$tabel()
          )
        # Provinciebestuur ----------------------------------------------------
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
          output$persberichten.beleid.maand.provinciebestuur.tabel <- renderTable(
            persberichten.beleid.maand.provinciebestuur$tabel()
          )
        # Ruimte --------------------------------------------------------------
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
          output$persberichten.beleid.maand.ruimte.tabel <- renderTable(
            persberichten.beleid.maand.ruimte$tabel()
          )
        # Vrije Tijd ----------------------------------------------------------
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
          output$persberichten.beleid.maand.vrijetijd.tabel <- renderTable(
            persberichten.beleid.maand.vrijetijd$tabel()
          )

      # Per Deelbeleid --------------------------------------------------------
        # Economie ------------------------------------------------------------
        persberichten.beleid.beleid.economie <- callModule(data.visual, "bericht.beleid.beleid.plot.aantal.economie", Id = "beleid.beleid" , Persstatistiek, Xaxis = "Deelbeleid", Fill = "Deelbeleid", colours = colours, beleid = "Economie")
          # Plot - aantal
          output$persberichten.beleid.beleid.economie.plot.aantal <- renderPlot(
            persberichten.beleid.beleid.economie$plot.aantal()
          )
          # Plot - procent
          output$persberichten.beleid.beleid.economie.plot.procent <- renderPlot(
            persberichten.beleid.beleid.economie$plot.procent()
          )
          # Table
          output$persberichten.beleid.beleid.economie.tabel <- renderTable(
            persberichten.beleid.beleid.economie$tabel()
          )
        # Gouverneur ----------------------------------------------------------
        persberichten.beleid.beleid.gouverneur <- callModule(data.visual, "bericht.beleid.beleid.plot.aantal.gouverneur", Id = "beleid.beleid" , Persstatistiek, Xaxis = "Deelbeleid", Fill = "Deelbeleid", colours = colours, beleid = "Gouverneur")
          # Plot  -aantal
          output$persberichten.beleid.beleid.gouverneur.plot.aantal <- renderPlot(
            persberichten.beleid.beleid.gouverneur$plot.aantal()
          )
          # Plot  - procent
          output$persberichten.beleid.beleid.gouverneur.plot.procent <- renderPlot(
            persberichten.beleid.beleid.gouverneur$plot.procent()
          )
          # Table
          output$persberichten.beleid.beleid.gouverneur.tabel <- renderTable(
            persberichten.beleid.beleid.gouverneur$tabel()
          )
        # Leefmilieu ----------------------------------------------------------
        persberichten.beleid.beleid.leefmilieu <- callModule(data.visual, "bericht.beleid.beleid.plot.aantal.leefmilieu", Id = "beleid.beleid" , Persstatistiek, Xaxis = "Deelbeleid", Fill = "Deelbeleid", colours = colours, beleid = "Leefmilieu")
          # Plot -aantal
          output$persberichten.beleid.beleid.leefmilieu.plot.aantal <- renderPlot(
            persberichten.beleid.beleid.leefmilieu$plot.aantal()
          )
          # Plot - procent
          output$persberichten.beleid.beleid.leefmilieu.plot.procent <- renderPlot(
            persberichten.beleid.beleid.leefmilieu$plot.procent()
          )
          # Table
          output$persberichten.beleid.beleid.leefmilieu.tabel <- renderTable(
            persberichten.beleid.beleid.leefmilieu$tabel()
          )
        # Mobiliteit ----------------------------------------------------------
        persberichten.beleid.beleid.mobiliteit <- callModule(data.visual, "bericht.beleid.beleid.plot.aantal.mobiliteit", Id = "beleid.beleid" , Persstatistiek, Xaxis = "Deelbeleid", Fill = "Deelbeleid", colours = colours, beleid = "Mobiliteit")
          # Plot - aantal 
          output$persberichten.beleid.beleid.mobiliteit.plot.aantal <- renderPlot(
            persberichten.beleid.beleid.mobiliteit$plot.aantal()
          )
          # Plot  - procent
          output$persberichten.beleid.beleid.mobiliteit.plot.procent <- renderPlot(
            persberichten.beleid.beleid.mobiliteit$plot.procent()
          )
          # Table
          output$persberichten.beleid.beleid.mobiliteit.tabel <- renderTable(
            persberichten.beleid.beleid.mobiliteit$tabel()
          )
        # Onderwijs en Educatie -----------------------------------------------
        persberichten.beleid.beleid.onderwijs <- callModule(data.visual, "bericht.beleid.beleid.plot.aantal.onderwijs", Id = "beleid.beleid" , Persstatistiek, Xaxis = "Deelbeleid", Fill = "Deelbeleid", colours = colours, beleid = "Onderwijs en Educatie")
          # Plot -aantal 
          output$persberichten.beleid.beleid.onderwijs.plot.aantal <- renderPlot(
            persberichten.beleid.beleid.onderwijs$plot.aantal()
          )
          # Plot  - procent
          output$persberichten.beleid.beleid.onderwijs.plot.procent <- renderPlot(
            persberichten.beleid.beleid.onderwijs$plot.procent()
          )
          # Tabel
          output$persberichten.beleid.beleid.onderwijs.tabel <- renderTable(
            persberichten.beleid.beleid.onderwijs$tabel()
          )
        # Provinciebestuur ----------------------------------------------------
        persberichten.beleid.beleid.provinciebestuur <- callModule(data.visual, "bericht.beleid.beleid.plot.aantal.provinciebestuur", Id = "beleid.beleid" , Persstatistiek, Xaxis = "Deelbeleid", Fill = "Deelbeleid", colours = colours, beleid = "Provinciebestuur")
          # Plot  - aantal
          output$persberichten.beleid.beleid.provinciebestuur.plot.aantal <- renderPlot(
            persberichten.beleid.beleid.provinciebestuur$plot.aantal()
          )
          # Plot  - procent
          output$persberichten.beleid.beleid.provinciebestuur.plot.procent <- renderPlot(
            persberichten.beleid.beleid.provinciebestuur$plot.procent()
          )
          # Table
          output$persberichten.beleid.beleid.provinciebestuur.tabel <- renderTable(
            persberichten.beleid.beleid.provinciebestuur$tabel()
          )
        # Ruimte --------------------------------------------------------------
        persberichten.beleid.beleid.ruimte <- callModule(data.visual, "bericht.beleid.beleid.plot.aantal.ruimte", Id = "beleid.beleid" , Persstatistiek, Xaxis = "Deelbeleid", Fill = "Deelbeleid", colours = colours, beleid = "Ruimte")
          # Plot  - aantal
          output$persberichten.beleid.beleid.ruimte.plot.aantal <- renderPlot(
            persberichten.beleid.beleid.ruimte$plot.aantal()
          )
          # Plot  - procent
          output$persberichten.beleid.beleid.ruimte.plot.procent <- renderPlot(
            persberichten.beleid.beleid.ruimte$plot.procent()
          )
          # Table
          output$persberichten.beleid.beleid.ruimte.tabel <- renderTable(
            persberichten.beleid.beleid.ruimte$tabel()
        )
        # Vrije Tijd ----------------------------------------------------------
        persberichten.beleid.beleid.vrijetijd <- callModule(data.visual, "bericht.beleid.beleid.plot.aantal.vrijetijd", Id = "beleid.beleid" , Persstatistiek, Xaxis = "Deelbeleid", Fill = "Deelbeleid", colours = colours, beleid = "Vrije Tijd")
          # Plot  - aantal
          output$persberichten.beleid.beleid.vrijetijd.plot.aantal <- renderPlot(
            persberichten.beleid.beleid.vrijetijd$plot.aantal()
          )
          # Plot  - procent
          output$persberichten.beleid.beleid.vrijetijd.plot.procent <- renderPlot(
            persberichten.beleid.beleid.vrijetijd$plot.procent()
          )
          # Table
          output$persberichten.beleid.beleid.vrijetijd.tabel <- renderTable(
            persberichten.beleid.beleid.vrijetijd$tabel()
          )
    # PER VERZENDER -----------------------------------------------------------
      # Algemeen --------------------------------------------------------------
        # Totaal per Verzender ------------------------------------------------
          # Plot - aantal
          persberichten.verzender.alg.totaal <- callModule(data.visual, "bericht.verzender.alg.totaal", Id = "verzender.alg.verzender" , Persstatistiek, Xaxis = "Verzender", Fill = "Verzender", colours = colours)
          output$persberichten.verzender.alg.totaal.plot.aantal <- renderPlot(
            persberichten.verzender.alg.totaal$plot.aantal()
          )
          # Plot - procent
          persberichten.verzender.alg.totaal <- callModule(data.visual, "bericht.verzender.alg.totaal", Id = "verzender.alg.verzender" , Persstatistiek, Xaxis = "Verzender", Fill = "Verzender", colours = colours)
          output$persberichten.verzender.alg.totaal.plot.procent <- renderPlot(
            persberichten.verzender.alg.totaal$plot.procent()
          )
          # Tabel
          output$persberichten.verzender.alg.totaal.tabel <- renderTable(
            persberichten.verzender.alg.totaal$tabel()
          )
        # Beleid per Verzender ------------------------------------------------
          # Plot - aantal
          persberichten.verzender.alg.beleid <- callModule(data.visual, "bericht.verzender.alg.beleid", Id = "verzender.alg.beleid" , Persstatistiek, Xaxis = "Verzender", Fill = "Beleid", colours = colours)
          output$persberichten.verzender.alg.beleid.plot.aantal <- renderPlot(
            persberichten.verzender.alg.beleid$plot.aantal()
          )
          # Plot - procent
          persberichten.verzender.alg.beleid <- callModule(data.visual, "bericht.verzender.alg.beleid", Id = "verzender.alg.beleid" , Persstatistiek, Xaxis = "Verzender", Fill = "Beleid", colours = colours)
          output$persberichten.verzender.alg.beleid.plot.procent <- renderPlot(
            persberichten.verzender.alg.beleid$plot.procent()
          )
          # Tabel
          output$persberichten.verzender.alg.beleid.tabel <- renderTable(
            persberichten.verzender.alg.beleid$tabel()
          )
      # Per Maand -------------------------------------------------------------
        # Persdienst ----------------------------------------------------------
          # Plot - aantal
          persberichten.verzender.maand.persdienst <- callModule(data.visual, "bericht.verzender.maand.persdienst", Id = "verzender.maand" , Persstatistiek, Xaxis = "Maand", Fill = "Maand", colours = colours, verzender = "Persdienst")
          output$persberichten.verzender.maand.persdienst.plot.aantal <- renderPlot(
            persberichten.verzender.maand.persdienst$plot.aantal()
          )
          # Plot - procent
          persberichten.verzender.maand.persdienst <- callModule(data.visual, "bericht.verzender.maand.persdienst", Id = "verzender.maand" , Persstatistiek, Xaxis = "Maand", Fill = "Maand", colours = colours, verzender = "Persdienst")
          output$persberichten.verzender.maand.persdienst.plot.procent <- renderPlot(
            persberichten.verzender.maand.persdienst$plot.procent()
          )
          # Tabel
          output$persberichten.verzender.maand.persdienst.tabel <- renderTable(
            persberichten.verzender.maand.persdienst$tabel()
          )
        # Provincie -----------------------------------------------------------
          # Plot - aantal
          persberichten.verzender.maand.provincie <- callModule(data.visual, "bericht.verzender.maand.provincie", Id = "verzender.maand" , Persstatistiek, Xaxis = "Maand", Fill = "Maand", colours = colours, verzender = "Provincie")
          output$persberichten.verzender.maand.provincie.plot.aantal <- renderPlot(
            persberichten.verzender.maand.provincie$plot.aantal()
          )
          # Plot - procent
          persberichten.verzender.maand.provincie <- callModule(data.visual, "bericht.verzender.maand.provincie", Id = "verzender.maand" , Persstatistiek, Xaxis = "Maand", Fill = "Maand", colours = colours, verzender = "Provincie")
          output$persberichten.verzender.maand.provincie.plot.procent <- renderPlot(
            persberichten.verzender.maand.provincie$plot.procent()
          )
          # Tabel
          output$persberichten.verzender.maand.provincie.tabel <- renderTable(
            persberichten.verzender.maand.provincie$tabel()
          )
        # Gouverneur ----------------------------------------------------------
          # Plot - aantal
          persberichten.verzender.maand.gouverneur <- callModule(data.visual, "bericht.verzender.maand.gouverneur", Id = "verzender.maand" , Persstatistiek, Xaxis = "Maand", Fill = "Maand", colours = colours, verzender = "Gouverneur")
          output$persberichten.verzender.maand.gouverneur.plot.aantal <- renderPlot(
            persberichten.verzender.maand.gouverneur$plot.aantal()
          )
          # Plot - procent
          persberichten.verzender.maand.gouverneur <- callModule(data.visual, "bericht.verzender.maand.gouverneur", Id = "verzender.maand" , Persstatistiek, Xaxis = "Maand", Fill = "Maand", colours = colours, verzender = "Gouverneur")
          output$persberichten.verzender.maand.gouverneur.plot.procent <- renderPlot(
            persberichten.verzender.maand.gouverneur$plot.procent()
          )
          # Tabel
          output$persberichten.verzender.maand.gouverneur.tabel <- renderTable(
            persberichten.verzender.maand.gouverneur$tabel()
          )
        # Extern --------------------------------------------------------------
          # Plot - aantal
          persberichten.verzender.maand.extern <- callModule(data.visual, "bericht.verzender.maand.extern", Id = "verzender.maand" , Persstatistiek, Xaxis = "Maand", Fill = "Maand", colours = colours, verzender = "Extern")
          output$persberichten.verzender.maand.extern.plot.aantal <- renderPlot(
            persberichten.verzender.maand.extern$plot.aantal()
          )
          # Plot - procent
          persberichten.verzender.maand.extern <- callModule(data.visual, "bericht.verzender.maand.extern", Id = "verzender.maand" , Persstatistiek, Xaxis = "Maand", Fill = "Maand", colours = colours, verzender = "Extern")
          output$persberichten.verzender.maand.extern.plot.procent <- renderPlot(
            persberichten.verzender.maand.extern$plot.procent()
          )
          # Tabel
          output$persberichten.verzender.maand.extern.tabel <- renderTable(
            persberichten.verzender.maand.extern$tabel()
          )
    # PER TYPE ----------------------------------------------------------------
      # Plot - aantal
      persberichten.type <- callModule(data.visual, "bericht.type", Id = "type" , Persstatistiek, Xaxis = "Beleid", Fill = "Type", colours = colours)
      output$persberichten.type.plot.aantal <- renderPlot(
        persberichten.type$plot.aantal()
      )
      # Plot - procent
      persberichten.type <- callModule(data.visual, "bericht.type", Id = "type" , Persstatistiek, Xaxis = "Beleid", Fill = "Type", colours = colours)
      output$persberichten.type.plot.procent <- renderPlot(
        persberichten.type$plot.procent()
      )
      # Tabel
      output$persberichten.type.tabel <- renderTable(
        persberichten.type$tabel()
      )
  # ===========================================================================
  
  # PERSRETURN ================================================================
    # PER BELEID --------------------------------------------------------------
      # Algemeen --------------------------------------------------------------
        # Plot - aantal
        persreturn.beleid.alg <- callModule(data.visual, "return.beleid.alg", Id = "return.beleid.alg" , Persstatistiek, Xaxis = "Beleid", Fill = "Persreturn", colours = return.colours)
        output$persreturn.beleid.alg.plot.aantal <- renderPlot(
          persreturn.beleid.alg$plot.aantal()
        )
        # Plot - procent
        persreturn.beleid.alg <- callModule(data.visual, "return.beleid.alg", Id = "return.beleid.alg" , Persstatistiek, Xaxis = "Beleid", Fill = "Persreturn", colours = return.colours)
        output$persreturn.beleid.alg.plot.procent <- renderPlot(
          persreturn.beleid.alg$plot.procent()
        )
        # Tabel 
        output$persreturn.beleid.alg.tabel <- renderTable(
          persreturn.beleid.alg$tabel()
        )
      # Deelbeleid ------------------------------------------------------------
        # Economie ------------------------------------------------------------
        persreturn.beleid.beleid.economie <- callModule(data.visual, "return.beleid.beleid.economie", Id = "return.beleid.beleid" , Persstatistiek, Xaxis = "Deelbeleid", Fill = "Persreturn", colours = return.colours, beleid = "Economie")
          # Plot - aantal
          output$persreturn.beleid.beleid.economie.plot.aantal <- renderPlot(
            persreturn.beleid.beleid.economie$plot.aantal()
          )
          # Plot - procent
          output$persreturn.beleid.beleid.economie.plot.procent <- renderPlot(
            persreturn.beleid.beleid.economie$plot.procent()
          )
          # Table
          output$persreturn.beleid.beleid.economie.tabel <- renderTable(
            persreturn.beleid.beleid.economie$tabel()
          )
        # Gouverneur ----------------------------------------------------------
        persreturn.beleid.beleid.gouverneur <- callModule(data.visual, "return.beleid.beleid.gouverneur", Id = "return.beleid.beleid" , Persstatistiek, Xaxis = "Deelbeleid", Fill = "Persreturn", colours = return.colours, beleid = "Gouverneur")
          # Plot  - antal
          output$persreturn.beleid.beleid.gouverneur.plot.aantal <- renderPlot(
            persreturn.beleid.beleid.gouverneur$plot.aantal()
          )
          # Plot - procent 
          output$persreturn.beleid.beleid.gouverneur.plot.procent <- renderPlot(
            persreturn.beleid.beleid.gouverneur$plot.procent()
          )
          # Table
          output$persreturn.beleid.beleid.gouverneur.tabel <- renderTable(
            persreturn.beleid.beleid.gouverneur$tabel()
          )
        # Leefmilieu ----------------------------------------------------------
        persreturn.beleid.beleid.leefmilieu <- callModule(data.visual, "return.beleid.beleid.leefmilieu", Id = "return.beleid.beleid" , Persstatistiek, Xaxis = "Deelbeleid", Fill = "Persreturn", colours = return.colours, beleid = "Leefmilieu")
          # Plot - aantal
          output$persreturn.beleid.beleid.leefmilieu.plot.aantal <- renderPlot(
            persreturn.beleid.beleid.leefmilieu$plot.aantal()
          )
          # Plot - aprocnet
          output$persreturn.beleid.beleid.leefmilieu.plot.procent <- renderPlot(
            persreturn.beleid.beleid.leefmilieu$plot.procent()
          )
          # Table
          output$persreturn.beleid.beleid.leefmilieu.tabel <- renderTable(
            persreturn.beleid.beleid.leefmilieu$tabel()
          )
        # Mobiliteit ----------------------------------------------------------
        persreturn.beleid.beleid.mobiliteit <- callModule(data.visual, "return.beleid.beleid.mobiliteit", Id = "return.beleid.beleid" , Persstatistiek, Xaxis = "Deelbeleid", Fill = "Persreturn", colours = return.colours, beleid = "Mobiliteit")
          # Plot  - aantal
          output$persreturn.beleid.beleid.mobiliteit.plot.aantal <- renderPlot(
            persreturn.beleid.beleid.mobiliteit$plot.aantal()
          )
          # Plot  - procent
          output$persreturn.beleid.beleid.mobiliteit.plot.procent <- renderPlot(
            persreturn.beleid.beleid.mobiliteit$plot.procent()
          )
          # Table
          output$persreturn.beleid.beleid.mobiliteit.tabel <- renderTable(
            persreturn.beleid.beleid.mobiliteit$tabel()
          )
        # Onderwijs en Educatie -----------------------------------------------
        persreturn.beleid.beleid.onderwijs <- callModule(data.visual, "return.beleid.beleid.onderwijs", Id = "return.beleid.beleid" , Persstatistiek, Xaxis = "Deelbeleid", Fill = "Persreturn", colours = return.colours, beleid = "Onderwijs en Educatie")
          # Plot  -aantal
          output$persreturn.beleid.beleid.onderwijs.plot.aantal <- renderPlot(
            persreturn.beleid.beleid.onderwijs$plot.aantal()
          )
          # Plot  - procent
          output$persreturn.beleid.beleid.onderwijs.plot.procent <- renderPlot(
            persreturn.beleid.beleid.onderwijs$plot.procent()
          )
          # Tabel
          output$persreturn.beleid.beleid.onderwijs.tabel <- renderTable(
            persreturn.beleid.beleid.onderwijs$tabel()
          )
        # Provinciebestuur ----------------------------------------------------
        persreturn.beleid.beleid.provinciebestuur <- callModule(data.visual, "return.beleid.beleid.provinciebestuur", Id = "return.beleid.beleid" , Persstatistiek, Xaxis = "Deelbeleid", Fill = "Persreturn", colours = return.colours, beleid = "Provinciebestuur")
          # Plot  - aantal
          output$persreturn.beleid.beleid.provinciebestuur.plot.aantal <- renderPlot(
            persreturn.beleid.beleid.provinciebestuur$plot.aantal()
          )
          # Plot  - procent
          output$persreturn.beleid.beleid.provinciebestuur.plot.procent <- renderPlot(
            persreturn.beleid.beleid.provinciebestuur$plot.procent()
          )
          # Table
          output$persreturn.beleid.beleid.provinciebestuur.tabel <- renderTable(
            persreturn.beleid.beleid.provinciebestuur$tabel()
          )
        # Ruimte --------------------------------------------------------------
        persreturn.beleid.beleid.ruimte <- callModule(data.visual, "return.beleid.beleid.ruimte", Id = "return.beleid.beleid" , Persstatistiek, Xaxis = "Deelbeleid", Fill = "Persreturn", colours = return.colours, beleid = "Ruimte")
          # Plot  - aantal
          output$persreturn.beleid.beleid.ruimte.plot.aantal <- renderPlot(
            persreturn.beleid.beleid.ruimte$plot.aantal()
          )
          # Plot  - procent
          output$persreturn.beleid.beleid.ruimte.plot.procent <- renderPlot(
            persreturn.beleid.beleid.ruimte$plot.procent()
          )
          # Table
          output$persreturn.beleid.beleid.ruimte.tabel <- renderTable(
            persreturn.beleid.beleid.ruimte$tabel()
          )
        # Vrije Tijd ----------------------------------------------------------
        persreturn.beleid.beleid.vrijetijd <- callModule(data.visual, "return.beleid.beleid.vrijetijd", Id = "return.beleid.beleid" , Persstatistiek, Xaxis = "Deelbeleid", Fill = "Persreturn", colours = return.colours, beleid = "Vrije Tijd")
          # Plot  - aantal
          output$persreturn.beleid.beleid.vrijetijd.plot.aantal <- renderPlot(
            persreturn.beleid.beleid.vrijetijd$plot.aantal()
          )
          # Plot  - procent
          output$persreturn.beleid.beleid.vrijetijd.plot.procent <- renderPlot(
            persreturn.beleid.beleid.vrijetijd$plot.procent()
          )
          # Table
          output$persreturn.beleid.beleid.vrijetijd.tabel <- renderTable(
            persreturn.beleid.beleid.vrijetijd$tabel()
          )
    # PER MEDIUM --------------------------------------------------------------
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
      output$persreturn.medium.tabel <- renderTable(
        persreturn.medium$tabel()
      )
  # ===========================================================================
      
  # HTML RAPPORT AANMAAK ======================================================
  output$report <- downloadHandler(
    
    # For PDF output, change this to "report.pdf"
    filename = reactive(paste0("Persstatistiek_", paste0(input$jaar, paste0("_", paste0(input$kwartaal, ".html"))))),
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).

      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      
      
    # VARIABLE COLLECTION FOR MARKDOWN (HTML) ---------------------------------
      # Persberichten ---------------------------------------------------------
        # Algemeen ------------------------------------------------------------
        Persberichten.alg <- list(kwartaal.plot.aantal = reactive(persberichten.alg.kwartaal$plot.aantal()),
                                  kwartaal.tabel = reactive(persberichten.alg.kwartaal$tabel()),
                                  kwartaal.uitleg = reactive(persberichten.alg.kwartaal$uitleg()),
                                  maand.plot.aantal = reactive(persberichten.alg.maand$plot.aantal()),
                                  maand.tabel = reactive(persberichten.alg.maand$tabel()),
                                  maand.uitleg = reactive(persberichten.alg.maand$uitleg()),
                                  beleid.plot.aantal = reactive(persberichten.alg.beleid$plot.aantal()),
                                  beleid.tabel = reactive(persberichten.alg.beleid$tabel()),
                                  beleid.uitleg= reactive(persberichten.alg.beleid$uitleg()))
        # Per beleid ----------------------------------------------------------
          # Maand -------------------------------------------------------------
          Persberichten.beleid.maand <- list(economie.plot.aantal = reactive(persberichten.beleid.maand.economie$plot.aantal()),
                                             economie.tabel = reactive(persberichten.beleid.maand.economie$tabel()),
                                             economie.uitleg = reactive(persberichten.beleid.maand.economie$uitleg()),
                                             gouverneur.plot.aantal = reactive(persberichten.beleid.maand.gouverneur$plot.aantal()),
                                             gouverneur.tabel = reactive(persberichten.beleid.maand.gouverneur$tabel()),
                                             gouverneur.uitleg = reactive(persberichten.beleid.maand.gouverneur$uitleg()),
                                             leefmilieu.plot.aantal = reactive(persberichten.beleid.maand.leefmilieu$plot.aantal()),
                                             leefmilieu.tabel = reactive(persberichten.beleid.maand.leefmilieu$tabel()),
                                             leefmilieu.uitleg = reactive(persberichten.beleid.maand.leefmilieu$uitleg()),
                                             mobiliteit.plot.aantal = reactive(persberichten.beleid.maand.mobiliteit$plot.aantal()),
                                             mobiliteit.tabel = reactive(persberichten.beleid.maand.mobiliteit$tabel()),
                                             mobiliteit.uitleg = reactive(persberichten.beleid.maand.mobiliteit$uitleg()),
                                             onderwijs.plot.aantal = reactive(persberichten.beleid.maand.onderwijs$plot.aantal()),
                                             onderwijs.tabel = reactive(persberichten.beleid.maand.onderwijs$tabel()),
                                             onderwijs.uitleg = reactive(persberichten.beleid.maand.onderwijs$uitleg()),
                                             provinciebestuur.plot.aantal = reactive(persberichten.beleid.maand.provinciebestuur$plot.aantal()),
                                             provinciebestuur.tabel = reactive(persberichten.beleid.maand.provinciebestuur$tabel()),
                                             provinciebestuur.uitleg = reactive(persberichten.beleid.maand.provinciebestuur$uitleg()),
                                             ruimte.plot.aantal = reactive(persberichten.beleid.maand.ruimte$plot.aantal()),
                                             ruimte.tabel = reactive(persberichten.beleid.maand.ruimte$tabel()),
                                             ruimte.uitleg = reactive(persberichten.beleid.maand.ruimte$uitleg()),
                                             vrijetijd.plot.aantal = reactive(persberichten.beleid.maand.vrijetijd$plot.aantal()),
                                             vrijetijd.tabel = reactive(persberichten.beleid.maand.vrijetijd$tabel()),
                                             vrijetijd.uitleg = reactive(persberichten.beleid.maand.vrijetijd$uitleg()))
          # Beleid ------------------------------------------------------------
          Persberichten.beleid.beleid <- list(economie.plot.aantal = reactive(persberichten.beleid.beleid.economie$plot.aantal()),
                                              economie.tabel = reactive(persberichten.beleid.beleid.economie$tabel()),
                                              economie.uitleg = reactive(persberichten.beleid.beleid.economie$uitleg()),
                                              gouverneur.plot.aantal = reactive(persberichten.beleid.beleid.gouverneur$plot.aantal()),
                                              gouverneur.tabel = reactive(persberichten.beleid.beleid.gouverneur$tabel()),
                                              gouverneur.uitleg = reactive(persberichten.beleid.beleid.gouverneur$uitleg()),
                                              leefmilieu.plot.aantal = reactive(persberichten.beleid.beleid.leefmilieu$plot.aantal()),
                                              leefmilieu.tabel = reactive(persberichten.beleid.beleid.leefmilieu$tabel()),
                                              leefmilieu.uitleg = reactive(persberichten.beleid.beleid.leefmilieu$uitleg()),
                                              mobiliteit.plot.aantal = reactive(persberichten.beleid.beleid.mobiliteit$plot.aantal()),
                                              mobiliteit.tabel = reactive(persberichten.beleid.beleid.mobiliteit$tabel()),
                                              mobiliteit.uitleg = reactive(persberichten.beleid.beleid.mobiliteit$uitleg()),
                                              onderwijs.plot.aantal = reactive(persberichten.beleid.beleid.onderwijs$plot.aantal()),
                                              onderwijs.tabel = reactive(persberichten.beleid.beleid.onderwijs$tabel()),
                                              onderwijs.uitleg = reactive(persberichten.beleid.beleid.onderwijs$uitleg()),
                                              provinciebestuur.plot.aantal = reactive(persberichten.beleid.beleid.provinciebestuur$plot.aantal()),
                                              provinciebestuur.tabel = reactive(persberichten.beleid.beleid.provinciebestuur$tabel()),
                                              provinciebestuur.uitleg = reactive(persberichten.beleid.beleid.provinciebestuur$uitleg()),
                                              ruimte.plot.aantal = reactive(persberichten.beleid.beleid.ruimte$plot.aantal()),
                                              ruimte.tabel = reactive(persberichten.beleid.beleid.ruimte$tabel()),
                                              ruimte.uitleg = reactive(persberichten.beleid.beleid.ruimte$uitleg()),
                                              vrijetijd.plot.aantal = reactive(persberichten.beleid.beleid.vrijetijd$plot.aantal()),
                                              vrijetijd.tabel = reactive(persberichten.beleid.beleid.vrijetijd$tabel()),
                                              vrijetijd.uitleg = reactive(persberichten.beleid.beleid.vrijetijd$uitleg()))
        # Verzender -----------------------------------------------------------
          # Algemeen ----------------------------------------------------------
          Persberichten.verzender.alg <- list(totaal.plot.aantal = reactive(persberichten.verzender.alg.totaal$plot.aantal()),
                                              totaal.tabel = reactive(persberichten.verzender.alg.totaal$tabel()),
                                              totaal.uitleg = reactive(persberichten.verzender.alg.totaal$uitleg()),
                                              beleid.plot.aantal = reactive(persberichten.verzender.alg.beleid$plot.aantal()),
                                              beleid.tabel = reactive(persberichten.verzender.alg.beleid$plot.aantal()),
                                              beleid.uitleg = reactive(persberichten.verzender.alg.beleid$uitleg()))
          # Per maand ---------------------------------------------------------
          Persberichten.verzender.maand <- list(persdienst.plot.aantal = reactive(persberichten.verzender.maand.persdienst$plot.aantal()),
                                                persdienst.tabel = reactive(persberichten.verzender.maand.persdienst$tabel()),
                                                persdienst.uitleg = reactive(persberichten.verzender.maand.persdienst$uitleg()),
                                                provincie.plot.aantal = reactive(persberichten.verzender.maand.provincie$plot.aantal()),
                                                provincie.tabel = reactive(persberichten.verzender.maand.provincie$tabel()),
                                                provincie.uitleg = reactive(persberichten.verzender.maand.provincie$uitleg()),
                                                gouverneur.plot.aantal = reactive(persberichten.verzender.maand.gouverneur$plot.aantal()),
                                                gouverneur.tabel = reactive(persberichten.verzender.maand.gouverneur$tabel()),
                                                gouverneur.uitleg = reactive(persberichten.verzender.maand.gouverneur$uitleg()),
                                                extern.plot.aantal = reactive(persberichten.verzender.maand.extern$plot.aantal()),
                                                extern.tabel = reactive(persberichten.verzender.maand.extern$tabel()),
                                                extern.uitleg = reactive(persberichten.verzender.maand.extern$uitleg()))
        # Type ----------------------------------------------------------------
        Persberichten.type <- list(type.plot.aantal = reactive(persberichten.type$plot.aantal()),
                                   type.tabel = reactive(persberichten.type$tabel()),
                                   type.uitleg = reactive(persberichten.type$uitleg()))
      # Persreturn ------------------------------------------------------------
        # Beleid --------------------------------------------------------------
        Persreturn.beleid <- list(algemeen.plot.aantal = reactive(persreturn.beleid.alg$plot.aantal()),
                                  algemeen.tabel = reactive(persreturn.beleid.alg$tabel()),
                                  algemeen.uitleg = reactive(persreturn.beleid.alg$uitleg()),
                                  economie.plot.aantal = reactive(persreturn.beleid.beleid.economie$plot.aantal()),
                                  economie.tabel = reactive(persreturn.beleid.beleid.economie$tabel()),
                                  economie.uitleg = reactive(persreturn.beleid.beleid.economie$uitleg()),
                                  gouverneur.plot.aantal = reactive(persreturn.beleid.beleid.gouverneur$plot.aantal()),
                                  gouverneur.tabel = reactive(persreturn.beleid.beleid.gouverneur$tabel()),
                                  gouverneur.uitleg = reactive(persreturn.beleid.beleid.gouverneur$uitleg()),
                                  leefmilieu.plot.aantal = reactive(persreturn.beleid.beleid.leefmilieu$plot.aantal()),
                                  leefmilieu.tabel = reactive(persreturn.beleid.beleid.leefmilieu$tabel()),
                                  leefmilieu.uitleg = reactive(persreturn.beleid.beleid.leefmilieu$uitleg()),
                                  mobiliteit.plot.aantal = reactive(persreturn.beleid.beleid.mobiliteit$plot.aantal()),
                                  mobiliteit.tabel = reactive(persreturn.beleid.beleid.mobiliteit$tabel()),
                                  mobiliteit.uitleg = reactive(persreturn.beleid.beleid.mobiliteit$uitleg()),
                                  onderwijs.plot.aantal = reactive(persreturn.beleid.beleid.onderwijs$plot.aantal()),
                                  onderwijs.tabel = reactive(persreturn.beleid.beleid.onderwijs$tabel()),
                                  onderwijs.uitleg = reactive(persreturn.beleid.beleid.onderwijs$uitleg()),
                                  provinciebestuur.plot.aantal = reactive(persreturn.beleid.beleid.provinciebestuur$plot.aantal()),
                                  provinciebestuur.tabel = reactive(persreturn.beleid.beleid.provinciebestuur$tabel()),
                                  provinciebestuur.uitleg = reactive(persreturn.beleid.beleid.provinciebestuur$uitleg()),
                                  ruimte.plot.aantal = reactive(persreturn.beleid.beleid.ruimte$plot.aantal()),
                                  ruimte.tabel = reactive(persreturn.beleid.beleid.ruimte$tabel()),
                                  ruimte.uitleg = reactive(persreturn.beleid.beleid.ruimte$uitleg()),
                                  vrijetijd.plot.aantal = reactive(persreturn.beleid.beleid.vrijetijd$plot.aantal()),
                                  vrijetijd.tabel = reactive(persreturn.beleid.beleid.vrijetijd$tabel()),
                                  vrijetijd.uitleg = reactive(persreturn.beleid.beleid.vrijetijd$uitleg()))
        # Medium --------------------------------------------------------------
        Persreturn.medium <- list(medium.plot.aantal = reactive(persreturn.medium$plot.aantal()),
                                  medium.tabel = reactive(persreturn.medium$tabel()),
                                  medium.uitleg = reactive(persreturn.medium$uitleg()))
  
    # Set up parameters to pass to Rmd document -------------------------------
      params <- list(titel = paste("Persstatistiek:", paste(input$jaar, input$kwartaal, sep= " - ")),
                     Persberichten.alg = Persberichten.alg,
                     Persberichten.beleid.maand = Persberichten.beleid.maand,
                     Persberichten.beleid.beleid = Persberichten.beleid.beleid,
                     Persberichten.verzender.alg = Persberichten.verzender.alg,
                     Persberichten.verzender.maand = Persberichten.verzender.maand,
                     Persberichten.type = Persberichten.type,
                     Persreturn.beleid = Persreturn.beleid,
                     Persreturn.medium = Persreturn.medium,
                     rendered_by_shiny = TRUE)
    
    # Knit HTML document ------------------------------------------------------   
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      req(input$file$datapath)
      withProgress(
        message = 'Rapport samenstellen',
        min = 0,
        max = 25,
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
  # ===========================================================================
}