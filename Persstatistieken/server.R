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
    # return(Persstatistiek())
    Persstatistiek()
  })
  # ===========================================================================
  
  # PERSBERICHTEN =============================================================
    source("./Modules/data_visualisation.R")
    # ALGEMEEN ----------------------------------------------------------------
      # Per Kwartaal ----------------------------------------------------------
        persberichten.alg.kwartaal <- callModule(data.visual, "bericht.alg.kwartaal", Id = "alg.kwartaal", data = Persstatistiek, Xaxis = "Kwartaal", Fill = "Kwartaal", colours = colours)      
        # Barplot
          output$persberichten.alg.kwartaal.plot <- renderPlot(
            persberichten.alg.kwartaal$plot()
          )
        # Tabel
          output$persberichten.alg.kwartaal.tabel <- renderTable(
            persberichten.alg.kwartaal$tabel()
          )
      # Per Maand -------------------------------------------------------------
        # Plot
          persberichten.alg.maand <- callModule(data.visual, "bericht.alg.maand", Id = "alg.maand", data = Persstatistiek, Xaxis = "Maand", Fill = "Maand", colours = colours)  
          output$persberichten.alg.maand.plot <- renderPlot(
            persberichten.alg.maand$plot()
          )
        # Tabel
          output$persberichten.alg.maand.tabel <- renderTable(
            persberichten.alg.maand$tabel()
          )
      # Per Beleid ------------------------------------------------------------
        # Plot
          persberichten.alg.beleid <- callModule(data.visual, "bericht.alg.beleid", Id = "alg.beleid", data = Persstatistiek, Xaxis = "Beleid", Fill = "Beleid", colours = colours)   
          output$persberichten.alg.beleid.plot <- renderPlot(
            persberichten.alg.beleid$plot()
          )
        # Tabel
          output$persberichten.alg.beleid.tabel <- renderTable(
            persberichten.alg.beleid$tabel()
          )
    # PER BELEID --------------------------------------------------------------
      # Per Maand -------------------------------------------------------------
        # Economie ------------------------------------------------------------
        persberichten.beleid.maand.economie <- callModule(data.visual, "bericht.beleid.maand.plot.economie", Id = "beleid.maand" ,data = Persstatistiek, Xaxis = "Maand", Fill = "Maand", colours = colours, beleid = "Economie")
          # Plot
          output$persberichten.beleid.maand.economie.plot <- renderPlot(
            persberichten.beleid.maand.economie$plot()
          )
          # Table
          output$persberichten.beleid.maand.economie.tabel <- renderTable(
            persberichten.beleid.maand.economie$tabel()
          )
        # Gouverneur ----------------------------------------------------------
        persberichten.beleid.maand.gouverneur <- callModule(data.visual, "bericht.beleid.maand.plot.gouverneur", Id = "beleid.maand" , data = Persstatistiek, Xaxis = "Maand", Fill = "Maand", colours = colours, beleid = "Gouverneur")
          # Plot  
          output$persberichten.beleid.maand.gouverneur.plot <- renderPlot(
            persberichten.beleid.maand.gouverneur$plot()
          )
          # Table
          output$persberichten.beleid.maand.gouverneur.tabel <- renderTable(
            persberichten.beleid.maand.gouverneur$tabel()
          )
          
        # Leefmilieu ----------------------------------------------------------
        persberichten.beleid.maand.leefmilieu <- callModule(data.visual, "bericht.beleid.maand.plot.leefmilieu", Id = "beleid.maand" , data = Persstatistiek, Xaxis = "Maand", Fill = "Maand", colours = colours, beleid = "Leefmilieu")
          # Plot
          output$persberichten.beleid.maand.leefmilieu.plot <- renderPlot(
            persberichten.beleid.maand.leefmilieu$plot()
          )
          # Table
          output$persberichten.beleid.maand.leefmilieu.tabel <- renderTable(
            persberichten.beleid.maand.leefmilieu$tabel()
          )
        # Mobiliteit ----------------------------------------------------------
        persberichten.beleid.maand.mobiliteit <- callModule(data.visual, "bericht.beleid.maand.plot.mobiliteit", Id = "beleid.maand" , data = Persstatistiek, Xaxis = "Maand", Fill = "Maand", colours = colours, beleid = "Mobiliteit")
          # Plot  
          output$persberichten.beleid.maand.mobiliteit.plot <- renderPlot(
            persberichten.beleid.maand.mobiliteit$plot()
          )
          # Table
          output$persberichten.beleid.maand.mobiliteit.tabel <- renderTable(
            persberichten.beleid.maand.mobiliteit$tabel()
          )
        # Onderwijs en Educatie -----------------------------------------------
        persberichten.beleid.maand.onderwijs <- callModule(data.visual, "bericht.beleid.maand.plot.onderwijs", Id = "beleid.maand" , data = Persstatistiek, Xaxis = "Maand", Fill = "Maand", colours = colours, beleid = "Onderwijs en Educatie")
          # Plot  
          output$persberichten.beleid.maand.onderwijs.plot <- renderPlot(
            persberichten.beleid.maand.onderwijs$plot()
          )
          # Tabel
          output$persberichten.beleid.maand.onderwijs.tabel <- renderTable(
            persberichten.beleid.maand.onderwijs$tabel()
          )
        # Provinciebestuur ----------------------------------------------------
        persberichten.beleid.maand.provinciebestuur <- callModule(data.visual, "bericht.beleid.maand.plot.provinciebestuur", Id = "beleid.maand" , data = Persstatistiek, Xaxis = "Maand", Fill = "Maand", colours = colours, beleid = "Provinciebestuur")
          # Plot  
          output$persberichten.beleid.maand.provinciebestuur.plot <- renderPlot(
            persberichten.beleid.maand.provinciebestuur$plot()
          )
          # Table
          output$persberichten.beleid.maand.provinciebestuur.tabel <- renderTable(
            persberichten.beleid.maand.provinciebestuur$tabel()
          )
        # Ruimte --------------------------------------------------------------
        persberichten.beleid.maand.ruimte <- callModule(data.visual, "bericht.beleid.maand.plot.ruimte", Id = "beleid.maand" , data = Persstatistiek, Xaxis = "Maand", Fill = "Maand", colours = colours, beleid = "Ruimte")
          # Plot  
          output$persberichten.beleid.maand.ruimte.plot <- renderPlot(
            persberichten.beleid.maand.ruimte$plot()
          )
          # Table
          output$persberichten.beleid.maand.ruimte.tabel <- renderTable(
            persberichten.beleid.maand.ruimte$tabel()
          )
        # Vrije Tijd ----------------------------------------------------------
        persberichten.beleid.maand.vrijetijd <- callModule(data.visual, "bericht.beleid.maand.plot.vrijetijd", Id = "beleid.maand" , data = Persstatistiek, Xaxis = "Maand", Fill = "Maand", colours = colours, beleid = "Vrije Tijd")
          # Plot  
          output$persberichten.beleid.maand.vrijetijd.plot <- renderPlot(
            persberichten.beleid.maand.vrijetijd$plot()
          )
          # Table
          output$persberichten.beleid.maand.vrijetijd.tabel <- renderTable(
            persberichten.beleid.maand.vrijetijd$tabel()
          )

      # Per Deelbeleid --------------------------------------------------------
        # Economie ------------------------------------------------------------
        persberichten.beleid.beleid.economie <- callModule(data.visual, "bericht.beleid.beleid.plot.economie", Id = "beleid.beleid" , Persstatistiek, Xaxis = "Deelbeleid", Fill = "Deelbeleid", colours = colours, beleid = "Economie")
          # Plot
          output$persberichten.beleid.beleid.economie.plot <- renderPlot(
            persberichten.beleid.beleid.economie$plot()
          )
          # Table
          output$persberichten.beleid.beleid.economie.tabel <- renderTable(
            persberichten.beleid.beleid.economie$tabel()
          )
        # Gouverneur ----------------------------------------------------------
        persberichten.beleid.beleid.gouverneur <- callModule(data.visual, "bericht.beleid.beleid.plot.gouverneur", Id = "beleid.beleid" , Persstatistiek, Xaxis = "Deelbeleid", Fill = "Deelbeleid", colours = colours, beleid = "Gouverneur")
          # Plot  
          output$persberichten.beleid.beleid.gouverneur.plot <- renderPlot(
            persberichten.beleid.beleid.gouverneur$plot()
          )
          # Table
          output$persberichten.beleid.beleid.gouverneur.tabel <- renderTable(
            persberichten.beleid.beleid.gouverneur$tabel()
          )
        # Leefmilieu ----------------------------------------------------------
        persberichten.beleid.beleid.leefmilieu <- callModule(data.visual, "bericht.beleid.beleid.plot.leefmilieu", Id = "beleid.beleid" , Persstatistiek, Xaxis = "Deelbeleid", Fill = "Deelbeleid", colours = colours, beleid = "Leefmilieu")
          # Plot
          output$persberichten.beleid.beleid.leefmilieu.plot <- renderPlot(
            persberichten.beleid.beleid.leefmilieu$plot()
          )
          # Table
          output$persberichten.beleid.beleid.leefmilieu.tabel <- renderTable(
            persberichten.beleid.beleid.leefmilieu$tabel()
          )
        # Mobiliteit ----------------------------------------------------------
        persberichten.beleid.beleid.mobiliteit <- callModule(data.visual, "bericht.beleid.beleid.plot.mobiliteit", Id = "beleid.beleid" , Persstatistiek, Xaxis = "Deelbeleid", Fill = "Deelbeleid", colours = colours, beleid = "Mobiliteit")
          # Plot  
          output$persberichten.beleid.beleid.mobiliteit.plot <- renderPlot(
            persberichten.beleid.beleid.mobiliteit$plot()
          )
          # Table
          output$persberichten.beleid.beleid.mobiliteit.tabel <- renderTable(
            persberichten.beleid.beleid.mobiliteit$tabel()
          )
        # Onderwijs en Educatie -----------------------------------------------
        persberichten.beleid.beleid.onderwijs <- callModule(data.visual, "bericht.beleid.beleid.plot.onderwijs", Id = "beleid.beleid" , Persstatistiek, Xaxis = "Deelbeleid", Fill = "Deelbeleid", colours = colours, beleid = "Onderwijs en Educatie")
          # Plot  
          output$persberichten.beleid.beleid.onderwijs.plot <- renderPlot(
            persberichten.beleid.beleid.onderwijs$plot()
          )
          # Tabel
          output$persberichten.beleid.beleid.onderwijs.tabel <- renderTable(
            persberichten.beleid.beleid.onderwijs$tabel()
          )
        # Provinciebestuur ----------------------------------------------------
        persberichten.beleid.beleid.provinciebestuur <- callModule(data.visual, "bericht.beleid.beleid.plot.provinciebestuur", Id = "beleid.beleid" , Persstatistiek, Xaxis = "Deelbeleid", Fill = "Deelbeleid", colours = colours, beleid = "Provinciebestuur")
          # Plot  
          output$persberichten.beleid.beleid.provinciebestuur.plot <- renderPlot(
            persberichten.beleid.beleid.provinciebestuur$plot()
          )
          # Table
          output$persberichten.beleid.beleid.provinciebestuur.tabel <- renderTable(
            persberichten.beleid.beleid.provinciebestuur$tabel()
          )
        # Ruimte --------------------------------------------------------------
        persberichten.beleid.beleid.ruimte <- callModule(data.visual, "bericht.beleid.beleid.plot.ruimte", Id = "beleid.beleid" , Persstatistiek, Xaxis = "Deelbeleid", Fill = "Deelbeleid", colours = colours, beleid = "Ruimte")
          # Plot  
          output$persberichten.beleid.beleid.ruimte.plot <- renderPlot(
            persberichten.beleid.beleid.ruimte$plot()
          )
          # Table
          output$persberichten.beleid.beleid.ruimte.tabel <- renderTable(
            persberichten.beleid.beleid.ruimte$tabel()
        )
        # Vrije Tijd ----------------------------------------------------------
        persberichten.beleid.beleid.vrijetijd <- callModule(data.visual, "bericht.beleid.beleid.plot.vrijetijd", Id = "beleid.beleid" , Persstatistiek, Xaxis = "Deelbeleid", Fill = "Deelbeleid", colours = colours, beleid = "Vrije Tijd")
          # Plot  
          output$persberichten.beleid.beleid.vrijetijd.plot <- renderPlot(
            persberichten.beleid.beleid.vrijetijd$plot()
          )
          # Table
          output$persberichten.beleid.beleid.vrijetijd.tabel <- renderTable(
            persberichten.beleid.beleid.vrijetijd$tabel()
          )
    # PER VERZENDER -----------------------------------------------------------
      # Algemeen --------------------------------------------------------------
        # Totaal per Verzender ------------------------------------------------
          # Plot
          persberichten.verzender.alg.totaal <- callModule(data.visual, "bericht.verzender.alg.totaal", Id = "verzender.alg.verzender" , Persstatistiek, Xaxis = "Verzender", Fill = "Verzender", colours = colours)
          output$persberichten.verzender.alg.totaal.plot <- renderPlot(
            persberichten.verzender.alg.totaal$plot()
          )
          # Tabel
          output$persberichten.verzender.alg.totaal.tabel <- renderTable(
            persberichten.verzender.alg.totaal$tabel()
          )
        # Beleid per Verzender ------------------------------------------------
          # Plot
          persberichten.verzender.alg.beleid <- callModule(data.visual, "bericht.verzender.alg.beleid", Id = "verzender.alg.beleid" , Persstatistiek, Xaxis = "Verzender", Fill = "Beleid", colours = colours)
          output$persberichten.verzender.alg.beleid.plot <- renderPlot(
            persberichten.verzender.alg.beleid$plot()
          )
          # Tabel
          output$persberichten.verzender.alg.beleid.tabel <- renderTable(
            persberichten.verzender.alg.beleid$tabel()
          )
      # Per Maand -------------------------------------------------------------
        # Persdienst ----------------------------------------------------------
          # Plot
          persberichten.verzender.maand.persdienst <- callModule(data.visual, "bericht.verzender.maand.persdienst", Id = "verzender.maand" , Persstatistiek, Xaxis = "Maand", Fill = "Maand", colours = colours, verzender = "Persdienst")
          output$persberichten.verzender.maand.persdienst.plot <- renderPlot(
            persberichten.verzender.maand.persdienst$plot()
          )
          # Tabel
          output$persberichten.verzender.maand.persdienst.tabel <- renderTable(
            persberichten.verzender.maand.persdienst$tabel()
          )
        # Provincie -----------------------------------------------------------
          # Plot 
          persberichten.verzender.maand.provincie <- callModule(data.visual, "bericht.verzender.maand.provincie", Id = "verzender.maand" , Persstatistiek, Xaxis = "Maand", Fill = "Maand", colours = colours, verzender = "Provincie")
          output$persberichten.verzender.maand.provincie.plot <- renderPlot(
            persberichten.verzender.maand.provincie$plot()
          )
          # Tabel
          output$persberichten.verzender.maand.provincie.tabel <- renderTable(
            persberichten.verzender.maand.provincie$tabel()
          )
        # Gouverneur ----------------------------------------------------------
          # Plot
          persberichten.verzender.maand.gouverneur <- callModule(data.visual, "bericht.verzender.maand.gouverneur", Id = "verzender.maand" , Persstatistiek, Xaxis = "Maand", Fill = "Maand", colours = colours, verzender = "Gouverneur")
          output$persberichten.verzender.maand.gouverneur.plot <- renderPlot(
            persberichten.verzender.maand.gouverneur$plot()
          )
          # Tabel
          output$persberichten.verzender.maand.gouverneur.tabel <- renderTable(
            persberichten.verzender.maand.gouverneur$tabel()
          )
        # Extern --------------------------------------------------------------
          # Plot
          persberichten.verzender.maand.extern <- callModule(data.visual, "bericht.verzender.maand.extern", Id = "verzender.maand" , Persstatistiek, Xaxis = "Maand", Fill = "Maand", colours = colours, verzender = "Extern")
          output$persberichten.verzender.maand.extern.plot <- renderPlot(
            persberichten.verzender.maand.extern$plot()
          )
          # Tabel
          output$persberichten.verzender.maand.extern.tabel <- renderTable(
            persberichten.verzender.maand.extern$tabel()
          )
    # PER TYPE ----------------------------------------------------------------
      # Plot
      persberichten.type <- callModule(data.visual, "bericht.type", Id = "type" , Persstatistiek, Xaxis = "Beleid", Fill = "Type", colours = colours)
      output$persberichten.type.plot <- renderPlot(
        persberichten.type$plot()
      )
      # Tabel
      output$persberichten.type.tabel <- renderTable(
        persberichten.type$tabel()
      )
  # ===========================================================================
  
  # PERSRETURN ================================================================
    # PER BELEID --------------------------------------------------------------
      # Algemeen --------------------------------------------------------------
        # Plot
        persreturn.beleid.alg <- callModule(data.visual, "return.beleid.alg", Id = "return.beleid.alg" , Persstatistiek, Xaxis = "Beleid", Fill = "Persreturn", colours = return.colours)
        output$persreturn.beleid.alg.plot <- renderPlot(
          persreturn.beleid.alg$plot()
        )
        # Tabel 
        output$persreturn.beleid.alg.tabel <- renderTable(
          persreturn.beleid.alg$tabel()
        )
      # Deelbeleid ------------------------------------------------------------
        # Economie ------------------------------------------------------------
        persreturn.beleid.beleid.economie <- callModule(data.visual, "return.beleid.beleid.economie", Id = "return.beleid.beleid" , Persstatistiek, Xaxis = "Deelbeleid", Fill = "Persreturn", colours = return.colours, beleid = "Economie")
          # Plot
          output$persreturn.beleid.beleid.economie.plot <- renderPlot(
            persreturn.beleid.beleid.economie$plot()
          )
          # Table
          output$persreturn.beleid.beleid.economie.tabel <- renderTable(
            persreturn.beleid.beleid.economie$tabel()
          )
        # Gouverneur ----------------------------------------------------------
        persreturn.beleid.beleid.gouverneur <- callModule(data.visual, "return.beleid.beleid.gouverneur", Id = "return.beleid.beleid" , Persstatistiek, Xaxis = "Deelbeleid", Fill = "Persreturn", colours = return.colours, beleid = "Gouverneur")
          # Plot  
          output$persreturn.beleid.beleid.gouverneur.plot <- renderPlot(
            persreturn.beleid.beleid.gouverneur$plot()
          )
          # Table
          output$persreturn.beleid.beleid.gouverneur.tabel <- renderTable(
            persreturn.beleid.beleid.gouverneur$tabel()
          )
        # Leefmilieu ----------------------------------------------------------
        persreturn.beleid.beleid.leefmilieu <- callModule(data.visual, "return.beleid.beleid.leefmilieu", Id = "return.beleid.beleid" , Persstatistiek, Xaxis = "Deelbeleid", Fill = "Persreturn", colours = return.colours, beleid = "Leefmilieu")
          # Plot
          output$persreturn.beleid.beleid.leefmilieu.plot <- renderPlot(
            persreturn.beleid.beleid.leefmilieu$plot()
          )
          # Table
          output$persreturn.beleid.beleid.leefmilieu.tabel <- renderTable(
            persreturn.beleid.beleid.leefmilieu$tabel()
          )
        # Mobiliteit ----------------------------------------------------------
        persreturn.beleid.beleid.mobiliteit <- callModule(data.visual, "return.beleid.beleid.mobiliteit", Id = "return.beleid.beleid" , Persstatistiek, Xaxis = "Deelbeleid", Fill = "Persreturn", colours = return.colours, beleid = "Mobiliteit")
          # Plot  
          output$persreturn.beleid.beleid.mobiliteit.plot <- renderPlot(
            persreturn.beleid.beleid.mobiliteit$plot()
          )
          # Table
          output$persreturn.beleid.beleid.mobiliteit.tabel <- renderTable(
            persreturn.beleid.beleid.mobiliteit$tabel()
          )
        # Onderwijs en Educatie -----------------------------------------------
        persreturn.beleid.beleid.onderwijs <- callModule(data.visual, "return.beleid.beleid.onderwijs", Id = "return.beleid.beleid" , Persstatistiek, Xaxis = "Deelbeleid", Fill = "Persreturn", colours = return.colours, beleid = "Onderwijs en Educatie")
          # Plot  
          output$persreturn.beleid.beleid.onderwijs.plot <- renderPlot(
            persreturn.beleid.beleid.onderwijs$plot()
          )
          # Tabel
          output$persreturn.beleid.beleid.onderwijs.tabel <- renderTable(
            persreturn.beleid.beleid.onderwijs$tabel()
          )
        # Provinciebestuur ----------------------------------------------------
        persreturn.beleid.beleid.provinciebestuur <- callModule(data.visual, "return.beleid.beleid.provinciebestuur", Id = "return.beleid.beleid" , Persstatistiek, Xaxis = "Deelbeleid", Fill = "Persreturn", colours = return.colours, beleid = "Provinciebestuur")
          # Plot  
          output$persreturn.beleid.beleid.provinciebestuur.plot <- renderPlot(
            persreturn.beleid.beleid.provinciebestuur$plot()
          )
          # Table
          output$persreturn.beleid.beleid.provinciebestuur.tabel <- renderTable(
            persreturn.beleid.beleid.provinciebestuur$tabel()
          )
        # Ruimte --------------------------------------------------------------
        persreturn.beleid.beleid.ruimte <- callModule(data.visual, "return.beleid.beleid.ruimte", Id = "return.beleid.beleid" , Persstatistiek, Xaxis = "Deelbeleid", Fill = "Persreturn", colours = return.colours, beleid = "Ruimte")
          # Plot  
          output$persreturn.beleid.beleid.ruimte.plot <- renderPlot(
            persreturn.beleid.beleid.ruimte$plot()
          )
          # Table
          output$persreturn.beleid.beleid.ruimte.tabel <- renderTable(
            persreturn.beleid.beleid.ruimte$tabel()
          )
        # Vrije Tijd ----------------------------------------------------------
        persreturn.beleid.beleid.vrijetijd <- callModule(data.visual, "return.beleid.beleid.vrijetijd", Id = "return.beleid.beleid" , Persstatistiek, Xaxis = "Deelbeleid", Fill = "Persreturn", colours = return.colours, beleid = "Vrije Tijd")
          # Plot  
          output$persreturn.beleid.beleid.vrijetijd.plot <- renderPlot(
            persreturn.beleid.beleid.vrijetijd$plot()
          )
          # Table
          output$persreturn.beleid.beleid.vrijetijd.tabel <- renderTable(
            persreturn.beleid.beleid.vrijetijd$tabel()
          )
    # PER MEDIUM --------------------------------------------------------------
      # Barplot
      persreturn.medium <- callModule(data.visual, "return.medium", Id = "return.medium" , Persstatistiek, Xaxis = "Beleid", Fill = "Medium", colours = colours)
      output$persreturn.medium.plot <- renderPlot(
        persreturn.medium$plot()
      )
      # Tabel
      output$persreturn.medium.tabel <- renderTable(
        persreturn.medium$tabel()
      )
  # ===========================================================================
      
  # Download test =============================================================
      Rapport.volgorde <- reactive({
        volgorde <- input$Persberichten
        for(i in volgorde) {
        # Persberichten algemeen ----------------------------------------------
          if(i == "Algemeen") {
            volgorde$Algemeen <- input$Persberichten.alg
            for (j in volgorde$Algemeen) {
              if(j == "Kwartaal") {
                volgorde$Algemeen$Kwartaal <- list("Kwartaal" = list("titel" = input$bericht.kwartaal.titel, "uitleg" = persberichten.alg.kwartaal$uitleg()))
              } else if (j == "Maand") {
                volgorde$Algemeen$Maand <- list("Maand" = list("titel" = input$bericht.maand.titel, "uitleg" = persberichten.alg.maand$uitleg()))
              } else if (j == "Beleid") {
                volgorde$Algemeen$Beleid <- list("Beleid" = list("titel" = input$bericht.beleid.titel, "uitleg" = persberichten.alg.beleid$uitleg()))
              }
            }
          }
        # Persberichten beleid ------------------------------------------------
        # } else if (i == "Beleid") {
        #   volgorde$Beleid <- input$Persberichten.beleid
        #   for (j in volgorde$Beleid) {
        #     if (j == "Provinciebestuur") {
        #       volgorde$Beleid$Provinciebestuur <- list("Provinciebestuur" = list("titel" = input$bericht.kwartaal.titel, "uitleg" = input$bericht.kwartaal.uitleg))
        #     } else if (j == "Economie") {
        #       volgorde$Beleid$Economie <- list("Economie" = list("titel" = input$bericht.kwartaal.titel, "uitleg" = input$bericht.kwartaal.uitleg))
        #     } else if (j == "Leefmilieu") {
        #       volgorde$Beleid$Leefmilieu <- list("Leefmilieu" = list("titel" = input$bericht.kwartaal.titel, "uitleg" = input$bericht.kwartaal.uitleg))
        #     } else if (j == "Mobiliteit") {
        #       volgorde$Beleid$Mobiliteit <- list("Mobiliteit" = list("titel" = input$bericht.kwartaal.titel, "uitleg" = input$bericht.kwartaal.uitleg))
        #     } else if (j == "Onderwijs en Educatie") {
        #       volgorde$Beleid$"Onderwijs en Educatie" <- list("Onderwijs en Educatie" = list("titel" = input$bericht.kwartaal.titel, "uitleg" = input$bericht.kwartaal.uitleg))
        #     } else if (j == "Ruimte") {
        #       volgorde$Beleid$Ruimte <- list("Ruimte" = list("titel" = input$bericht.kwartaal.titel, "uitleg" = input$bericht.kwartaal.uitleg))
        #     } else if (j == "Vrije Tijd") {
        #       volgorde$Beleid$"Vrije Tijd" <- list("Vrije Tijd" = list("titel" = input$bericht.kwartaal.titel, "uitleg" = input$bericht.kwartaal.uitleg))
        #     } else if (j == "Gouverneur") {
        #       volgorde$Beleid$"Vrije Tijd" <- list("Gouverneur" = list("titel" = input$bericht.kwartaal.titel, "uitleg" = input$bericht.kwartaal.uitleg))
        #     }
        #   }
        }
        volgorde
      })
      
      
      output$results <- renderPrint({
        Rapport.volgorde()
        # Rapport.volgorde["Algemeen"]
        # input$Persberichten # This matches the input_id of the rank list
      })
      
      
      
  # VARIABLE COLLECTION FOR MARKDOWN (HTML) ===================================
    # Persberichten -----------------------------------------------------------
      # Algemeen --------------------------------------------------------------
      Persberichten.alg <- list(kwartaal.plot = reactive(persberichten.alg.kwartaal.plot()),
                                kwartaal.tabel = reactive(persberichten.alg.kwartaal.tabel()),
                                maand.plot = reactive(persberichten.alg.maand.plot()),
                                maand.tabel = reactive(persberichten.alg.maand.tabel()),
                                beleid.plot = reactive(persberichten.alg.beleid.plot()),
                                beleid.tabel = reactive(persberichten.alg.beleid.tabel()))
      # Per beleid ------------------------------------------------------------
        # Maand ---------------------------------------------------------------
        Persberichten.beleid.maand <- list(economie.plot = reactive(persberichten.beleid.maand.economie$plot()),
                                           economie.tabel = reactive(persberichten.beleid.maand.economie$tabel()),
                                           gouverneur.plot = reactive(persberichten.beleid.maand.gouverneur$plot()),
                                           gouverneur.tabel = reactive(persberichten.beleid.maand.gouverneur$tabel()),
                                           leefmilieu.plot = reactive(persberichten.beleid.maand.leefmilieu$plot()),
                                           leefmilieu.tabel = reactive(persberichten.beleid.maand.leefmilieu$tabel()),
                                           mobiliteit.plot = reactive(persberichten.beleid.maand.mobiliteit$plot()),
                                           mobiliteit.tabel = reactive(persberichten.beleid.maand.mobiliteit$tabel()),
                                           onderwijs.plot = reactive(persberichten.beleid.maand.onderwijs$plot()),
                                           onderwijs.tabel = reactive(persberichten.beleid.maand.onderwijs$tabel()),
                                           provinciebestuur.plot = reactive(persberichten.beleid.maand.provinciebestuur$plot()),
                                           provinciebestuur.tabel = reactive(persberichten.beleid.maand.provinciebestuur$tabel()),
                                           ruimte.plot = reactive(persberichten.beleid.maand.ruimte$plot()),
                                           ruimte.tabel = reactive(persberichten.beleid.maand.ruimte$tabel()),
                                           vrijetijd.plot = reactive(persberichten.beleid.maand.vrijetijd$plot()),
                                           vrijetijd.tabel = reactive(persberichten.beleid.maand.vrijetijd$tabel()))
        # Beleid --------------------------------------------------------------
        Persberichten.beleid.beleid <- list(economie.plot = reactive(persberichten.beleid.beleid.economie$plot()),
                                           economie.tabel = reactive(persberichten.beleid.beleid.economie$tabel()),
                                           gouverneur.plot = reactive(persberichten.beleid.beleid.gouverneur$plot()),
                                           gouverneur.tabel = reactive(persberichten.beleid.beleid.gouverneur$tabel()),
                                           leefmilieu.plot = reactive(persberichten.beleid.beleid.leefmilieu$plot()),
                                           leefmilieu.tabel = reactive(persberichten.beleid.beleid.leefmilieu$tabel()),
                                           mobiliteit.plot = reactive(persberichten.beleid.beleid.mobiliteit$plot()),
                                           mobiliteit.tabel = reactive(persberichten.beleid.beleid.mobiliteit$tabel()),
                                           onderwijs.plot = reactive(persberichten.beleid.beleid.onderwijs$plot()),
                                           onderwijs.tabel = reactive(persberichten.beleid.beleid.onderwijs$tabel()),
                                           provinciebestuur.plot = reactive(persberichten.beleid.beleid.provinciebestuur$plot()),
                                           provinciebestuur.tabel = reactive(persberichten.beleid.beleid.provinciebestuur$tabel()),
                                           ruimte.plot = reactive(persberichten.beleid.beleid.ruimte$plot()),
                                           ruimte.tabel = reactive(persberichten.beleid.beleid.ruimte$tabel()),
                                           vrijetijd.plot = reactive(persberichten.beleid.beleid.vrijetijd$plot()),
                                           vrijetijd.tabel = reactive(persberichten.beleid.beleid.vrijetijd$tabel()))
        # Verzender -----------------------------------------------------------
          # Algemeen ----------------------------------------------------------
          Persberichten.verzender.alg <- list(totaal.plot = reactive(persberichten.verzender.alg.totaal$plot()),
                                              totaal.tabel = reactive(persberichten.verzender.alg.totaal$tabel()),
                                              beleid.plot = reactive(persberichten.verzender.alg.beleid$plot()),
                                              beleid.tabel = reactive(persberichten.verzender.alg.beleid$plot()))
          # Per maand ---------------------------------------------------------
          Persberichten.verzender.maand <- list(persdienst.plot = reactive(persberichten.verzender.maand.persdienst$plot()),
                                                persdienst.tabel = reactive(persberichten.verzender.maand.persdienst$tabel()),
                                                provincie.plot = reactive(persberichten.verzender.maand.provincie$plot()),
                                                provincie.tabel = reactive(persberichten.verzender.maand.provincie$tabel()),
                                                gouverneur.plot = reactive(persberichten.verzender.maand.gouverneur$plot()),
                                                gouverneur.tabel = reactive(persberichten.verzender.maand.gouverneur$tabel()),
                                                extern.plot = reactive(persberichten.verzender.maand.extern$plot()),
                                                extern.tabel = reactive(persberichten.verzender.maand.extern$tabel()))
        # Type ----------------------------------------------------------------
        Persberichten.type <- list(type.plot = reactive(persberichten.type$plot()),
                                   type.tabel = reactive(persberichten.type$tabel()))
    # Persreturn --------------------------------------------------------------
      # Beleid ----------------------------------------------------------------
      Persreturn.beleid <- list(algemeen.plot = reactive(persreturn.beleid.alg$plot()),
                                algemeen.tabel = reactive(persreturn.beleid.alg$tabel()),
                                economie.plot = reactive(persreturn.beleid.economie$plot()),
                                economie.tabel = reactive(persreturn.beleid.alg$tabel()),
                                gouverneur.plot = reactive(persreturn.beleid.gouverneur$plot()),
                                gouverneur.tabel = reactive(persreturn.beleid.gouverneur$tabel()),
                                leefmilieu.plot = reactive(persreturn.beleid.leefmilieu$plot()),
                                leefmilieu.tabel = reactive(persreturn.beleid.leefmilieu$tabel()),
                                mobiliteit.plot = reactive(persreturn.beleid.mobiliteit$plot()),
                                mobiliteit.tabel = reactive(persreturn.beleid.mobiliteit$tabel()),
                                onderwijs.plot = reactive(persreturn.beleid.onderwijs$plot()),
                                onderwijs.tabel = reactive(persreturn.beleid.onderwijs$tabel()),
                                provinciebestuur.plot = reactive(persreturn.beleid.provinciebestuur$plot()),
                                provinciebestuur.tabel = reactive(persreturn.beleid.provinciebestuur$tabel()),
                                ruimte.plot = reactive(persreturn.beleid.ruimte$plot()),
                                ruimte.tabel = reactive(persreturn.beleid.ruimte$tabel()),
                                vrijetijd.plot = reactive(persreturn.beleid.vrijetijd$plot()),
                                vrijetijd.tabel = reactive(persreturn.beleid.vrijetijd$tabel()))
      # Medium ----------------------------------------------------------------
      Persreturn.medium <- list(medium.plot = reactive(persreturn.medium$plot()),
                                medium.tabel = reactive(persreturn.medium$tabel()))
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
      
      # Set up parameters to pass to Rmd document
      params <- list(titel = paste("Persstatistiek:", paste(input$jaar, input$kwartaal, sep= " - ")),
                     Persberichten.alg = Persberichten.alg,
                     Persberichten.beleid.maand = Persberichten.beleid.maand,
                     Persberichten.beleid.beleid = Persberichten.beleid.beleid,
                     Persberichten.verzender.alg = Persberichten.verzender.alg,
                     Persberichten.verzender.maand = Persberichten.verzender.maand,
                     Persberichten.type = Persberichten.type,
                     Persreturn.beleid = Persreturn.beleid,
                     Persreturn.medium = Persreturn.medium)
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
  # ===========================================================================
}