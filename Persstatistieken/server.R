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
  # Data Preparation --------------------------------------------------------
  source("./Functions/Data_Preparation.R")
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
    # ALGEMEEN ----------------------------------------------------------------
      # Per Kwartaal ----------------------------------------------------------
        # Barplot
          source("./Functions/Persberichten/Algemeen/Per_kwartaal/kwartaal_barplot.R")
          persberichten.alg.kwartaal.plot <- bericht.alg.kwartaal.barplot(reactive(Persstatistiek()))
          output$persberichten.alg.kwartaal.plot <- renderPlot(
            persberichten.alg.kwartaal.plot()
          )
        # Tabel
          source("./Functions/Persberichten/Algemeen/Per_kwartaal/kwartaal_tabel.R")
          persberichten.alg.kwartaal.tabel <- bericht.alg.kwartaal.tabel(reactive(Persstatistiek()))
          output$persberichten.alg.kwartaal.tabel <- renderTable(
            persberichten.alg.kwartaal.tabel()
          )
      # Per Maand -------------------------------------------------------------
        # Barplot
          source("./Functions/Persberichten/Algemeen/Per_maand/maand_barplot.R")
          persberichten.alg.maand.plot <- bericht.alg.maand.barplot(reactive(Persstatistiek()))
          output$persberichten.alg.maand.plot <- renderPlot(
            persberichten.alg.maand.plot()
          )
        # Tabel
          source("./Functions/Persberichten/Algemeen/Per_maand/maand_tabel.R")
          persberichten.alg.maand.tabel <- bericht.alg.maand.tabel(reactive(Persstatistiek()))
          output$persberichten.alg.maand.tabel <- renderTable(
            persberichten.alg.maand.tabel()
          )
      # Per Beleid ------------------------------------------------------------
        # Barplot
          source("./Functions/Persberichten/Algemeen/Per_beleid/beleid_barplot.R")
          persberichten.alg.beleid.plot <- bericht.alg.beleid.barplot(reactive(Persstatistiek()))
          output$persberichten.alg.beleid.plot <- renderPlot(
            persberichten.alg.beleid.plot()
          )
        # Tabel
          source("./Functions/Persberichten/Algemeen/Per_beleid/beleid_tabel.R")
          persberichten.alg.beleid.tabel <- bericht.alg.beleid.tabel(reactive(Persstatistiek()))
          output$persberichten.alg.beleid.tabel <- renderTable(
            persberichten.alg.beleid.tabel()
          )
    # PER BELEID --------------------------------------------------------------
      # Per Maand -------------------------------------------------------------
        # Barplot -------------------------------------------------------------
          source("./Functions/Persberichten/Per_beleid/Per_maand/maand_beleid_barplot.R")
          # Economie
          persberichten.beleid.maand.economie.plot <- bericht.beleid.maand.barplot(reactive(Persstatistiek()), reactive("Economie"))
          output$persberichten.beleid.maand.economie.plot <- renderPlot(
            persberichten.beleid.maand.economie.plot()
          )
          # Gouverneur
          persberichten.beleid.maand.gouverneur.plot <- bericht.beleid.maand.barplot(reactive(Persstatistiek()), reactive("Gouverneur"))
          output$persberichten.beleid.maand.gouverneur.plot <- renderPlot(
            persberichten.beleid.maand.gouverneur.plot()
          )
          # Leefmilieu
          persberichten.beleid.maand.leefmilieu.plot <- bericht.beleid.maand.barplot(reactive(Persstatistiek()), reactive("Leefmilieu"))
          output$persberichten.beleid.maand.leefmilieu.plot <- renderPlot(
            persberichten.beleid.maand.leefmilieu.plot()
          )
          # Mobiliteit
          persberichten.beleid.maand.mobiliteit.plot <- bericht.beleid.maand.barplot(reactive(Persstatistiek()), reactive("Mobiliteit"))
          output$persberichten.beleid.maand.mobiliteit.plot <- renderPlot(
            persberichten.beleid.maand.mobiliteit.plot()
          )
          # Onderwijs en Educatie
          persberichten.beleid.maand.onderwijs.plot <- bericht.beleid.maand.barplot(reactive(Persstatistiek()), reactive("Onderwijs en Educatie"))
          output$persberichten.beleid.maand.onderwijs.plot <- renderPlot(
            persberichten.beleid.maand.onderwijs.plot()
          )
          # Provinciebestuur
          persberichten.beleid.maand.provinciebestuur.plot <- bericht.beleid.maand.barplot(reactive(Persstatistiek()), reactive("Provinciebestuur"))
          output$persberichten.beleid.maand.provinciebestuur.plot <- renderPlot(
            persberichten.beleid.maand.provinciebestuur.plot()
          )
          # Ruimte
          persberichten.beleid.maand.ruimte.plot <- bericht.beleid.maand.barplot(reactive(Persstatistiek()), reactive("Ruimte"))
          output$persberichten.beleid.maand.ruimte.plot <- renderPlot(
            persberichten.beleid.maand.ruimte.plot()
          )
          # Vrije Tijd
          persberichten.beleid.maand.vrijetijd.plot <- bericht.beleid.maand.barplot(reactive(Persstatistiek()), reactive("Vrije Tijd"))
          output$persberichten.beleid.maand.vrijetijd.plot <- renderPlot(
            persberichten.beleid.maand.vrijetijd.plot()
          )
        # Tabel ---------------------------------------------------------------
          source("./Functions/Persberichten/Per_beleid/Per_maand/maand_beleid_tabel.R")
          # Economie
          persberichten.beleid.maand.economie.tabel <- bericht.beleid.maand.tabel(reactive(Persstatistiek()), reactive("Economie"))
          output$persberichten.beleid.maand.economie.tabel <- renderTable(
            persberichten.beleid.maand.economie.tabel()
          )
          # Gouverneur
          persberichten.beleid.maand.gouverneur.tabel <- bericht.beleid.maand.tabel(reactive(Persstatistiek()), reactive("Gouverneur"))
          output$persberichten.beleid.maand.gouverneur.tabel <- renderTable(
            persberichten.beleid.maand.gouverneur.tabel()
          )
          # Leefmilieu
          persberichten.beleid.maand.leefmilieu.tabel <- bericht.beleid.maand.tabel(reactive(Persstatistiek()), reactive("Leefmilieu"))
          output$persberichten.beleid.maand.leefmilieu.tabel <- renderTable(
            persberichten.beleid.maand.leefmilieu.tabel()
          )
          # Mobiliteit
          persberichten.beleid.maand.mobiliteit.tabel <- bericht.beleid.maand.tabel(reactive(Persstatistiek()), reactive("Mobiliteit"))
          output$persberichten.beleid.maand.mobiliteit.tabel <- renderTable(
            persberichten.beleid.maand.mobiliteit.tabel()
          )
          # Onderwijs en Educatie
          persberichten.beleid.maand.onderwijs.tabel <- bericht.beleid.maand.tabel(reactive(Persstatistiek()), reactive("Onderwijs en Educatie"))
          output$persberichten.beleid.maand.onderwijs.tabel <- renderTable(
            persberichten.beleid.maand.onderwijs.tabel()
          )
          # Provinciebestuur
          persberichten.beleid.maand.provinciebestuur.tabel <- bericht.beleid.maand.tabel(reactive(Persstatistiek()), reactive("Provinciebestuur"))
          output$persberichten.beleid.maand.provinciebestuur.tabel <- renderTable(
            persberichten.beleid.maand.provinciebestuur.tabel()
          )
          # Ruimte
          persberichten.beleid.maand.ruimte.tabel <- bericht.beleid.maand.tabel(reactive(Persstatistiek()), reactive("Ruimte"))
          output$persberichten.beleid.maand.ruimte.tabel <- renderTable(
            persberichten.beleid.maand.ruimte.tabel()
          )
          # Vrije Tijd
          persberichten.beleid.maand.vrijetijd.tabel <- bericht.beleid.maand.tabel(reactive(Persstatistiek()), reactive("Vrije Tijd"))
          output$persberichten.beleid.maand.vrijetijd.tabel <- renderTable(
            persberichten.beleid.maand.vrijetijd.tabel()
          )
      # Per Deelbeleid --------------------------------------------------------
        # Barplot -------------------------------------------------------------
          source("./Functions/Persberichten/Per_beleid/Per_beleid/beleid_detail_barplot.R")
          # Economie
          persberichten.beleid.beleid.economie.plot <- bericht.beleid.beleid.barplot(reactive(Persstatistiek()), reactive("Economie"))
          output$persberichten.beleid.beleid.economie.plot <- renderPlot(
            persberichten.beleid.beleid.economie.plot()
          )
          # Gouverneur
          persberichten.beleid.beleid.gouverneur.plot <- bericht.beleid.beleid.barplot(reactive(Persstatistiek()), reactive("Gouverneur"))
          output$persberichten.beleid.beleid.gouverneur.plot <- renderPlot(
            persberichten.beleid.beleid.gouverneur.plot()
          )
          # Leefmilieu
          persberichten.beleid.beleid.leefmilieu.plot <- bericht.beleid.beleid.barplot(reactive(Persstatistiek()), reactive("Leefmilieu"))
          output$persberichten.beleid.beleid.leefmilieu.plot <- renderPlot(
            persberichten.beleid.beleid.leefmilieu.plot()
          )
          # Mobiliteit
          persberichten.beleid.beleid.mobiliteit.plot <- bericht.beleid.beleid.barplot(reactive(Persstatistiek()), reactive("Mobiliteit"))
          output$persberichten.beleid.beleid.mobiliteit.plot <- renderPlot(
            persberichten.beleid.beleid.mobiliteit.plot()
          )
          # Onderwijs en Educatie
          persberichten.beleid.beleid.onderwijs.plot <- bericht.beleid.beleid.barplot(reactive(Persstatistiek()), reactive("Onderwijs en Educatie"))
          output$persberichten.beleid.beleid.onderwijs.plot <- renderPlot(
            persberichten.beleid.beleid.onderwijs.plot()
          )
          # Provinciebestuur
          persberichten.beleid.beleid.provinciebestuur.plot <- bericht.beleid.beleid.barplot(reactive(Persstatistiek()), reactive("Provinciebestuur"))
          output$persberichten.beleid.beleid.provinciebestuur.plot <- renderPlot(
            persberichten.beleid.beleid.provinciebestuur.plot()
          )
          # Ruimte
          persberichten.beleid.beleid.ruimte.plot <- bericht.beleid.beleid.barplot(reactive(Persstatistiek()), reactive("Ruimte"))
          output$persberichten.beleid.beleid.ruimte.plot <- renderPlot(
            persberichten.beleid.beleid.ruimte.plot()
          )
          # Vrije Tijd
          persberichten.beleid.beleid.vrijetijd.plot <- bericht.beleid.beleid.barplot(reactive(Persstatistiek()), reactive("Vrije Tijd"))
          output$persberichten.beleid.beleid.vrijetijd.plot <- renderPlot(
            persberichten.beleid.beleid.vrijetijd.plot()
          )
        # Tabel ---------------------------------------------------------------
          source("./Functions/Persberichten/Per_beleid/Per_beleid/beleid_detail_tabel.R")
          # Economie
          persberichten.beleid.beleid.economie.tabel <- bericht.beleid.beleid.tabel(reactive(Persstatistiek()), reactive("Economie"))
          output$persberichten.beleid.beleid.economie.tabel <- renderTable(
            persberichten.beleid.beleid.economie.tabel()
          )
          # Gouverneur
          persberichten.beleid.beleid.gouverneur.tabel <- bericht.beleid.beleid.tabel(reactive(Persstatistiek()), reactive("Gouverneur"))
          output$persberichten.beleid.beleid.gouverneur.tabel <- renderTable(
            persberichten.beleid.beleid.gouverneur.tabel()
          )
          # Leefmilieu
          persberichten.beleid.beleid.leefmilieu.tabel <- bericht.beleid.beleid.tabel(reactive(Persstatistiek()), reactive("Leefmilieu"))
          output$persberichten.beleid.beleid.leefmilieu.tabel <- renderTable(
            persberichten.beleid.beleid.leefmilieu.tabel()
          )
          # Mobiliteit
          persberichten.beleid.beleid.mobiliteit.tabel <- bericht.beleid.beleid.tabel(reactive(Persstatistiek()), reactive("Mobiliteit"))
          output$persberichten.beleid.beleid.mobiliteit.tabel <- renderTable(
            persberichten.beleid.beleid.mobiliteit.tabel()
          )
          # Onderwijs en Educatie
          persberichten.beleid.beleid.onderwijs.tabel <- bericht.beleid.beleid.tabel(reactive(Persstatistiek()), reactive("Onderwijs en Educatie"))
          output$persberichten.beleid.beleid.onderwijs.tabel <- renderTable(
            persberichten.beleid.beleid.onderwijs.tabel()
          )
          # Provinciebestuur
          persberichten.beleid.beleid.provinciebestuur.tabel <- bericht.beleid.beleid.tabel(reactive(Persstatistiek()), reactive("Provinciebestuur"))
          output$persberichten.beleid.beleid.provinciebestuur.tabel <- renderTable(
            persberichten.beleid.beleid.provinciebestuur.tabel()
          )
          # Ruimte
          persberichten.beleid.beleid.ruimte.tabel <- bericht.beleid.beleid.tabel(reactive(Persstatistiek()), reactive("Ruimte"))
          output$persberichten.beleid.beleid.ruimte.tabel <- renderTable(
            persberichten.beleid.beleid.ruimte.tabel()
          )
          # Vrije Tijd
          persberichten.beleid.beleid.vrijetijd.tabel <- bericht.beleid.beleid.tabel(reactive(Persstatistiek()), reactive("Vrije Tijd"))
          output$persberichten.beleid.beleid.vrijetijd.tabel <- renderTable(
            persberichten.beleid.beleid.vrijetijd.tabel()
          )
    # PER VERZENDER -----------------------------------------------------------
      # Algemeen --------------------------------------------------------------
        # Totaal per Verzender ------------------------------------------------
          # Barplot -----------------------------------------------------------
          source("./Functions/Persberichten/Per_Verzender/Algemeen/Verzender_totaal/verzender_totaal_barplot.R")
          persberichten.verzender.alg.totaal.plot <- bericht.verzender.alg.totaal.barplot(reactive(Persstatistiek()))
          output$persberichten.verzender.alg.totaal.plot <- renderPlot(
            persberichten.verzender.alg.totaal.plot()
          )
          # Tabel -------------------------------------------------------------
          source("./Functions/Persberichten/Per_Verzender/Algemeen/Verzender_totaal/verzender_totaal_tabel.R")
          persberichten.verzender.alg.totaal.tabel <- bericht.verzender.alg.totaal.tabel(reactive(Persstatistiek()))
          output$persberichten.verzender.alg.totaal.tabel <- renderTable(
            persberichten.verzender.alg.totaal.tabel()
          )
        # Beleid per Verzender ------------------------------------------------
          # Barlot ------------------------------------------------------------
          source("./Functions/Persberichten/Per_Verzender/Algemeen/Verzender_beleid/verzender_beleid_barplot.R")
          persberichten.verzender.alg.beleid.plot <- bericht.verzender.alg.beleid.barplot(reactive(Persstatistiek()))
          output$persberichten.verzender.alg.beleid.plot <- renderPlot(
            persberichten.verzender.alg.beleid.plot()
          )
          # Tabel -------------------------------------------------------------
          source("./Functions/Persberichten/Per_Verzender/Algemeen/Verzender_beleid/verzender_beleid_tabel.R")
          persberichten.verzender.alg.beleid.tabel <- bericht.verzender.alg.beleid.tabel(reactive(Persstatistiek()))
          output$persberichten.verzender.alg.beleid.tabel <- renderTable(
            persberichten.verzender.alg.beleid.tabel()
          )
      # Per Maand -------------------------------------------------------------
        # Barlot ------------------------------------------------------------
          source("./Functions/Persberichten/Per_Verzender/Per_maand/verzender_maand_barplot.R")
          # Persdienst
          persberichten.verzender.maand.persdienst.plot <- bericht.verzender.maand.barplot(reactive(Persstatistiek()), reactive("Persdienst"))
          output$persberichten.verzender.maand.persdienst.plot <- renderPlot(
            persberichten.verzender.maand.persdienst.plot()
          )
          # Provincie
          persberichten.verzender.maand.provincie.plot <- bericht.verzender.maand.barplot(reactive(Persstatistiek()), reactive("Provincie"))
          output$persberichten.verzender.maand.provincie.plot <- renderPlot(
            persberichten.verzender.maand.provincie.plot()
          )
          # Gouverneur
          persberichten.verzender.maand.gouverneur.plot <- bericht.verzender.maand.barplot(reactive(Persstatistiek()), reactive("Gouverneur"))
          output$persberichten.verzender.maand.gouverneur.plot <- renderPlot(
            persberichten.verzender.maand.gouverneur.plot()
          )
          # Extern
          persberichten.verzender.maand.extern.plot <- bericht.verzender.maand.barplot(reactive(Persstatistiek()), reactive("Extern"))
          output$persberichten.verzender.maand.extern.plot <- renderPlot(
            persberichten.verzender.maand.extern.plot()
          )
        # Tabel -------------------------------------------------------------
          source("./Functions/Persberichten/Per_Verzender/Per_maand/verzender_maand_tabel.R")
          # Persdienst
          persberichten.verzender.maand.persdienst.tabel <- bericht.verzender.maand.tabel(reactive(Persstatistiek()), reactive("Persdienst"))
          output$persberichten.verzender.maand.persdienst.tabel <- renderTable(
            persberichten.verzender.maand.persdienst.tabel()
          )
          # Provincie
          persberichten.verzender.maand.provincie.tabel <- bericht.verzender.maand.tabel(reactive(Persstatistiek()), reactive("Provincie"))
          output$persberichten.verzender.maand.provincie.tabel <- renderTable(
            persberichten.verzender.maand.provincie.tabel()
          )
          # Gouverneur
          persberichten.verzender.maand.gouverneur.tabel <- bericht.verzender.maand.tabel(reactive(Persstatistiek()), reactive("Gouverneur"))
          output$persberichten.verzender.maand.gouverneur.tabel <- renderTable(
            persberichten.verzender.maand.gouverneur.tabel()
          )
          # Extern
          persberichten.verzender.maand.extern.tabel <- bericht.verzender.maand.tabel(reactive(Persstatistiek()), reactive("Extern"))
          output$persberichten.verzender.maand.extern.tabel <- renderTable(
            persberichten.verzender.maand.extern.tabel()
          )
    # PER TYPE ----------------------------------------------------------------
      # Barplot ---------------------------------------------------------------
      source("./Functions/Persberichten/Per_type/type_barplot.R")
      persberichten.type.plot <- bericht.type.barplot(reactive(Persstatistiek()))
      output$persberichten.type.plot <- renderPlot(
        persberichten.type.plot()
      )
      # Tabel -----------------------------------------------------------------
      source("./Functions/Persberichten/Per_type/type_tabel.R")
      persberichten.type.tabel <- bericht.type.tabel(reactive(Persstatistiek()))
      output$persberichten.type.tabel <- renderTable(
        persberichten.type.tabel()
      )
  # ===========================================================================
  
  # PERSRETURN ================================================================
    # PER BELEID --------------------------------------------------------------
      # Algemeen --------------------------------------------------------------
        # Barplot -------------------------------------------------------------
        source("./Functions/Persreturn/Per_beleid/Algemeen/return_beleid_barplot.R")
        persreturn.beleid.alg.plot <- persreturn.beleid.barplot(reactive(Persstatistiek()))
        output$persreturn.beleid.alg.plot <- renderPlot(
          persreturn.beleid.alg.plot()
        )
        # Tabel ---------------------------------------------------------------
        source("./Functions/Persreturn/Per_beleid/Algemeen/return_beleid_tabel.R")
        persreturn.beleid.alg.tabel <- persreturn.beleid.tabel(reactive(Persstatistiek()))
        output$persreturn.beleid.alg.tabel <- renderPlot(
          persreturn.beleid.alg.tabel()
        )
      # Deelbeleid ------------------------------------------------------------
        # Barplot -------------------------------------------------------------
        source("./Functions/Persreturn/Per_beleid/Detail/return_deelbeleid_barplot.R")
          # Economie
          persreturn.beleid.economie.plot <- persreturn.deelbeleid.barplot(reactive(Persstatistiek()), reactive("Economie"))
          output$persreturn.beleid.economie.plot <- renderPlot(
            persreturn.beleid.economie.plot()
          )
          # Gouverneur
          persreturn.beleid.gouverneur.plot <- persreturn.deelbeleid.barplot(reactive(Persstatistiek()), reactive("Gouverneur"))
          output$persreturn.beleid.gouverneur.plot <- renderPlot(
            persreturn.beleid.gouverneur.plot()
          )
          # Leefmilieu
          persreturn.beleid.leefmilieu.plot <- persreturn.deelbeleid.barplot(reactive(Persstatistiek()), reactive("Leefmilieu"))
          output$persreturn.beleid.leefmilieu.plot <- renderPlot(
            persreturn.beleid.leefmilieu.plot()
          )
          # Mobiliteit
          persreturn.beleid.mobiliteit.plot <- persreturn.deelbeleid.barplot(reactive(Persstatistiek()), reactive("Mobiliteit"))
          output$persreturn.beleid.mobiliteit.plot <- renderPlot(
            persreturn.beleid.mobiliteit.plot()
          )
          # Onderwijs en Educatie
          persreturn.beleid.onderwijs.plot <- persreturn.deelbeleid.barplot(reactive(Persstatistiek()), reactive("Onderwijs en Educatie"))
          output$persreturn.beleid.onderwijs.plot <- renderPlot(
            persreturn.beleid.onderwijs.plot()
          )
          # Provinciebestuur
          persreturn.beleid.provinciebestuur.plot <- persreturn.deelbeleid.barplot(reactive(Persstatistiek()), reactive("Provinciebestuur"))
          output$persreturn.beleid.provinciebestuur.plot <- renderPlot(
            persreturn.beleid.provinciebestuur.plot()
          )
          # Ruimte
          persreturn.beleid.ruimte.plot <- persreturn.deelbeleid.barplot(reactive(Persstatistiek()), reactive("Ruimte"))
          output$persreturn.beleid.ruimte.plot <- renderPlot(
            persreturn.beleid.ruimte.plot()
          )
          # Vrije Tijd
          persreturn.beleid.vrijetijd.plot <- persreturn.deelbeleid.barplot(reactive(Persstatistiek()), reactive("Vrije Tijd"))
          output$persreturn.beleid.vrijetijd.plot <- renderPlot(
            persreturn.beleid.vrijetijd.plot()
          )
        # Tabel ---------------------------------------------------------------
        source("./Functions/Persreturn/Per_beleid/Detail/return_deelbeleid_tabel.R")
          # Economie
          persreturn.beleid.economie.tabel <- persreturn.deelbeleid.tabel(reactive(Persstatistiek()), reactive("Economie"))
          output$persreturn.beleid.economie.tabel <- renderTable(
            persreturn.beleid.economie.tabel()
          )
          # Gouverneur
          persreturn.beleid.gouverneur.tabel <- persreturn.deelbeleid.tabel(reactive(Persstatistiek()), reactive("Gouverneur"))
          output$persreturn.beleid.gouverneur.tabel <- renderTable(
            persreturn.beleid.gouverneur.tabel()
          )
          # Leefmilieu
          persreturn.beleid.leefmilieu.tabel <- persreturn.deelbeleid.tabel(reactive(Persstatistiek()), reactive("Leefmilieu"))
          output$persreturn.beleid.leefmilieu.tabel <- renderTable(
            persreturn.beleid.leefmilieu.tabel()
          )
          # Mobiliteit
          persreturn.beleid.mobiliteit.tabel <- persreturn.deelbeleid.tabel(reactive(Persstatistiek()), reactive("Mobiliteit"))
          output$persreturn.beleid.mobiliteit.tabel <- renderTable(
            persreturn.beleid.mobiliteit.tabel()
          )
          # Onderwijs en Educatie
          persreturn.beleid.onderwijs.tabel <- persreturn.deelbeleid.tabel(reactive(Persstatistiek()), reactive("Onderwijs en Educatie"))
          output$persreturn.beleid.onderwijs.tabel <- renderTable(
            persreturn.beleid.onderwijs.tabel()
          )
          # Provinciebestuur
          persreturn.beleid.provinciebestuur.tabel <- persreturn.deelbeleid.tabel(reactive(Persstatistiek()), reactive("Provinciebestuur"))
          output$persreturn.beleid.provinciebestuur.tabel <- renderTable(
            persreturn.beleid.provinciebestuur.tabel()
          )
          # Ruimte
          persreturn.beleid.ruimte.tabel <- persreturn.deelbeleid.tabel(reactive(Persstatistiek()), reactive("Ruimte"))
          output$persreturn.beleid.ruimte.tabel <- renderTable(
            persreturn.beleid.ruimte.tabel()
          )
          # Vrije Tijd
          persreturn.beleid.vrijetijd.tabel <- persreturn.deelbeleid.tabel(reactive(Persstatistiek()), reactive("Vrije Tijd"))
          output$persreturn.beleid.vrijetijd.tabel <- renderTable(
            persreturn.beleid.vrijetijd.tabel()
          )
    # PER PLATFORM ------------------------------------------------------------
      # Barplot ---------------------------------------------------------------
      source("./Functions/Persreturn/Per_platform/return_platform_barplot.R")
      persreturn.platform.plot <- return.platform.barplot(reactive(Persstatistiek()))
      output$persreturn.platform.plot <- renderPlot(
        persreturn.platform.plot()
      )
      # Tabel -----------------------------------------------------------------
      source("./Functions/Persreturn/Per_platform/return_platform_tabel.R")
      persreturn.platform.tabel <- return.platform.tabel(reactive(Persstatistiek()))
      output$persreturn.platform.tabel <- renderTable(
        persreturn.platform.tabel()
      )
  # ===========================================================================
      
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
        Persberichten.beleid.maand <- list(economie.plot = reactive(persberichten.beleid.maand.economie.plot()),
                                           economie.tabel = reactive(persberichten.beleid.maand.economie.tabel()),
                                           gouverneur.plot = reactive(persberichten.beleid.maand.gouverneur.plot()),
                                           gouverneur.tabel = reactive(persberichten.beleid.maand.gouverneur.tabel()),
                                           leefmilieu.plot = reactive(persberichten.beleid.maand.leefmilieu.plot()),
                                           leefmilieu.tabel = reactive(persberichten.beleid.maand.leefmilieu.tabel()),
                                           mobiliteit.plot = reactive(persberichten.beleid.maand.mobiliteit.plot()),
                                           mobiliteit.tabel = reactive(persberichten.beleid.maand.mobiliteit.tabel()),
                                           onderwijs.plot = reactive(persberichten.beleid.maand.onderwijs.plot()),
                                           onderwijs.tabel = reactive(persberichten.beleid.maand.onderwijs.tabel()),
                                           provinciebestuur.plot = reactive(persberichten.beleid.maand.provinciebestuur.plot()),
                                           provinciebestuur.tabel = reactive(persberichten.beleid.maand.provinciebestuur.tabel()),
                                           ruimte.plot = reactive(persberichten.beleid.maand.ruimte.plot()),
                                           ruimte.tabel = reactive(persberichten.beleid.maand.ruimte.tabel()),
                                           vrijetijd.plot = reactive(persberichten.beleid.maand.vrijetijd.plot()),
                                           vrijetijd.tabel = reactive(persberichten.beleid.maand.vrijetijd.tabel()))
        # Beleid --------------------------------------------------------------
        Persberichten.beleid.beleid <- list(economie.plot = reactive(persberichten.beleid.beleid.economie.plot()),
                                           economie.tabel = reactive(persberichten.beleid.beleid.economie.tabel()),
                                           gouverneur.plot = reactive(persberichten.beleid.beleid.gouverneur.plot()),
                                           gouverneur.tabel = reactive(persberichten.beleid.beleid.gouverneur.tabel()),
                                           leefmilieu.plot = reactive(persberichten.beleid.beleid.leefmilieu.plot()),
                                           leefmilieu.tabel = reactive(persberichten.beleid.beleid.leefmilieu.tabel()),
                                           mobiliteit.plot = reactive(persberichten.beleid.beleid.mobiliteit.plot()),
                                           mobiliteit.tabel = reactive(persberichten.beleid.beleid.mobiliteit.tabel()),
                                           onderwijs.plot = reactive(persberichten.beleid.beleid.onderwijs.plot()),
                                           onderwijs.tabel = reactive(persberichten.beleid.beleid.onderwijs.tabel()),
                                           provinciebestuur.plot = reactive(persberichten.beleid.beleid.provinciebestuur.plot()),
                                           provinciebestuur.tabel = reactive(persberichten.beleid.beleid.provinciebestuur.tabel()),
                                           ruimte.plot = reactive(persberichten.beleid.beleid.ruimte.plot()),
                                           ruimte.tabel = reactive(persberichten.beleid.beleid.ruimte.tabel()),
                                           vrijetijd.plot = reactive(persberichten.beleid.beleid.vrijetijd.plot()),
                                           vrijetijd.tabel = reactive(persberichten.beleid.beleid.vrijetijd.tabel()))
        # Verzender -----------------------------------------------------------
          # Algemeen ----------------------------------------------------------
          Persberichten.verzender.alg <- list(totaal.plot = reactive(persberichten.verzender.alg.totaal.plot()),
                                              totaal.tabel = reactive(persberichten.verzender.alg.totaal.tabel()),
                                              beleid.plot = reactive(persberichten.verzender.alg.beleid.plot()),
                                              beleid.tabel = reactive(persberichten.verzender.alg.beleid.plot()))
          # Per maand ---------------------------------------------------------
          Persberichten.verzender.maand <- list(persdienst.plot = reactive(persberichten.verzender.maand.persdienst.plot()),
                                                persdienst.tabel = reactive(persberichten.verzender.maand.persdienst.tabel()),
                                                provincie.plot = reactive(persberichten.verzender.maand.provincie.plot()),
                                                provincie.tabel = reactive(persberichten.verzender.maand.provincie.tabel()),
                                                gouverneur.plot = reactive(persberichten.verzender.maand.gouverneur.plot()),
                                                gouverneur.tabel = reactive(persberichten.verzender.maand.gouverneur.tabel()),
                                                extern.plot = reactive(persberichten.verzender.maand.extern.plot()),
                                                extern.tabel = reactive(persberichten.verzender.maand.extern.tabel()))
        # Type ----------------------------------------------------------------
        Persberichten.type <- list(type.plot = reactive(persberichten.type.plot()),
                                   type.tabel = reactive(persberichten.type.tabel()))
    # Persreturn --------------------------------------------------------------
      # Beleid ----------------------------------------------------------------
      Persreturn.beleid <- list(algemeen.plot = reactive(persreturn.beleid.alg.plot()),
                                algemeen.tabel = reactive(persreturn.beleid.alg.tabel()),
                                economie.plot = reactive(persreturn.beleid.economie.plot()),
                                economie.tabel = reactive(persreturn.beleid.alg.tabel()),
                                gouverneur.plot = reactive(persreturn.beleid.gouverneur.plot()),
                                gouverneur.tabel = reactive(persreturn.beleid.gouverneur.tabel()),
                                leefmilieu.plot = reactive(persreturn.beleid.leefmilieu.plot()),
                                leefmilieu.tabel = reactive(persreturn.beleid.leefmilieu.tabel()),
                                mobiliteit.plot = reactive(persreturn.beleid.mobiliteit.plot()),
                                mobiliteit.tabel = reactive(persreturn.beleid.mobiliteit.tabel()),
                                onderwijs.plot = reactive(persreturn.beleid.onderwijs.plot()),
                                onderwijs.tabel = reactive(persreturn.beleid.onderwijs.tabel()),
                                provinciebestuur.plot = reactive(persreturn.beleid.provinciebestuur.plot()),
                                provinciebestuur.tabel = reactive(persreturn.beleid.provinciebestuur.tabel()),
                                ruimte.plot = reactive(persreturn.beleid.ruimte.plot()),
                                ruimte.tabel = reactive(persreturn.beleid.ruimte.tabel()),
                                vrijetijd.plot = reactive(persreturn.beleid.vrijetijd.plot()),
                                vrijetijd.tabel = reactive(persreturn.beleid.vrijetijd.tabel()))
      # Platform --------------------------------------------------------------
      Persreturn.platform <- list(platform.plot = reactive(persreturn.platform.plot()),
                                  platform.tabel = reactive(persreturn.platform.tabel()))
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
                     Persreturn.platform = Persreturn.platform)
      
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