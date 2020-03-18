###############################################################################
# SHINY APP (Persstatistiek): SERVER
###############################################################################

server <- function(input, output) {
  
  # Packages
  library(knitr)
  library(RColorBrewer)
  
  # Functions
  source("./Functions/Persreturn_per_beleid_Table.R")
  source("./Functions/Persreturn_per_beleid_Barplot.R")
  
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
        # Totaal per Verzender
          # Barplot
          # Tabel
        # Beleid per Verzender
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
  
  # HTML RAPPORT AANMAAK ======================================================
  
  # ===========================================================================
  
  

  # Persreturn -------------------------------------------------------------      
  # Per beleid -----------------------------------------------------------
  # Preparation --------------------------------------------------------
  df.return.beleid <- reactive({
    # Create dataframe for barplot ---------------------------------
    return.beleid <- data.frame(table(Persstatistiek()$Beleid,
                                      Persstatistiek()$Persreturn))
    # Rename columns -----------------------------------------------
    colnames(return.beleid) <- c("Beleid", "Persreturn", "Freq")
    
    return(return.beleid)
  })
  # Barplot ------------------------------------------------------------
  persreturn.beleid.barplot <- Persreturn.beleid.barplot(reactive(df.return.beleid()), reactive("Persreturn per beleid"), reactive("Beleid"))
  output$persreturn.beleid.barplot <- renderPlot({
    persreturn.beleid.barplot()
  })
  # Table --------------------------------------------------------------
  persreturn.beleid.tabel <- Persreturn.beleid.tabel(reactive(df.return.beleid()), reactive("Persreturn per beleid"), reactive("Beleid"))
  output$persreturn.beleid.tabel <- renderTable({
    persreturn.beleid.tabel()
  })
  # Per beleid detail-----------------------------------------------------
  # Preparation --------------------------------------------------------
  df.Persreturn.beleid.detail <- reactive({
    Persreturn <- split(Persstatistiek(), Persstatistiek()$Beleid)
    
    for (i in levels(Persstatistiek()$Beleid)) ({
      Persreturn[[i]] <- data.frame(table(Persreturn[[i]]$"Detail beleid", Persreturn[[i]]$Persreturn))
      Persreturn[[i]] <- cbind(Beleid = i, Persreturn[[i]])
      colnames(Persreturn[[i]]) <- c("Beleid","Detail", "Persreturn", "Freq")
      
      # Fix missing freq
      temp <- Persreturn[[i]]
      temp$Persreturn <- as.factor(temp$Persreturn)
      if(!("Ja" %in% levels(temp$Persreturn))) {
        df.Ja <- Persreturn[[i]]
        df.Ja$Persreturn <- "Ja"
        df.Ja$Freq <- 0
        Persreturn[[i]] <- rbind(df.Ja, Persreturn[[i]])
      } else if(!("Nee" %in% levels(temp$Persreturn))) {
        df.Nee <- Persreturn[[i]]
        df.Nee$Persreturn <- "Nee"
        df.Nee$Freq <- 0
        Persreturn[[i]] <- rbind(df.Nee, Persreturn[[i]])
      }
    })
    return(Persreturn)
  })
  # Barplots & Tables --------------------------------------------------
  # Economie ---------------------------------------------------------
  # Barplot ---------------------------------------------------------
  persreturn.beleid.economie.barplot <- Persreturn.beleid.barplot(reactive(df.Persreturn.beleid.detail()$Economie), reactive("Persreturn: Economie"), reactive("Detail"))
  output$persreturn.beleid.economie.barplot <- renderPlot({
    persreturn.beleid.economie.barplot()
  })
  # Table -----------------------------------------------------------
  persreturn.beleid.economie.tabel <- Persreturn.beleid.tabel(reactive(df.Persreturn.beleid.detail()$Economie), reactive("Persreturn: Economie"), reactive("Detail"))
  output$persreturn.beleid.economie.tabel <- renderTable({
    persreturn.beleid.economie.tabel()
  })
  # Gouverneur -------------------------------------------------------
  # Barplot ---------------------------------------------------------
  persreturn.beleid.gouverneur.barplot <- Persreturn.beleid.barplot(reactive(df.Persreturn.beleid.detail()$Gouverneur), reactive("Persreturn: Gouverneur"), reactive("Detail"))
  output$persreturn.beleid.gouverneur.barplot <- renderPlot({
    persreturn.beleid.gouverneur.barplot()
  })
  # Table -----------------------------------------------------------
  persreturn.beleid.gouverneur.tabel <- Persreturn.beleid.tabel(reactive(df.Persreturn.beleid.detail()$Gouverneur), reactive("Persreturn: Gouverneur"), reactive("Detail"))
  output$persreturn.beleid.gouverneur.tabel <- renderTable({
    persreturn.beleid.gouverneur.tabel()
  })
  # Leefmilieu -------------------------------------------------------
  # Barplot ---------------------------------------------------------
  persreturn.beleid.leefmilieu.barplot <- Persreturn.beleid.barplot(reactive(df.Persreturn.beleid.detail()$Leefmilieu), reactive("Persreturn: Leefmilieu"), reactive("Detail"))
  output$persreturn.beleid.leefmilieu.barplot <- renderPlot({
    persreturn.beleid.leefmilieu.barplot()
  })
  # Table -----------------------------------------------------------
  persreturn.beleid.leefmilieu.tabel <- Persreturn.beleid.tabel(reactive(df.Persreturn.beleid.detail()$Leefmilieu), reactive("Persreturn: Leefmilieu"), reactive("Detail"))
  output$persreturn.beleid.leefmilieu.tabel <- renderTable({
    persreturn.beleid.leefmilieu.tabel()
  })
  # Mobiliteit -------------------------------------------------------
  # Barplot ---------------------------------------------------------
  persreturn.beleid.mobiliteit.barplot <- Persreturn.beleid.barplot(reactive(df.Persreturn.beleid.detail()$Mobiliteit), reactive("Persreturn: Mobiliteit"), reactive("Detail"))
  output$persreturn.beleid.mobiliteit.barplot <- renderPlot({
    persreturn.beleid.mobiliteit.barplot()
  })
  # Table -----------------------------------------------------------
  persreturn.beleid.mobiliteit.tabel <- Persreturn.beleid.tabel(reactive(df.Persreturn.beleid.detail()$Mobiliteit), reactive("Persreturn: Mobiliteit"), reactive("Detail"))
  output$persreturn.beleid.mobiliteit.tabel <- renderTable({
    persreturn.beleid.mobiliteit.tabel()
  })
  # Onderwijs en Educatie --------------------------------------------
  # Barplot ---------------------------------------------------------
  persreturn.beleid.onderwijs.barplot <- Persreturn.beleid.barplot(reactive(df.Persreturn.beleid.detail()$"Onderwijs en Educatie"), reactive("Persreturn: Onderwijs en Educatie"), reactive("Detail"))
  output$persreturn.beleid.onderwijs.barplot <- renderPlot({
    persreturn.beleid.onderwijs.barplot()
  })
  # Table -----------------------------------------------------------
  persreturn.beleid.onderwijs.tabel <- Persreturn.beleid.tabel(reactive(df.Persreturn.beleid.detail()$"Onderwijs en Educatie"), reactive("Persreturn: Onderwijs en Educatie"), reactive("Detail"))
  output$persreturn.beleid.onderwijs.tabel <- renderTable({
    persreturn.beleid.onderwijs.tabel()
  })
  # Provinciebestuur -------------------------------------------------
  # Barplot ---------------------------------------------------------
  persreturn.beleid.provinciebestuur.barplot <- Persreturn.beleid.barplot(reactive(df.Persreturn.beleid.detail()$Provinciebestuur), reactive("Persreturn: Provinciebestuur"), reactive("Detail"))
  output$persreturn.beleid.provinciebestuur.barplot <- renderPlot({
    persreturn.beleid.provinciebestuur.barplot()
  })
  # Table -----------------------------------------------------------
  persreturn.beleid.provinciebestuur.tabel <- Persreturn.beleid.tabel(reactive(df.Persreturn.beleid.detail()$Provinciebestuur), reactive("Persreturn: Provinciebestuur"), reactive("Detail"))
  output$persreturn.beleid.provinciebestuur.tabel <- renderTable({
    persreturn.beleid.provinciebestuur.tabel()
  })
  # Ruimte -----------------------------------------------------------
  # Barplot ---------------------------------------------------------
  persreturn.beleid.ruimte.barplot <- Persreturn.beleid.barplot(reactive(df.Persreturn.beleid.detail()$Ruimte), reactive("Persreturn: Ruimte"), reactive("Detail"))
  output$persreturn.beleid.ruimte.barplot <- renderPlot({
    persreturn.beleid.ruimte.barplot()
  })
  # Table --------------------------------------------------------------
  persreturn.beleid.ruimte.tabel <- Persreturn.beleid.tabel(reactive(df.Persreturn.beleid.detail()$Ruimte), reactive("Persreturn: Ruimte"), reactive("Detail"))
  output$persreturn.beleid.ruimte.tabel <- renderTable({
    persreturn.beleid.ruimte.tabel()
  })
  # Vrije Tijd -------------------------------------------------------
  # Barplot ------------------------------------------------------------
  persreturn.beleid.vrijetijd.barplot <- Persreturn.beleid.barplot(reactive(df.Persreturn.beleid.detail()$"Vrije Tijd"), reactive("Persreturn: Vrije Tijd"), reactive("Detail"))
  output$persreturn.beleid.vrijetijd.barplot <- renderPlot({
    persreturn.beleid.vrijetijd.barplot()
  })
  # Table --------------------------------------------------------------
  persreturn.beleid.vrijetijd.tabel <- Persreturn.beleid.tabel(reactive(df.Persreturn.beleid.detail()$"Vrije Tijd"), reactive("Persreturn: Vrije Tijd"), reactive("Detail"))
  output$persreturn.beleid.vrijetijd.tabel <- renderTable({
    persreturn.beleid.vrijetijd.tabel()
  })

  # HTML aanmaak -----------------------------------------------------------
  output$report <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "report.html",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(data = Persstatistiek(),
                     jaar = "jaar",
                     kwartaal = input$kwartaal,
                     # 1.1 Persberichten algemeen -------------------------
                     # 1.1.1 Kwartaal
                     persberichten.alg.kwartaal.tabel = df.berichten.Kwartaal(),
                     persberichten.alg.kwartaal.barplot = berichten.barplot.kwartaal(),
                     # 1.1.2 Maand
                     persberichten.alg.maand.tabel = berichten.tabel.maand(),
                     persberichten.alg.maand.barplot = berichten.barplot.maand(),
                     # 1.1.3 Beleid
                     persberichten.alg.beleid.barplot = persberichten.beleid.barplot(),
                     persberichten.alg.beleid.tabel = persberichten.beleid.tabel(),
                     # 1.2 Persberichten per beleid -----------------------
                     # 1.2.1 Economie ------------------------------------
                     # Per maand
                     persberichten.beleid.maand.economie.barplot = Persberichten.beleid.maand.barplot(reactive(df.berichten.Maand.totaal.per.Beleid()), reactive("Economie"))(),
                     persberichten.beleid.maand.economie.tabel = Persberichten.beleid.maand.tabel(reactive(df.berichten.Maand.totaal.per.Beleid()), reactive("Economie"))(),
                     # Per deelbeleid
                     persberichten.beleid.economie.barplot = persberichten.beleid.economie.barplot(),
                     persberichten.beleid.economie.tabel = persberichten.beleid.economie.tabel(),
                     # 1.2.2 Gouverneur ----------------------------------
                     # Per maand
                     persberichten.beleid.maand.gouverneur.barplot = Persberichten.beleid.maand.barplot(reactive(df.berichten.Maand.totaal.per.Beleid()), reactive("Gouverneur"))(),
                     persberichten.beleid.maand.gouverneur.tabel = Persberichten.beleid.maand.tabel(reactive(df.berichten.Maand.totaal.per.Beleid()), reactive("Gouverneur"))(),
                     # Per deelbeleid
                     persberichten.beleid.gouverneur.barplot = persberichten.beleid.gouverneur.barplot(),
                     persberichten.beleid.gouverneur.tabel = persberichten.beleid.gouverneur.tabel(),
                     # 1.2.3 Leefmilieu ----------------------------------
                     # Per maand
                     persberichten.beleid.maand.leefmilieu.barplot = Persberichten.beleid.maand.barplot(reactive(df.berichten.Maand.totaal.per.Beleid()), reactive("Leefmilieu"))(),
                     persberichten.beleid.maand.leefmilieu.tabel = Persberichten.beleid.maand.tabel(reactive(df.berichten.Maand.totaal.per.Beleid()), reactive("Leefmilieu"))(),
                     # Per deelbeleid
                     persberichten.beleid.leefmilieu.barplot = persberichten.beleid.leefmilieu.barplot(),
                     persberichten.beleid.leefmilieu.tabel = persberichten.beleid.leefmilieu.tabel(),
                     # 1.2.4 Mobiliteit ----------------------------------
                     # Per maand
                     persberichten.beleid.maand.mobiliteit.barplot = Persberichten.beleid.maand.barplot(reactive(df.berichten.Maand.totaal.per.Beleid()), reactive("Mobiliteit"))(),
                     persberichten.beleid.maand.mobiliteit.tabel = Persberichten.beleid.maand.tabel(reactive(df.berichten.Maand.totaal.per.Beleid()), reactive("Mobiliteit"))(),
                     # Per deelbeleid
                     persberichten.beleid.mobiliteit.barplot = persberichten.beleid.mobiliteit.barplot(),
                     persberichten.beleid.mobiliteit.tabel = persberichten.beleid.mobiliteit.tabel(),
                     # 1.2.5 Onderwijs en Educatie -----------------------
                     # Per maand
                     persberichten.beleid.maand.onderwijs.barplot = Persberichten.beleid.maand.barplot(reactive(df.berichten.Maand.totaal.per.Beleid()), reactive("Onderwijs en Educatie"))(),
                     persberichten.beleid.maand.onderwijs.tabel = Persberichten.beleid.maand.tabel(reactive(df.berichten.Maand.totaal.per.Beleid()), reactive("Onderwijs en Educatie"))(),
                     # Per deelbeleid
                     persberichten.beleid.onderwijs.barplot = persberichten.beleid.onderwijs.barplot(),
                     persberichten.beleid.onderwijs.tabel = persberichten.beleid.onderwijs.tabel(),
                     # 1.2.6 Provinciebestuur ----------------------------
                     # Per maand
                     persberichten.beleid.maand.provinciebestuur.barplot = Persberichten.beleid.maand.barplot(reactive(df.berichten.Maand.totaal.per.Beleid()), reactive("Provinciebestuur"))(),
                     persberichten.beleid.maand.provinciebestuur.tabel = Persberichten.beleid.maand.tabel(reactive(df.berichten.Maand.totaal.per.Beleid()), reactive("Provinciebestuur"))(),
                     # Per deelbeleid
                     persberichten.beleid.provinciebestuur.barplot = persberichten.beleid.provinciebestuur.barplot(),
                     persberichten.beleid.provinciebestuur.tabel = persberichten.beleid.provinciebestuur.tabel(),
                     # 1.2.7 Ruimte --------------------------------------
                     # Per maand
                     persberichten.beleid.maand.ruimte.barplot = Persberichten.beleid.maand.barplot(reactive(df.berichten.Maand.totaal.per.Beleid()), reactive("Ruimte"))(),
                     persberichten.beleid.maand.ruimte.tabel = Persberichten.beleid.maand.tabel(reactive(df.berichten.Maand.totaal.per.Beleid()), reactive("Ruimte"))(),
                     # Per deelbeleid
                     persberichten.beleid.ruimte.barplot = persberichten.beleid.ruimte.barplot(),
                     persberichten.beleid.ruimte.tabel = persberichten.beleid.ruimte.tabel(),
                     # 1.2.8 Vrije Tijd ----------------------------------
                     # Per maand
                     persberichten.beleid.maand.vrijetijd.barplot = Persberichten.beleid.maand.barplot(reactive(df.berichten.Maand.totaal.per.Beleid()), reactive("Vrije Tijd"))(),
                     persberichten.beleid.maand.vrijetijd.tabel = Persberichten.beleid.maand.tabel(reactive(df.berichten.Maand.totaal.per.Beleid()), reactive("Vrije Tijd"))(),
                     # Per deelbeleid
                     persberichten.beleid.vrijetijd.barplot = persberichten.beleid.vrijetijd.barplot(),
                     persberichten.beleid.vrijetijd.tabel = persberichten.beleid.vrijetijd.tabel(),
                     # 1.3 Persberichten per verzender --------------------
                     # 1.3.1 Algemeen
                     berichten.verzender.beleid.table = berichten.verzender.beleid.table(),
                     berichten.verzender.beleid.barplot = berichten.verzender.beleid.barplot(),
                     # 1.3.2 Per maand
                     # 1.4 Persberichten per type -------------------------
                     berichten.type.table = berichten.type.table(),
                     berichten.type.barplot = berichten.type.barplot(),
                     # 2.1 Persreturn per beleid --------------------------
                     # Algemeen ----------------------------------------
                     persreturn.beleid.barplot = persreturn.beleid.barplot(),
                     persreturn.beleid.tabel = persreturn.beleid.tabel(),
                     # Per deelbeleid ----------------------------------
                     persreturn.beleid.economie.barplot = persreturn.beleid.economie.barplot(),
                     persreturn.beleid.economie.tabel = persreturn.beleid.economie.tabel(),
                     persreturn.beleid.gouverneur.barplot = persreturn.beleid.gouverneur.barplot(),
                     persreturn.beleid.gouverneur.tabel = persreturn.beleid.gouverneur.tabel(),
                     persreturn.beleid.leefmilieu.barplot = persreturn.beleid.leefmilieu.barplot(),
                     persreturn.beleid.leefmilieu.tabel = persreturn.beleid.leefmilieu.tabel(),
                     persreturn.beleid.mobiliteit.barplot = persreturn.beleid.mobiliteit.barplot(),
                     persreturn.beleid.mobiliteit.tabel = persreturn.beleid.mobiliteit.tabel(),
                     persreturn.beleid.onderwijs.barplot = persreturn.beleid.onderwijs.barplot(),
                     persreturn.beleid.onderwijs.tabel = persreturn.beleid.onderwijs.tabel(),
                     persreturn.beleid.provinciebestuur.barplot = persreturn.beleid.provinciebestuur.barplot(),
                     persreturn.beleid.provinciebestuur.tabel = persreturn.beleid.provinciebestuur.tabel(),
                     persreturn.beleid.ruimte.barplot = persreturn.beleid.ruimte.barplot(),
                     persreturn.beleid.ruimte.tabel = persreturn.beleid.ruimte.tabel(),
                     persreturn.beleid.vrijetijd.barplot = persreturn.beleid.vrijetijd.barplot(),
                     persreturn.beleid.vrijetijd.tabel = persreturn.beleid.vrijetijd.tabel(),
                     # 2.2 Persreturn per platform ------------------------
                     return.platform.table = return.platform.table(),
                     return.platform.barplot = return.platform.barplot())
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
}