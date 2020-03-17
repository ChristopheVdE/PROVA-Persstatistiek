###############################################################################
# SHINY APP (Persstatistiek): SERVER
###############################################################################

server <- function(input, output) {
  
  # Functions
  source("./Functions/Persberichten_per_beleid_Table.R")
  source("./Functions/Persberichten_per_beleid_Barplot.R")
  source("./Functions/Persberichten_per_maand_per_beleid_Table.R")
  source("./Functions/Persberichten_per_maand_per_beleid_Barplot.R")
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
      # Per Maand
      # Per Deelbeleid
    # PER VERZENDER -----------------------------------------------------------
      # Algemeen
      # Per Maand
    # PER TYPE ----------------------------------------------------------------
  # ===========================================================================
  
  # PERSRETURN ================================================================
    # PER BELEID --------------------------------------------------------------
    # PER PLATFORM ------------------------------------------------------------
  # ===========================================================================
  
  # HTML RAPPORT AANMAAK ======================================================
  
  # ===========================================================================
  
  
  
  
  # Per beleid ----------------------------------------------------------------
  # Preparation ---------------------------------------------------------------
  df.bericht.beleid <- reactive({
    # Create dataframe for barplot --------------------------------------------
    bericht.beleid <- data.frame(table(Persstatistiek()$Beleid))
    
    # Rename columns ----------------------------------------------------------
    colnames(bericht.beleid) <- c("Beleid","Persberichten")
    return(bericht.beleid)
  })
  
  # Barplot & Tables ----------------------------------------------------------
  # Barplot -------------------------------------------------------------------
  persberichten.beleid.barplot <- Persberichten.beleid.barplot(reactive(df.bericht.beleid()), reactive("Persreturn per beleid"), reactive("Beleid"))
  output$persberichten.beleid.barplot <- renderPlot({
    persberichten.beleid.barplot()
  })
  # Table ---------------------------------------------------------------------
  persberichten.beleid.tabel <- Persberichten.beleid.tabel(reactive(df.bericht.beleid()), reactive("Persreturn per beleid"), reactive("Beleid"))
  output$persberichten.beleid.tabel <- renderTable({
    persberichten.beleid.tabel()
  })
  
  # Per beleid (detail) --------------------------------------------------
  # Preparation ---------------------------------------------------------
  df.persberichten.beleid.detail <- reactive({
    Persberichten <- split(Persstatistiek(), Persstatistiek()$Beleid)
    
    for (i in levels(Persstatistiek()$Beleid)) ({
      Persberichten[[i]] <- data.frame(table(Persberichten[[i]]$"Detail beleid"))
      Persberichten[[i]] <- cbind(Beleid = i, Persberichten[[i]])
      colnames(Persberichten[[i]]) <- c("Beleid", "Detail","Persberichten")
    })
    return(Persberichten)
  })
  # Barplots & Tables --------------------------------------------------
  # Economie ---------------------------------------------------------
  # Barplot ---------------------------------------------------------
  persberichten.beleid.economie.barplot <- Persberichten.beleid.barplot(reactive(df.persberichten.beleid.detail()$Economie), reactive("Persberichten: Economie"), reactive("Detail"))
  output$persberichten.beleid.economie.barplot <- renderPlot({
    persberichten.beleid.economie.barplot()
  })
  # Table -----------------------------------------------------------
  persberichten.beleid.economie.tabel <- Persberichten.beleid.tabel(reactive(df.persberichten.beleid.detail()$Economie), reactive("Persberichten: Economie"), reactive("Detail"))
  output$persberichten.beleid.economie.tabel <- renderTable({
    persberichten.beleid.economie.tabel()
  })
  # Gouverneur -------------------------------------------------------
  # Barplot ---------------------------------------------------------
  persberichten.beleid.gouverneur.barplot <- Persberichten.beleid.barplot(reactive(df.persberichten.beleid.detail()$Gouverneur), reactive("Persberichten: Gouverneur"), reactive("Detail"))
  output$persberichten.beleid.gouverneur.barplot <- renderPlot({
    persberichten.beleid.gouverneur.barplot()
  })
  # Table -----------------------------------------------------------
  persberichten.beleid.gouverneur.tabel <- Persberichten.beleid.tabel(reactive(df.persberichten.beleid.detail()$Gouverneur), reactive("Persberichten: Gouverneur"), reactive("Detail"))
  output$persberichten.beleid.gouverneur.tabel <- renderTable({
    persberichten.beleid.gouverneur.tabel()
  })
  # Leefmilieu -------------------------------------------------------
  # Barplot ---------------------------------------------------------
  persberichten.beleid.leefmilieu.barplot <- Persberichten.beleid.barplot(reactive(df.persberichten.beleid.detail()$Leefmilieu), reactive("Persberichten: Leefmilieu"), reactive("Detail"))
  output$persberichten.beleid.leefmilieu.barplot <- renderPlot({
    persberichten.beleid.leefmilieu.barplot()
  })
  # Table -----------------------------------------------------------
  persberichten.beleid.leefmilieu.tabel <- Persberichten.beleid.tabel(reactive(df.persberichten.beleid.detail()$Leefmilieu), reactive("Persberichten: Leefmilieu"), reactive("Detail"))
  output$persberichten.beleid.leefmilieu.tabel <- renderTable({
    persberichten.beleid.leefmilieu.tabel()
  })
  # Mobiliteit -------------------------------------------------------
  # Barplot ---------------------------------------------------------
  persberichten.beleid.mobiliteit.barplot <- Persberichten.beleid.barplot(reactive(df.persberichten.beleid.detail()$Mobiliteit), reactive("Persberichten: Mobiliteit"), reactive("Detail"))
  output$persberichten.beleid.mobiliteit.barplot <- renderPlot({
    persberichten.beleid.mobiliteit.barplot()
  })
  # Table -----------------------------------------------------------
  persberichten.beleid.mobiliteit.tabel <- Persberichten.beleid.tabel(reactive(df.persberichten.beleid.detail()$Mobiliteit), reactive("Persberichten: Mobiliteit"), reactive("Detail"))
  output$persberichten.beleid.mobiliteit.tabel <- renderTable({
    persberichten.beleid.mobiliteit.tabel()
  })
  # Onderwijs en Educatie --------------------------------------------
  # Barplot ---------------------------------------------------------
  persberichten.beleid.onderwijs.barplot <- Persberichten.beleid.barplot(reactive(df.persberichten.beleid.detail()$"Onderwijs en Educatie"), reactive("Persberichten: Onderwijs en Educatie"), reactive("Detail"))
  output$persberichten.beleid.onderwijs.barplot <- renderPlot({
    persberichten.beleid.onderwijs.barplot()
  })
  # Table -----------------------------------------------------------
  persberichten.beleid.onderwijs.tabel <- Persberichten.beleid.tabel(reactive(df.persberichten.beleid.detail()$"Onderwijs en Educatie"), reactive("Persberichten: Onderwijs en Educatie"), reactive("Detail"))
  output$persberichten.beleid.onderwijs.tabel <- renderTable({
    persberichten.beleid.onderwijs.tabel()
  })
  # Provinciebestuur -------------------------------------------------
  # Barplot ---------------------------------------------------------
  persberichten.beleid.provinciebestuur.barplot <- Persberichten.beleid.barplot(reactive(df.persberichten.beleid.detail()$Provinciebestuur), reactive("Persberichten: Provinciebestuur"), reactive("Detail"))
  output$persberichten.beleid.provinciebestuur.barplot <- renderPlot({
    persberichten.beleid.provinciebestuur.barplot()
  })
  # Table -----------------------------------------------------------
  persberichten.beleid.provinciebestuur.tabel <- Persberichten.beleid.tabel(reactive(df.persberichten.beleid.detail()$Provinciebestuur), reactive("Persberichten: Provinciebestuur"), reactive("Detail"))
  output$persberichten.beleid.provinciebestuur.tabel <- renderTable({
    persberichten.beleid.provinciebestuur.tabel()
  })
  # Ruimte -----------------------------------------------------------
  # Barplot ---------------------------------------------------------
  persberichten.beleid.ruimte.barplot <- Persberichten.beleid.barplot(reactive(df.persberichten.beleid.detail()$Ruimte), reactive("Persberichten: Ruimte"), reactive("Detail"))
  output$persberichten.beleid.ruimte.barplot <- renderPlot({
    persberichten.beleid.ruimte.barplot()
  })
  # Table --------------------------------------------------------------
  persberichten.beleid.ruimte.tabel <- Persberichten.beleid.tabel(reactive(df.persberichten.beleid.detail()$Ruimte), reactive("Persberichten: Ruimte"), reactive("Detail"))
  output$persberichten.beleid.ruimte.tabel <- renderTable({
    persberichten.beleid.ruimte.tabel()
  })
  # Vrije Tijd -------------------------------------------------------
  # Barplot ------------------------------------------------------------
  persberichten.beleid.vrijetijd.barplot <- Persberichten.beleid.barplot(reactive(df.persberichten.beleid.detail()$"Vrije Tijd"), reactive("Persberichten: Vrije tijd"), reactive("Detail"))
  output$persberichten.beleid.vrijetijd.barplot <- renderPlot({
    persberichten.beleid.vrijetijd.barplot()
  })
  # Table --------------------------------------------------------------
  persberichten.beleid.vrijetijd.tabel <- Persberichten.beleid.tabel(reactive(df.persberichten.beleid.detail()$"Vrije Tijd"), reactive("Persberichten: Vrije tijd"), reactive("Detail"))
  output$persberichten.beleid.vrijetijd.tabel <- renderTable({
    persberichten.beleid.vrijetijd.tabel()
  })
  
  # Per type -------------------------------------------------------------
  # Preparation --------------------------------------------------------
  df.berichten.type <- reactive({
    berichten <- data.frame(table(Persstatistiek()$Beleid, Persstatistiek()$Soort))
    colnames(berichten) <- c("Beleid", "Type", "Freq")
    berichten$Type <- as.factor(berichten$Type)
    # Add in possible missing "Type"
    for(i in c("Activiteitenkalender", "Agendatip", "Evenementenkalender", "Persagenda", "Persbericht", "Persuitnodiging")) {
      if(!(i %in% levels(berichten$Type))) {
        temp <- data.frame(
          Beleid = c("Economie", "Gouverneur", "Leefmilieu", "Mobiliteit", "Onderwijs en Educatie", "Provinciebestuur", "Ruimte", "Vrije Tijd"),
          Type = i,
          Freq = 0
        )
        berichten <- rbind(berichten, temp)
      }
    }
    # Return dataset  
    return(berichten)
  })
  # Table --------------------------------------------------------------
  berichten.type.table <- reactive({
    temp <- df.berichten.type()
    temp <- split(df.berichten.type(), df.berichten.type()$Type)
    temp <- data.frame(
      Beleid = c("Economie", "Gouverneur", "Leefmilieu", "Mobiliteit", "Onderwijd en Educatie", "Provinciebestuur", "Ruimte", "Vrije Tijd"),
      Activiteitenkalender = temp$Activiteitenkalender$Freq,
      Agendatip = temp$Agendatip$Freq,
      Evenementenkalender = temp$Evenementenkalender$Freq,
      Persagenda = temp$Persagenda$Freq,
      Persbericht = temp$Persbericht$Freq,
      Persuitnodiging = temp$Persuitnodiging$Freq
    )
    return(temp)
  })
  output$berichten.type.table <- renderTable({
    berichten.type.table()
  })
  # Barplot ------------------------------------------------------------
  berichten.type.barplot <- reactive({
    # Specify color pallete
    colors <- brewer.pal(8,"Pastel2")
    # Create plot
    ggplot(data=df.berichten.type(), aes(x=Beleid, y=Freq, fill=Type)) +
      geom_bar(position = "dodge", stat='identity') +
      xlab("Beleid") +
      ylab("Aantal") +
      ggtitle("Persberichten per Type") +
      geom_text(aes(label=Freq),
                position=position_dodge(0.9), vjust=0) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_fill_manual(values=colors)
  })
  output$berichten.type.barplot <- renderPlot({
    berichten.type.barplot()
  })
  # Per Verzender --------------------------------------------------------
  # Algemeen -----------------------------------------------------------
  # Per beleid -------------------------------------------------------
  # Preparation --------------------------------------------------------
  df.berichten.verzender.beleid <- reactive({
    berichten <- data.frame(table(Persstatistiek()$Beleid, Persstatistiek()$Verzender))
    colnames(berichten) <- c("Beleid", "Verzender", "Freq")
    # Add in possible missing "Verzender"
    for(i in c("Persdienst", "Provincie", "Gouverneur", "Extern")) {
      if(!(i %in% levels(berichten$Verzender))) {
        temp <- data.frame(
          Beleid = c("Economie", "Gouverneur", "Leefmilieu", "Mobiliteit", "Onderwijs en Educatie", "Provinciebestuur", "Ruimte", "Vrije Tijd"),
          Verzender = i,
          Freq = 0
        )
        berichten <- rbind(berichten, temp)
      }
    }
    # Return dataset
    return(berichten)
  })
  # Table --------------------------------------------------------------
  berichten.verzender.beleid.table <- reactive({
    temp <- split(df.berichten.verzender.beleid(), df.berichten.verzender.beleid()$Verzender)
    temp <- data.frame(
      Beleid = c("Economie", "Gouverneur", "Leefmilieu", "Mobiliteit", "Onderwijd en Educatie", "Provinciebestuur", "Ruimte", "Vrije Tijd"),
      Persdienst = temp$Persdienst$Freq,
      Provincie = temp$Provincie$Freq,
      Gouverneur = temp$Gouverneur$Freq,
      Extern = temp$Extern$Freq
    )
  })
  output$berichten.verzender.beleid.table <- renderTable({
    berichten.verzender.beleid.table()
  })
  # Barplot ------------------------------------------------------------
  berichten.verzender.beleid.barplot <- reactive({
    # Specify color pallete
    colors <- brewer.pal(8,"Pastel2")
    # Create plot
    ggplot(data=df.berichten.verzender.beleid(), aes(x=Verzender, y=Freq, fill=Beleid)) +
      geom_bar(position = "dodge", stat='identity') +
      xlab("Verzender") +
      ylab("Aantal") +
      ggtitle("Persberichten per Verzender") +
      geom_text(aes(label=Freq),
                position=position_dodge(0.9), vjust=0) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_fill_manual(values=colors)
  })
  output$berichten.verzender.beleid.barplot <- renderPlot({
    berichten.verzender.beleid.barplot()
  })
  
  # Per maand ----------------------------------------------------------
  # Preparation ------------------------------------------------------
  df.berichten.verzender.maand <- reactive({
    berichten <- data.frame(table(Persstatistiek()$Maand, Persstatistiek()$Verzender))
    colnames(berichten) <- c("Maand", "Verzender", "Freq")
    # Add in possible missing "Verzender"
    for(i in c("Persdienst", "Provincie", "Gouverneur", "Extern")) {
      if(!(i %in% levels(berichten$Verzender))) {
        temp <- data.frame(
          Maand = levels(berichten$Maand),
          Verzender = i,
          Freq = 0
        )
        berichten <- rbind(berichten, temp)
      }
    }
    # Split on month
    berichten <- split(berichten, berichten$Maand)
    
    # Return dataset
    return(berichten)
  })
  # Table ------------------------------------------------------------
  output$berichten.verzender.januari.table <- renderTable({
    df.berichten.verzender.maand()$jan
  })
  # Barplot ----------------------------------------------------------
  output$berichten.verzender.januari.barplot <- renderPlot({
    # Specify color pallete
    colors <- brewer.pal(8,"Pastel2")
    # Create plot
    ggplot(data=df.berichten.verzender.maand()$jan, aes(x=Maand, y=Freq, fill=Verzender)) +
      geom_bar(position = "dodge", stat='identity') +
      xlab("Verzender") +
      ylab("Aantal") +
      ggtitle("Persberichten per Verzender") +
      geom_text(aes(label=Freq),
                position=position_dodge(0.9), vjust=0) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_fill_manual(values=colors)
  })
  # Per Tijd -------------------------------------------------------------
  # Per Maand ----------------------------------------------------------

  
  # Totaal Maand per Beleid ------------------------------------------
  # Preparation ----------------------------------------------------
  df.berichten.Maand.totaal.per.Beleid <-  reactive({
    berichten <- data.frame(table(Persstatistiek()$Beleid, Persstatistiek()$Maand))
    colnames(berichten) <- c("Beleid", "Maand", "Freq")
    berichten$Maand <- factor(berichten$Maand, levels = c("jan", "feb", "mrt", "apr", "mei", "jun", "jul", "aug", "sep", "okt", "nov", "dec"))
    split(berichten, berichten$Beleid)
  })
  # Barplot (Per Maand) ---------------------------------------------
  output$barplot.berichten.Maand.totaal.per.Beleid <- renderPlot({
    # Specify color pallete
    colors <- c(brewer.pal(8,"Pastel2"), brewer.pal(9, "Pastel1"))
    # Create plot
    ggplot(data=df.berichten.Maand.totaal.per.Beleid()[[input$beleid]], aes(x=Beleid, y=Freq, fill=Maand)) +
      geom_bar(position = "dodge", stat='identity') +
      xlab("Beleid") +
      ylab("Aantal") +
      ggtitle("Persberichten: Maand (totaal) per Beleid") +
      geom_text(aes(label=Freq),
                position=position_dodge(0.9), vjust=0) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 0, hjust = 0.5)) +
      scale_fill_manual(values=colors)
  })
  # Beleid per Maand -------------------------------------------------
  # Preparation ------------------------------------------------------
  df.berichten.Beleid.per.Maand <-  reactive({
    berichten <- data.frame(table(Persstatistiek()$Beleid, Persstatistiek()$Maand))
    colnames(berichten) <- c("Beleid", "Maand", "Freq")
    berichten$Maand <- factor(berichten$Maand, levels = c("jan", "feb", "mrt", "apr", "mei", "jun", "jul", "aug", "sep", "okt", "nov", "dec"))
    split(berichten, berichten$Maand)
  })
  # Piechart (Per Maand) ---------------------------------------------
  output$piechart.berichten.Beleid.per.Maand <- renderPlot({
    try(
      {
        par(mar=c(2,0,2,10))
        pie(df.berichten.Beleid.per.Maand()[[input$maand]]$Freq, 
            labels = df.berichten.Beleid.per.Maand()[[input$maand]]$Freq, 
            col = brewer.pal(8,"Pastel2"),
            main = paste("Persberichten:", input$maand))
        par(mar=c(0,0,0,0))
        legend(1, 0.37, c("Economie", "Gouverneur", "Leefmilieu", "Mobiliteit", "Onderwijs en Educatie", "Provinciebestuur", "Ruimte", "Vrije Tijd"), fill = brewer.pal(8,"Pastel2"))
      }, 
      silent = TRUE
    )
  })
  

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
  # Per platform ---------------------------------------------------------
  # Preparation --------------------------------------------------------
  df.return.platform <- reactive({
    
    # Create table persreturn algemeen -------------------------------
    Algemeen <- split(Persstatistiek(), Persstatistiek()$Persreturn)
    Algemeen <- Algemeen$Ja
    Algemeen <- data.frame(table(Algemeen$Beleid, Algemeen$Persreturn))
    colnames(Algemeen) <- c("Beleid", "Algemeen", "Freq")
    Algemeen$Algemeen <- "Algemeen"
    
    # create table: TV
    TV <- split(Persstatistiek(), Persstatistiek()$TV)
    TV <- TV$Ja
    TV <- data.frame(table(TV$Beleid, TV$TV))
    colnames(TV) <- c("Beleid", "TV", "Freq")
    TV$TV <- "TV"
    
    # create Table: Web
    Web <- split(Persstatistiek(), Persstatistiek()$"Alleen web")
    Web <- Web$Ja
    Web$Ja <- "Web"
    Web <- data.frame(table(Web$Beleid, Web$"Alleen web"))
    colnames(Web) <- c("Beleid", "Alleen web", "Freq")
    Web$"Alleen web" <- "Alleen web"
    
    # Merge dataframes
    persreturn <- data.frame(Beleid = TV$Beleid,
                             Platform = c(Algemeen$Algemeen, TV$TV, Web$"Alleen web"),
                             Persreturn = c(Algemeen$Freq, TV$Freq, Web$Freq))
    return(persreturn)
  })
  # Table --------------------------------------------------------------
  return.platform.table <- reactive({
    temp <- split(df.return.platform(), df.return.platform()$Platform)
    temp <- data.frame(Beleid = c("Economie", "Gouverneur", "Leefmilieu", "Mobiliteit", "Onderwijs en Educatie", "Provinciebestuur", "Ruimte", "Vrije Tijd"),
                       Algemeen = temp$Algemeen$Persreturn,
                       Web = temp$"Alleen web"$Persreturn,
                       TV = temp$TV$Persreturn
    )
    colnames(temp) <- c("Beleid", "Persreturn: Algemeen", "Persreturn: Alleen web", "Persreturn: TV")
    return(temp)
  })
  output$return.platform.table <-renderTable({
    return.platform.table()
  })
  # Barplot ------------------------------------------------------------
  return.platform.barplot <- reactive({
    # Specify color pallete
    colors <- brewer.pal(8,"Pastel2")
    # Create plot
    ggplot(data=df.return.platform(), aes(x=Beleid, y=Persreturn, fill=Platform)) +
      geom_bar(position = "dodge", stat='identity') +
      xlab("Beleid") +
      ylab("Aantal") +
      ggtitle("Persreturn per platform") +
      geom_text(aes(label=Persreturn),
                position=position_dodge(0.9), vjust=0) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_fill_manual(values=colors)
  })
  output$return.platform.barplot <- renderPlot({
    return.platform.barplot()
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