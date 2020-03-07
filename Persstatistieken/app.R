# Installing and loading packages
pkg <- installed.packages()[, "Package"]
to.load <- c("RColorBrewer", "knitr", "ggplot2", "readxl", "shinydashboard")
for (load in to.load) {
    if(!(load %in% pkg)) {
        install.packages(load, dependencies=TRUE)
    }
    library(load, character.only = TRUE )
}



source("./Modules/Persreturn_per_beleid.R")



if (interactive()) {
    shinyApp(
    # UI =======================================================================
        ui = dashboardPage(
          # Title ------------------------------------------------------------
            dashboardHeader(
                title = "Persstatistiek"
            ),
          # Sidebar ---------------------------------------------------
            dashboardSidebar(
                sidebarMenu(
                    menuItem("Input", tabName = "Input"),
                        selectInput("kwartaal",
                                "Selecteer kwartaal:",
                                choices = c("Q1", "Q2", "Q3", "Q4", "Jaar"),
                                selected = "Q1"),
                    
                    tags$hr(),
                    menuItem("Charts", tabname = "Charts", icon = icon("bar-chart-o"),
                        menuSubItem("Persberichten/ Beleid", tabName = "Persberichten"),
                        menuSubItem("Persreturn/ Beleid", tabName = "Persreturn"),
                        menuSubItem("Charts3", tabName = "Charts3"),
                        menuSubItem("Charts4", tabName = "Charts4")
                    ),
                    
                    tags$hr(),
                    downloadButton("report", "Generate report")
                )
            ),
            
          # Body ---------------------------------------------------------------
            dashboardBody(
                tabItems(
            # Input scherm -----------------------------------------------
              tabItem(
                  tabName = "Input",
                  fluidRow(
                      box(
                          box(
                              fileInput("file", 
                                        "Kies Excel document:",
                                        multiple = FALSE,
                                        accept = c(".xls", ".xlsx"),
                                        width = 900,
                                        placeholder = "No file selected"),
                              width = 6,
                              height = 100
                          ),
                          box(
                              textInput("sheet", "Te gebruiken Werkblad", value ="Hele organisatie", placeholder = "Hele organisatie" ),
                              width = 3,
                              height = 100
                          ),
                          box(
                              selectInput("kwartaal",
                                          "Selecteer kwartaal:",
                                          choices = c("Q1", "Q2", "Q3", "Q4", "Jaar"),
                                          selected = "Q1"),
                              width = 3,
                              height = 100
                          ),
                          width = 12,
                          height = 130
                      ),
                      box(
                          tableOutput("table"),
                          width = 12
                      )
                  )
              ),
            
            # Persreturn per beleid --------------------------------------
              tabItem(
                  tabName = "Persreturn",
                  fluidRow(
                    
                      Persreturn.beleidOutput("test", title = "Persberichten per Beleid", width = "12"),

                      tabBox(
                          title = "Persreturn: Economie",
                          width = 6,
                          tabPanel("Barplot", plotOutput("bar.return.detail.Economie")),
                          tabPanel("Tabel", tableOutput("table.return.detail.Economie"))
                      ),
                      tabBox(
                          title = "Persreturn: Gouverneur",
                          width = 6,
                          tabPanel("Barplot", plotOutput("bar.return.detail.Gouverneur")),
                          tabPanel("Tabel", tableOutput("table.return.detail.Gouverneur"))
                      ),
                      tabBox(
                          title = "Persreturn: Leefmilieu",
                          width = 6,
                          tabPanel("Barplot", plotOutput("bar.return.detail.Leefmilieu")),
                          tabPanel("Tabel", tableOutput("table.return.detail.Leefmilieu"))
                      ),
                      tabBox(
                          title = "Persreturn: Mobiliteit",
                          width = 6,
                          tabPanel("Barplot", plotOutput("bar.return.detail.Mobiliteit")),
                          tabPanel("Tabel", tableOutput("table.return.detail.Mobiliteit"))
                      ),
                      tabBox(
                          title = "Persreturn: Onderwijs & Educatie",
                          width = 6,
                          tabPanel("Barplot", plotOutput("bar.return.detail.Onderwijs en Educatie")),
                          tabPanel("Tabel", tableOutput("table.return.detail.Onderwijs en Educatie"))
                      ),
                      tabBox(
                          title = "Persreturn: Provinciebestuur",
                          width = 6,
                          tabPanel("Barplot", plotOutput("bar.return.detail.Provinciebestuur")),
                          tabPanel("Tabel", tableOutput("table.return.detail.Provinciebestuur"))
                      ),
                      tabBox(
                          title = "Persreturn: Ruimte",
                          width = 6,
                          tabPanel("Barplot", plotOutput("bar.return.detail.Ruimte")),
                          tabPanel("Tabel", tableOutput("table.return.detail.Ruimte"))
                      ),
                      tabBox(
                          title = "Persreturn: Vrije Tijd",
                          width = 6,
                          tabPanel("Barplot", plotOutput("bar.return.detail.Vrije Tijd")),
                          tabPanel("Tabel", tableOutput("table.return.detail.Vrije Tijd"))
                      )
                  )
              ),
            # Persberichten per beleid -----------------------------------
              tabItem(
                  tabName = "Persberichten",
                  fluidRow(
                      tabBox(
                          title = "Persberichten per beleid",
                          width = 12,
                          tabPanel("Barplot", plotOutput("bar.bericht.beleid")),
                          tabPanel("Tabel", tableOutput("bericht.beleid"))
                      ),
                      tabBox(
                          title = "Persberichten: Economie",
                          width = 6,
                          tabPanel("Barplot", plotOutput("bar.bericht.detail.Economie")),
                          tabPanel("Tabel", tableOutput("table.bericht.detail.Economie"))
                      ),
                      tabBox(
                          title = "Persberichten: Gouverneur",
                          width = 6,
                          tabPanel("Barplot", plotOutput("bar.bericht.detail.Gouverneur")),
                          tabPanel("Tabel", tableOutput("table.bericht.detail.Gouverneur"))
                      ),
                      tabBox(
                          title = "Persberichten: Leefmilieu",
                          width = 6,
                          tabPanel("Barplot", plotOutput("bar.bericht.detail.Leefmilieu")),
                          tabPanel("Tabel", tableOutput("table.bericht.detail.Leefmilieu"))
                      ),
                      tabBox(
                          title = "Persberichten: Mobiliteit",
                          width = 6,
                          tabPanel("Barplot", plotOutput("bar.bericht.detail.Mobiliteit")),
                          tabPanel("Tabel", tableOutput("table.bericht.detail.Mobiliteit"))
                      ),
                      tabBox(
                          title = "Persberichten: Onderwijs & Educatie",
                          width = 6,
                          tabPanel("Barplot", plotOutput("bar.bericht.detail.Onderwijs en Educatie")),
                          tabPanel("Tabel", tableOutput("table.bericht.detail.Onderwijs en Educatie"))
                      ),
                      tabBox(
                          title = "Persberichten: Provinciebestuur",
                          width = 6,
                          tabPanel("Barplot", plotOutput("bar.bericht.detail.Provinciebestuur")),
                          tabPanel("Tabel", tableOutput("table.bericht.detail.Provinciebestuur"))
                      ),
                      tabBox(
                          title = "Persberichten: Ruimte",
                          width = 6,
                          tabPanel("Barplot", plotOutput("bar.bericht.detail.Ruimte")),
                          tabPanel("Tabel", tableOutput("table.bericht.detail.Ruimte"))
                      ),
                      tabBox(
                          title = "Persberichten: Vrije Tijd",
                          width = 6,
                          tabPanel("Barplot", plotOutput("bar.bericht.detail.Vrije Tijd")),
                          tabPanel("Tabel", tableOutput("table.bericht.detail.Vrije Tijd"))
                      )
                  )
              ),
            # nog iets ---------------------------------------------------
                tabItem(
                  tabName = "Charts3",
              ),
            # en nog iets ------------------------------------------------
              tabItem(
                  tabName = "Charts4",
              )
          )
          
              # h3('Summary'),
              # verbatimTextOutput("summary"),
              # h3("Table"),
              # tableOutput("table"),
              # h3('Persrerturn per Beleid'),
              # plotOutput("Return per beleid")
      #         
      #     )
      )
  ),
  
    # SERVER ===================================================================
        server <- function(input, output) {
      
      # Data Preparation -------------------------------------------------------      
        # Prepare dataset ------------------------------------------------------
            Persstatistiek <- reactive({
                
                req(input$file)
                
            # Reading Excel ----------------------------------------------------
                Excel <- read_excel(input$file$datapath, sheet = input$sheet)
                
            # check for and remove possible NA values --------------------------
                Excel <- Excel[complete.cases(Excel),]
                
            # Removing non-required columns ------------------------------------
                Excel$Afzender <- NULL
                Excel$Onderwerp <- NULL
                Excel$Datum <- NULL

            # Fixing Mistakes --------------------------------------------------

              # Verzender ------------------------------------------------------
                Excel$Verzender <- gsub("extern", "Extern", Excel$Verzender, ignore.case = FALSE)
                Excel$Verzender <- gsub("gouverneur", "Gouverneur", Excel$Verzender, ignore.case = FALSE)
                Excel$Verzender <- gsub("persdienst", "Persdienst", Excel$Verzender, ignore.case = FALSE)
                Excel$Verzender <- gsub("provincie", "Provincie", Excel$Verzender, ignore.case = FALSE)

              # Pu bij Pb; Persreturn; Alleen web; TV --------------------------
                for (i in c("Pu bij Pb", "Persreturn", "Alleen web", "TV")) ({
                    Excel[[i]] <- gsub("Ja", "Ja", Excel[[i]], ignore.case = FALSE)
                    Excel[[i]] <- gsub("nee", "Nee", Excel[[i]], ignore.case = FALSE)
                })

              # Beleid ---------------------------------------------------------
                Excel$Beleid <- gsub("economie", "Economie", Excel$Beleid, ignore.case = FALSE)
                Excel$Beleid <- gsub("gouverneur", "Gouverneur", Excel$Beleid, ignore.case = FALSE)
                Excel$Beleid <- gsub("leefmilieu", "Leefmilieu", Excel$Beleid, ignore.case = FALSE)
                Excel$Beleid <- gsub("mobiliteit", "Mobiliteit", Excel$Beleid, ignore.case = FALSE)
                Excel$Beleid <- gsub("Onderwijs en educatie", "Onderwijs en Educatie", Excel$Beleid, ignore.case = FALSE)
                Excel$Beleid <- gsub("provinciebestuur", "Provinciebestuur", Excel$Beleid, ignore.case = FALSE)
                Excel$Beleid <- gsub("ruimte", "Ruimte", Excel$Beleid, ignore.case = FALSE)
                Excel$Beleid <- gsub("Vrije tijd", "Vrije Tijd", Excel$Beleid, ignore.case = FALSE)

              # Detail Beleid --------------------------------------------------
                Excel$"Detail beleid" <- gsub("De Warande", "de Warande", Excel$"Detail beleid", ignore.case = FALSE)
                Excel$"Detail beleid" <- gsub("Economie, innovatie en samenleving", "Economie, Innovatie en Samenleving", Excel$"Detail beleid", ignore.case = FALSE)
                Excel$"Detail beleid" <- gsub("Economie, Innovatie en samenleving", "Economie, Innovatie en Samenleving", Excel$"Detail beleid", ignore.case = FALSE)
                Excel$"Detail beleid" <- gsub("Economie, innovatie en Samenleving", "Economie, Innovatie en Samenleving", Excel$"Detail beleid", ignore.case = FALSE)
                Excel$"Detail beleid" <- gsub("Regionaal Landschappen", "Regionale Landschappen", Excel$"Detail beleid", ignore.case = FALSE)
                Excel$"Detail beleid" <- gsub("Toerisme provincie Antwerpen", "Toerisme Provincie Antwerpen", Excel$"Detail beleid", ignore.case = FALSE)

              # Soort ----------------------------------------------------------
                Excel$Soort <- gsub("persbericht", "Persbericht", Excel$Soort, ignore.case = FALSE)
                Excel$Soort <- gsub("agendatip", "Agendatip", Excel$Soort, ignore.case = FALSE)
                Excel$Soort <- gsub("persagenda", "Persagenda", Excel$Soort, ignore.case = FALSE)
                Excel$Soort <- gsub("activiteitenkalender", "Activiteitenkalender", Excel$Soort, ignore.case = FALSE)
                Excel$Soort <- gsub("evenementenkalender", "Evenementenkalender", Excel$Soort, ignore.case = FALSE)

            # As factor --------------------------------------------------------
                for (i in c("Verzender", "Pu bij Pb", "Persreturn", "Alleen web", "TV", "Beleid", "Soort", "Maand")) ({
                    Excel[[i]] <- as.factor(Excel[[i]])
                })

            # Split ------------------------------------------------------------
                Persstatistiek <- split.data.frame(Excel, Excel$Kwartaal)
                Persstatistiek$Jaar <- Excel
                
            # Removing Excel ---------------------------------------------------
                Excel <- NULL
            
            # Return dataset ---------------------------------------------------
                return(Persstatistiek[[input$kwartaal]])
            }) 

            
        # Summary of data ------------------------------------------------------    
            output$summary <- renderText({
                summary(Persstatistiek())
            })
            
        # Render Original Table ------------------------------------------------
            output$table <- renderTable({
                return(Persstatistiek())
            })
    
        

      # Persbericht / beleid ----------------------------------------------------
        # Prep: Persreturn/ Beleid ----------------------------------------------
          df.bericht.beleid <- reactive({
            # Create dataframe for barplot --------------------------------------
            bericht.beleid <- data.frame(table(Persstatistiek()$Beleid))

            # Rename columns ----------------------------------------------------
            colnames(bericht.beleid) <- c("Beleid","Persberichten")
            return(bericht.beleid)
          })
          
        # Table: Persbericht/Beleid ---------------------------------------------
          # output$bericht.beleid <- renderTable({
          #   df.bericht.beleid()
          # })
          
        # Barplot: Persbericht/Beleid -------------------------------------------
          callModule(Persreturn.beleid, "test", reactive(df.bericht.beleid()), reactive("Persreturn per beleid"), reactive("Beleid"))
          
          # output$bar.bericht.beleid <- renderPlot({
          # 
          #   colors <- brewer.pal(8,"Pastel1")
          # 
          #   ggplot(data=df.bericht.beleid(), aes(x=Beleid, y=`Aantal Persberichten`, fill=Beleid)) +
          #     geom_bar(position = "dodge", stat='identity') +
          #     xlab("Beleid") +
          #     ylab("Aantal") +
          #     ggtitle(c("Persreturn per beleid:")) +
          #     geom_text(aes(label=`Aantal Persberichten`),
          #               position=position_dodge(0.9), vjust=0) +
          #     theme_bw() +
          #     theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
          #     scale_fill_manual(values=colors)
          # })

      # Persbericht/ Detail beleid ----------------------------------------------
        # Prep: Persbericht (totaal)/Detail Beleid ------------------------------
          Persberichten <- reactive({
            # Create dummy column to use for counting total "Persberichten" -----
            temp <- Persstatistiek()
            temp$dummy <- 1
            Persberichten <- split(temp, temp$Beleid)
            # Create dummy column to use for counting total "Persberichten" -----
            for (i in levels(Persstatistiek()$Beleid)) ({
              Persberichten[[i]] <- data.frame(table(Persberichten[[i]]$"Detail beleid", Persberichten[[i]]$dummy))
              colnames(Persberichten[[i]]) <- c("Detail beleid", "Kwartaal", "Aantal Persberichten")
              Persberichten[[i]]$Kwartaal <- NULL
            })
            return(Persberichten)
          })
        # Economie -------------------------------------------------------------
          # Table ------------------------------------------------------------
          output$table.bericht.detail.Economie <- renderTable({
            return(Persberichten()$Economie)
          })
          # Barplot ------------------------------------------------------------
          output$bar.bericht.detail.Economie <- renderPlot({

            colors <- c(brewer.pal(8,"Pastel2"), brewer.pal(9, "Pastel1"))

            ggplot(data=Persberichten()$Economie, aes(x=`Detail beleid`, y=`Aantal Persberichten`, fill=`Detail beleid`)) +
              geom_bar(position = "dodge", stat='identity') +
              xlab("Detail beleid") +
              ylab("Aantal") +
              ggtitle("PErsberichten: Economie") +
              geom_text(aes(label=`Aantal Persberichten`),
                        position=position_dodge(0.9), vjust=0) +
              theme_bw() +
              theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
              scale_fill_manual(values=colors)
          })
        # Gouverneur -----------------------------------------------------------
          # Table ------------------------------------------------------------
          output$table.bericht.detail.Gouverneur <- renderTable({
            return(Persberichten()$Gouverneur)
          })
          # Barplot ------------------------------------------------------------
          output$bar.bericht.detail.Gouverneur <- renderPlot({

            colors <- c(brewer.pal(8,"Pastel2"), brewer.pal(9, "Pastel1"))

            ggplot(data=Persberichten()$Gouverneur, aes(x=`Detail beleid`, y=`Aantal Persberichten`, fill=`Detail beleid`)) +
              geom_bar(position = "dodge", stat='identity') +
              xlab("Detail beleid") +
              ylab("Aantal") +
              ggtitle("Persberichten: Gouverneur") +
              geom_text(aes(label=`Aantal Persberichten`),
                        position=position_dodge(0.9), vjust=0) +
              theme_bw() +
              theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
              scale_fill_manual(values=colors)
          })
        # Leefmilieu -----------------------------------------------------------
          # Table ------------------------------------------------------------
          output$table.bericht.detail.Leefmilieu <- renderTable({
            return(Persberichten()$Leefmilieu)
          })
          # Barplot ------------------------------------------------------------
          output$bar.bericht.detail.Leefmilieu <- renderPlot({
            
            colors <- c(brewer.pal(8,"Pastel2"), brewer.pal(9, "Pastel1"))
            
            ggplot(data=Persberichten()$Leefmilieu, aes(x=`Detail beleid`, y=`Aantal Persberichten`, fill=`Detail beleid`)) +
              geom_bar(position = "dodge", stat='identity') +
              xlab("Detail beleid") +
              ylab("Aantal") +
              ggtitle("Persberichten: Leefmilieu") +
              geom_text(aes(label=`Aantal Persberichten`),
                        position=position_dodge(0.9), vjust=0) +
              theme_bw() +
              theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
              scale_fill_manual(values=colors)
          })
        # Mobiliteit -----------------------------------------------------------
          # Table ------------------------------------------------------------
          output$table.bericht.detail.Mobiliteit <- renderTable({
            return(Persberichten()$Mobiliteit)
          })
          # Barplot ------------------------------------------------------------
          output$bar.bericht.detail.Mobiliteit <- renderPlot({
            
            colors <- c(brewer.pal(8,"Pastel2"), brewer.pal(9, "Pastel1"))
            
            ggplot(data=Persberichten()$Mobiliteit, aes(x=`Detail beleid`, y=`Aantal Persberichten`, fill=`Detail beleid`)) +
              geom_bar(position = "dodge", stat='identity') +
              xlab("Detail beleid") +
              ylab("Aantal") +
              ggtitle("Persberichten: Mobiliteit") +
              geom_text(aes(label=`Aantal Persberichten`),
                        position=position_dodge(0.9), vjust=0) +
              theme_bw() +
              theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
              scale_fill_manual(values=colors)
          })
        # Onderwijs en Educatie ------------------------------------------------
          # Table --------------------------------------------------------------
          output$`table.bericht.detail.Onderwijs en Educatie` <- renderTable({
            return(Persberichten()$"Onderwijs en Educatie")
          })
          # Barplot ------------------------------------------------------------
          output$`bar.bericht.detail.Onderwijs en Educatie` <- renderPlot({
            
            colors <- c(brewer.pal(8,"Pastel2"), brewer.pal(9, "Pastel1"))
            
            ggplot(data=Persberichten()$"Onderwijs en Educatie", aes(x=`Detail beleid`, y=`Aantal Persberichten`, fill=`Detail beleid`)) +
              geom_bar(position = "dodge", stat='identity') +
              xlab("Detail beleid") +
              ylab("Aantal") +
              ggtitle("Persberichten: Onderwijs en Educatie") +
              geom_text(aes(label=`Aantal Persberichten`),
                        position=position_dodge(0.9), vjust=0) +
              theme_bw() +
              theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
              scale_fill_manual(values=colors)
          })
        # Provinciebestuur -----------------------------------------------------
          # Table ------------------------------------------------------------
          output$table.bericht.detail.Provinciebestuur <- renderTable({
            return(Persberichten()$Provinciebestuur)
          })
          # Barplot ------------------------------------------------------------
          output$bar.bericht.detail.Provinciebestuur <- renderPlot({
            
            colors <- c(brewer.pal(8,"Pastel2"), brewer.pal(9, "Pastel1"))
            
            ggplot(data=Persberichten()$Provinciebestuur, aes(x=`Detail beleid`, y=`Aantal Persberichten`, fill=`Detail beleid`)) +
              geom_bar(position = "dodge", stat='identity') +
              xlab("Detail beleid") +
              ylab("Aantal") +
              ggtitle("Persberichten: Provinciebestuur") +
              geom_text(aes(label=`Aantal Persberichten`),
                        position=position_dodge(0.9), vjust=0) +
              theme_bw() +
              theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
              scale_fill_manual(values=colors)
          })
        # Ruimte ---------------------------------------------------------------
          # Table ------------------------------------------------------------
          output$table.bericht.detail.Ruimte <- renderTable({
            return(Persberichten()$Ruimte)
          })
          # Barplot ------------------------------------------------------------
          output$bar.bericht.detail.Ruimte <- renderPlot({
            
            colors <- c(brewer.pal(8,"Pastel2"), brewer.pal(9, "Pastel1"))
            
            ggplot(data=Persberichten()$Ruimte, aes(x=`Detail beleid`, y=`Aantal Persberichten`, fill=`Detail beleid`)) +
              geom_bar(position = "dodge", stat='identity') +
              xlab("Detail beleid") +
              ylab("Aantal") +
              ggtitle("Persberichten: Ruimte") +
              geom_text(aes(label=`Aantal Persberichten`),
                        position=position_dodge(0.9), vjust=0) +
              theme_bw() +
              theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
              scale_fill_manual(values=colors)
          })
        # Vrije Tijd -----------------------------------------------------------
          # Table ------------------------------------------------------------
          output$`table.bericht.detail.Vrije Tijd` <- renderTable({
            return(Persberichten()$"Vrije Tijd")
          })
          # Barplot ------------------------------------------------------------
          output$`bar.bericht.detail.Vrije Tijd` <- renderPlot({
            
            colors <- c(brewer.pal(8,"Pastel2"), brewer.pal(9, "Pastel1"))
            
            ggplot(data=Persberichten()$"Vrije Tijd", aes(x=`Detail beleid`, y=`Aantal Persberichten`, fill=`Detail beleid`)) +
              geom_bar(position = "dodge", stat='identity') +
              xlab("Detail beleid") +
              ylab("Aantal") +
              ggtitle("Persberichten: Vrije Tijd") +
              geom_text(aes(label=`Aantal Persberichten`),
                        position=position_dodge(0.9), vjust=0) +
              theme_bw() +
              theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
              scale_fill_manual(values=colors)
          })
      
      
      
      # Persreturn / beleid ----------------------------------------------------
        # Prep: Persreturn/ Beleid --------------------------------------------
            df.return.beleid <- reactive({
              # Create dataframe for barplot -----------------------------------
                return.beleid <- data.frame(table(Persstatistiek()$Beleid,
                                                  Persstatistiek()$Persreturn))
              # Rename columns -------------------------------------------------
                colnames(return.beleid) <- c("Beleid", "Persreturn", "Freq")
                
                return(return.beleid)
            })
    
        # Table: Persreturn/Beleid ---------------------------------------------
            output$return.beleid <- renderTable({
                df.return.beleid()
            })
            
        # Barplot: Persreturn/Beleid -------------------------------------------
            output$bar.return.beleid <- renderPlot({

                colors <- brewer.pal(6,"Pastel1")

                ggplot(data=df.return.beleid(), aes(x=Beleid, y=Freq, fill=Persreturn)) + 
                    geom_bar(position = "dodge", stat='identity') +
                    xlab("Beleid") +
                    ylab("Aantal") +
                    ggtitle(c("Persreturn per beleid:")) +
                    geom_text(aes(label=Freq), 
                              position=position_dodge(0.9), vjust=0) +
                    theme_bw() +
                    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
                    scale_fill_manual(values=colors[2:1])  
            })

            
      
      # Persreturn/ Detail beleid ----------------------------------------------
        # Prep: Persreturn (totaal)/Detail Beleid ------------------------------
            Persreturn <- reactive({
                Persreturn <- split(Persstatistiek(), Persstatistiek()$Beleid)
                
                for (i in levels(Persstatistiek()$Beleid)) ({
                  Persreturn[[i]] <- data.frame(table(Persreturn[[i]]$"Detail beleid", Persreturn[[i]]$Persreturn))
                  colnames(Persreturn[[i]]) <- c("Detail beleid", "Persreturn", "Freq")
                })
                
                return(Persreturn)
            })
        # Economie -------------------------------------------------------------
          # Table ------------------------------------------------------------
            output$table.return.detail.Economie <- renderTable({
              return(Persreturn()$Economie)
            })
          # Barplot ------------------------------------------------------------
            output$bar.return.detail.Economie <- renderPlot({
              
              colors <- brewer.pal(6,"Pastel1")
              
              ggplot(data=Persreturn()$Economie, aes(x=`Detail beleid`, y=Freq, fill=Persreturn)) + 
                geom_bar(position = "dodge", stat='identity') +
                xlab("Detail beleid") +
                ylab("Aantal") +
                ggtitle("Persreturn: Economie") +
                geom_text(aes(label=Freq), 
                          position=position_dodge(0.9), vjust=0) +
                theme_bw() +
                theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
                scale_fill_manual(values=colors[2:1])  
            })
        # Gouverneur -----------------------------------------------------------
          # Table ------------------------------------------------------------
          output$table.return.detail.Gouverneur <- renderTable({
            return(Persreturn()$Gouverneur)
          })
          # Barplot ------------------------------------------------------------
          output$bar.return.detail.Gouverneur <- renderPlot({
            
            colors <- brewer.pal(6,"Pastel1")
            
            ggplot(data=Persreturn()$Gouverneur, aes(x=`Detail beleid`, y=Freq, fill=Persreturn)) + 
              geom_bar(position = "dodge", stat='identity') +
              xlab("Detail beleid") +
              ylab("Aantal") +
              ggtitle("Persreturn: Gouverneur") +
              geom_text(aes(label=Freq), 
                        position=position_dodge(0.9), vjust=0) +
              theme_bw() +
              theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
              scale_fill_manual(values=colors[2:1])  
          })
        # Leefmilieu -----------------------------------------------------------
          # Table ------------------------------------------------------------
          output$table.return.detail.Leefmilieu <- renderTable({
            return(Persreturn()$Leefmilieu)
          })
          # Barplot ------------------------------------------------------------
          output$bar.return.detail.Leefmilieu <- renderPlot({
            
            colors <- brewer.pal(6,"Pastel1")
            
            ggplot(data=Persreturn()$Leefmilieu, aes(x=`Detail beleid`, y=Freq, fill=Persreturn)) + 
              geom_bar(position = "dodge", stat='identity') +
              xlab("Detail beleid") +
              ylab("Aantal") +
              ggtitle("Persreturn: Leefmilieu") +
              geom_text(aes(label=Freq), 
                        position=position_dodge(0.9), vjust=0) +
              theme_bw() +
              theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
              scale_fill_manual(values=colors[2:1])  
          })
        # Mobiliteit -----------------------------------------------------------
          # Table ------------------------------------------------------------
          output$table.return.detail.Mobiliteit <- renderTable({
            return(Persreturn()$Mobiliteit)
          })
          # Barplot ------------------------------------------------------------
          output$bar.return.detail.Mobiliteit <- renderPlot({
            
            colors <- brewer.pal(6,"Pastel1")
            
            ggplot(data=Persreturn()$Mobiliteit, aes(x=`Detail beleid`, y=Freq, fill=Persreturn)) + 
              geom_bar(position = "dodge", stat='identity') +
              xlab("Detail beleid") +
              ylab("Aantal") +
              ggtitle("Persreturn: Mobiliteit") +
              geom_text(aes(label=Freq), 
                        position=position_dodge(0.9), vjust=0) +
              theme_bw() +
              theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
              scale_fill_manual(values=colors[2:1])  
          })
        # Onderwijs & Educatie -------------------------------------------------
          # Table --------------------------------------------------------------
          output$`table.return.detail.Onderwijs en Educatie` <- renderTable({
            return(Persreturn()$`Onderwijs en Educatie`)
          })
          # Barplot ------------------------------------------------------------
          output$`bar.return.detail.Onderwijs en Educatie` <- renderPlot({
            
            colors <- brewer.pal(6,"Pastel1")
            
            ggplot(data=Persreturn()$`Onderwijs en Educatie`, aes(x=`Detail beleid`, y=Freq, fill=Persreturn)) + 
              geom_bar(position = "dodge", stat='identity') +
              xlab("Detail beleid") +
              ylab("Aantal") +
              ggtitle("Persreturn: Onderwijs & Educatie") +
              geom_text(aes(label=Freq), 
                        position=position_dodge(0.9), vjust=0) +
              theme_bw() +
              theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
              scale_fill_manual(values=colors[2:1])  
          })
        # Provinciebestuur -------------------------------------------------------------
          # Table ------------------------------------------------------------
          output$table.return.detail.Provinciebestuur <- renderTable({
            return(Persreturn()$Provinciebestuur)
          })
          # Barplot ------------------------------------------------------------
          output$bar.return.detail.Provinciebestuur <- renderPlot({
            
            colors <- brewer.pal(6,"Pastel1")
            
            ggplot(data=Persreturn()$Provinciebestuur, aes(x=`Detail beleid`, y=Freq, fill=Persreturn)) + 
              geom_bar(position = "dodge", stat='identity') +
              xlab("Detail beleid") +
              ylab("Aantal") +
              ggtitle("Persreturn: Provinciebestuur") +
              geom_text(aes(label=Freq), 
                        position=position_dodge(0.9), vjust=0) +
              theme_bw() +
              theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
              scale_fill_manual(values=colors[2:1])  
          })
        # Ruimte -------------------------------------------------------------
          # Table ------------------------------------------------------------
          output$table.return.detail.Ruimte <- renderTable({
            return(Persreturn()$Ruimte)
          })
          # Barplot ------------------------------------------------------------
          output$bar.return.detail.Ruimte <- renderPlot({
            
            colors <- brewer.pal(6,"Pastel1")
            
            ggplot(data=Persreturn()$Ruimte, aes(x=`Detail beleid`, y=Freq, fill=Persreturn)) + 
              geom_bar(position = "dodge", stat='identity') +
              xlab("Detail beleid") +
              ylab("Aantal") +
              ggtitle("Persreturn: Ruimte") +
              geom_text(aes(label=Freq), 
                        position=position_dodge(0.9), vjust=0) +
              theme_bw() +
              theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
              scale_fill_manual(values=colors[2:1])  
          })
        # Vrije Tijd -------------------------------------------------------------
          # Table ------------------------------------------------------------
          output$`table.return.detail.Vrije Tijd` <- renderTable({
            return(Persreturn()$`Vrije Tijd`)
          })
          # Barplot ------------------------------------------------------------
          output$`bar.return.detail.Vrije Tijd` <- renderPlot({
            
            colors <- brewer.pal(6,"Pastel1")
            
            ggplot(data=Persreturn()$`Vrije Tijd`, aes(x=`Detail beleid`, y=Freq, fill=Persreturn)) + 
              geom_bar(position = "dodge", stat='identity') +
              xlab("Detail beleid") +
              ylab("Aantal") +
              ggtitle("Persreturn: Vrije Tijd") +
              geom_text(aes(label=Freq), 
                        position=position_dodge(0.9), vjust=0) +
              theme_bw() +
              theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
              scale_fill_manual(values=colors[2:1])  
          })
            
            
            
            
            
            
        
      # Barplot: Persberichten/ kwartaal (jaarbasis) -------------------------
            
        # Barplot: type persbericht (Soort) / beleid ---------------------------
            
        # Barplot: persberichten/ maand ----------------------------------------
            
        # Barplot: persreturntype (Tv, Web) / beleid ---------------------------
            
        # Barplot: persberichten (totaal)/ beleid/ verzender -------------------
            
        # persberichten/ kwartaal/ beleid / verzender --------------------------
        
        # Pdf aanmaak ----------------------------------------------------------
            output$report <- downloadHandler(
                # For PDF output, change this to "report.pdf"
                filename = "report.pdf",
                content = function(file) {
                    # Copy the report file to a temporary directory before processing it, in
                    # case we don't have write permissions to the current working dir (which
                    # can happen when deployed).
                    tempReport <- file.path(tempdir(), "report.Rmd")
                    file.copy("report.Rmd", tempReport, overwrite = TRUE)
                    
                    # Set up parameters to pass to Rmd document
                    params <- list(n = Persstatistiek())
                    
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
    )
}