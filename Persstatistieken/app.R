# Installing and loading packages
pkg <- installed.packages()[, "Package"]
to.load <- c("RColorBrewer", "knitr", "ggplot2", "readxl", "shinydashboard")
for (load in to.load) {
    if(!(load %in% pkg)) {
        install.packages(load, dependencies=TRUE)
    }
    library(load, character.only = TRUE )
}

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
                        menuSubItem("Persreturn/ Beleid", tabName = "Charts1"),
                        menuSubItem("Charts2", tabName = "Charts2"),
                        menuSubItem("Charts3", tabName = "Charts3"),
                        menuSubItem("Charts4", tabName = "Charts4")
                    ),
                    
                    tags$hr(),
                    downloadButton("report", "Generate report")
                )
            ),
            
          # Body -------------------------------------------------------------
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
                  
                  # Charts ----------------------------------------------------
                    tabItem(
                        tabName = "Charts1",
                        fluidRow(
                            tabBox(
                                title = "Persreturn per beleid",
                                width = 12,
                                tabPanel("Barplot", plotOutput("bar.return.beleid")),
                                tabPanel("Tabel", tableOutput("return.beleid"))
                                
                            )
                        )
                    ),
                    tabItem(
                        tabName = "Charts2",
                    ),
                      tabItem(
                        tabName = "Charts3",
                    ),
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
                for (i in c("Kwartaal", "Verzender", "Pu bij Pb", "Persreturn", "Alleen web", "TV", "Beleid", "Detail beleid", "Soort", "Maand")) ({
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

        # Barplot: Persreturn (totaal)/Detail Beleid ---------------------------
            
        # Barplot: Persreturn (totaal)/Beleid ----------------------------------
            
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