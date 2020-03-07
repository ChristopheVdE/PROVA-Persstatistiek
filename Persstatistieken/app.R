# Installing and loading packages
pkg <- installed.packages()[, "Package"]
to.load <- c("RColorBrewer", "knitr", "ggplot2", "readxl", "shinydashboard")
for (load in to.load) {
    if(!(load %in% pkg)) {
        install.packages(load, dependencies=TRUE)
    }
    library(load, character.only = TRUE )
}



source("./Modules/Persberichten_per_beleid.R")
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
                    menuItem("Persberichten", tabname = "Persberichten", icon = icon("bar-chart-o"),
                              menuSubItem("Per Beleid", tabName = "Bericht_Beleid"),
                              menuSubItem("Per Type", tabName = "Bericht_Type"),
                              menuSubItem("Per Verzender", tabName = "Bericht_Verzender"),
                              menuSubItem("Per Tijd", tabName = "Bericht_Tijd")
                    ),
                    menuItem("Persreturn", tabname = "Persreturn", icon = icon("bar-chart-o"),
                             menuSubItem("Per Beleid", tabName = "Return_Beleid"),
                             menuSubItem("Per Platform", tabName = "Return_Platform")
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
            
            # Persberichten ---------------------------------------------
              # Per beleid ----------------------------------------------
                tabItem(
                  tabName = "Bericht_Beleid",
                  fluidRow(
                    Persberichten.beleidOutput("Bericht.Beleid", title = "Persberichten per Beleid", width = "12"),
                    Persberichten.beleidOutput("Bericht.Economie", title = "Persberichten: Economie"),
                    Persberichten.beleidOutput("Bericht.Gouverneur", title = "Persberichten: Gouverneur"),
                    Persberichten.beleidOutput("Bericht.Leefmilieu", title = "Persberichten: Leefmilieu"),
                    Persberichten.beleidOutput("Bericht.Mobiliteit", title = "Persberichten: Mobiliteit"),
                    Persberichten.beleidOutput("Bericht.Onderwijs en Educatie", title = "Persberichten: Onderwijs en Educatie"),
                    Persberichten.beleidOutput("Bericht.Provinciebestuur", title = "Persberichten: Provinciebestuur"),
                    Persberichten.beleidOutput("Bericht.Ruimte", title = "Persberichten: Ruimte"),
                    Persberichten.beleidOutput("Bericht.Vrije Tijd", title = "Persberichten: Vrije Tijd")
                  )
                ),
              # Per Type -------------------------------------------------
                tabItem(
                  tabName = "Bericht_Type",
                  fluidRow(
                    tabBox(
                      title = "Persberichten per Soort",
                      width = 12,
                      tabPanel("Barplot", plotOutput("berichten.type.barplot")),
                      tabPanel("Table", tableOutput("berichten.type.table"))
                    )
                  )
                ),
              # Per Verzender --------------------------------------------
                tabItem(
                  tabName = "Bericht_Verzender",
                  fluidRow(
                    tabBox(
                      title = "Persberichten per Verzender",
                      width = 12,
                      tabPanel("Barplot", plotOutput("berichten.verzender.barplot")),
                      tabPanel("Table", tableOutput("berichten.verzender.table"))
                    ) 
                  )
                ),
              # Per Tijd -------------------------------------------------
                tabItem(
                  tabName = "Bericht_Tijd",
                  fluidRow(
                    
                  )
                ),
            # Persreturn 
              # Per beleid --------------------------------------
                tabItem(
                  tabName = "Return_Beleid",
                  fluidRow(
                    Persreturn.beleidOutput("Return.Beleid", title = "Persreturn per Beleid", width = 12),
                    Persreturn.beleidOutput("Return.Economie", title = "Persreturn: Economie"),
                    Persreturn.beleidOutput("Return.Gouverneur", title = "Persreturn: Gouverneur"),
                    Persreturn.beleidOutput("Return.Leefmilieu", title = "Persreturn: Leefmilieu"),
                    Persreturn.beleidOutput("Return.Mobiliteit", title = "Persreturn: Mobiliteit"),
                    Persreturn.beleidOutput("Return.Onderwijs en Educatie", title = "Persreturn: Onderwijs en Educatie"),
                    Persreturn.beleidOutput("Return.Provinciebestuur", title = "Persreturn: Provinciebestuur"),
                    Persreturn.beleidOutput("Return.Ruimte", title = "Persreturn: Ruimte"),
                    Persreturn.beleidOutput("Return.Vrije Tijd", title = "Persreturn: Vrije Tijd")        
                  )
                )
              )
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
    
        

      # Persberichten ----------------------------------------------------------
        # Per beleid -----------------------------------------------------------
          # Preparation ---------------------------------------------------------
            df.bericht.beleid <- reactive({
              # Create dataframe for barplot ------------------------------------
              bericht.beleid <- data.frame(table(Persstatistiek()$Beleid))
  
              # Rename columns --------------------------------------------------
              colnames(bericht.beleid) <- c("Beleid","Persberichten")
              return(bericht.beleid)
            })
            
          # Barplot & Tables -------------------------------------------------------
            callModule(Persberichten.beleid, "Bericht.Beleid", reactive(df.bericht.beleid()), plottitle = reactive("Persberichten per beleid"), type = reactive("Beleid"))
  
        # Per beleid (detail) --------------------------------------------------
          # Preparation ---------------------------------------------------------
            Persberichten <- reactive({
              Persberichten <- split(Persstatistiek(), Persstatistiek()$Beleid)
  
              for (i in levels(Persstatistiek()$Beleid)) ({
                Persberichten[[i]] <- data.frame(table(Persberichten[[i]]$"Detail beleid"))
                Persberichten[[i]] <- cbind(Beleid = i, Persberichten[[i]])
                colnames(Persberichten[[i]]) <- c("Beleid", "Detail","Persberichten")
              })
              return(Persberichten)
            })
          # Barplots & Tables -------------------------------------------------------------
            callModule(Persberichten.beleid, "Bericht.Economie", reactive(Persberichten()$Economie), plottitle = reactive("Persberichten: Economie"), type = reactive("Detail"))
            callModule(Persberichten.beleid, "Bericht.Gouverneur", reactive(Persberichten()$Gouverneur), plottitle = reactive("Persberichten: Gouverneur"), type = reactive("Detail"))
            callModule(Persberichten.beleid, "Bericht.Leefmilieu", reactive(Persberichten()$Leefmilieu), plottitle = reactive("Persberichten: Leefmilieu"), type = reactive("Detail"))
            callModule(Persberichten.beleid, "Bericht.Mobiliteit", reactive(Persberichten()$Mobiliteit), plottitle = reactive("Persberichten: Milieu"), type = reactive("Detail"))
            callModule(Persberichten.beleid, "Bericht.Onderwijs en Educatie", reactive(Persberichten()$`Onderwijs en Educatie`), plottitle = reactive("Persberichten: Onderwijs en Educatie"), type = reactive("Detail"))
            callModule(Persberichten.beleid, "Bericht.Provinciebestuur", reactive(Persberichten()$Provinciebestuur), plottitle = reactive("Persberichten: Provinciebestuur"), type = reactive("Detail"))
            callModule(Persberichten.beleid, "Bericht.Ruimte", reactive(Persberichten()$Ruimte), plottitle = reactive("Persberichten: Ruimte"), type = reactive("Detail"))
            callModule(Persberichten.beleid, "Bericht.Vrije Tijd", reactive(Persberichten()$`Vrije Tijd`), plottitle = reactive("Persberichten: Vrije Tijd"), type = reactive("Detail"))
  
        
        # Per type -------------------------------------------------------------
          # Preparation --------------------------------------------------------
            df.berichten.type <- reactive({
              berichten <- data.frame(table(Persstatistiek()$Beleid, Persstatistiek()$Soort))
              colnames(berichten) <- c("Beleid", "Type", "Freq")
              return(berichten)
            })
          # Table --------------------------------------------------------------
            output$berichten.type.table <- renderTable({
              df.berichten.type()
            })
          # Barplot ------------------------------------------------------------
            output$berichten.type.barplot <- renderPlot({
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
            
        # Per Verzender --------------------------------------------------------
          # Preparation --------------------------------------------------------
            df.berichten.verzender <- reactive({
              berichten <- data.frame(table(Persstatistiek()$Beleid, Persstatistiek()$Verzender))
              colnames(berichten) <- c("Beleid", "Verzender", "Freq")
              return(berichten)
            })
          # Table --------------------------------------------------------------
            output$berichten.verzender.table <- renderTable({
              df.berichten.verzender()
            })
          # Barplot ------------------------------------------------------------
            output$berichten.verzender.barplot <- renderPlot({
              # Specify color pallete
              colors <- brewer.pal(8,"Pastel2")
              # Create plot
              ggplot(data=df.berichten.verzender(), aes(x=Beleid, y=Freq, fill=Verzender)) +
                geom_bar(position = "dodge", stat='identity') +
                xlab("Beleid") +
                ylab("Aantal") +
                ggtitle("Persberichten per Verzender") +
                geom_text(aes(label=Freq),
                          position=position_dodge(0.9), vjust=0) +
                theme_bw() +
                theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
                scale_fill_manual(values=colors)
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
          # Barplot & Tables -------------------------------------------------------
            callModule(Persreturn.beleid, "Return.Beleid", reactive(df.return.beleid()), plottitle = reactive("Persreturn per beleid"), type = reactive("Beleid"))
  
        
        # Per beleid detail-----------------------------------------------------
          # Preparation --------------------------------------------------------
              Persreturn <- reactive({
                  Persreturn <- split(Persstatistiek(), Persstatistiek()$Beleid)
                  
                  for (i in levels(Persstatistiek()$Beleid)) ({
                    Persreturn[[i]] <- data.frame(table(Persreturn[[i]]$"Detail beleid", Persreturn[[i]]$Persreturn))
                    Persreturn[[i]] <- cbind(Beleid = i, Persreturn[[i]])
                    colnames(Persreturn[[i]]) <- c("Beleid","Detail", "Persreturn", "Freq")
                  })
                  
                  return(Persreturn)
              })
          # Barplots & Tables -------------------------------------------------------------
            callModule(Persreturn.beleid, "Return.Economie", reactive(Persreturn()$Economie), plottitle = reactive("Persreturn: Economie"), type = reactive("Detail"))
            callModule(Persreturn.beleid, "Return.Gouverneur", reactive(Persreturn()$Gouverneur), plottitle = reactive("Persreturn: Gouverneur"), type = reactive("Detail"))
            callModule(Persreturn.beleid, "Return.Leefmilieu", reactive(Persreturn()$Leefmilieu), plottitle = reactive("Persreturn: Leefmilieu"), type = reactive("Detail"))
            callModule(Persreturn.beleid, "Return.Mobiliteit", reactive(Persreturn()$Mobiliteit), plottitle = reactive("Persreturn: Milieu"), type = reactive("Detail"))
            callModule(Persreturn.beleid, "Return.Onderwijs en Educatie", reactive(Persreturn()$`Onderwijs en Educatie`), plottitle = reactive("Persreturn: Onderwijs en Educatie"), type = reactive("Detail"))
            callModule(Persreturn.beleid, "Return.Provinciebestuur", reactive(Persreturn()$Provinciebestuur), plottitle = reactive("Persreturn: Provinciebestuur"), type = reactive("Detail"))
            callModule(Persreturn.beleid, "Return.Ruimte", reactive(Persreturn()$Ruimte), plottitle = reactive("v: Ruimte"), type = reactive("Detail"))
            callModule(Persreturn.beleid, "Return.Vrije Tijd", reactive(Persreturn()$`Vrije Tijd`), plottitle = reactive("Persreturn: Vrije Tijd"), type = reactive("Detail"))
            

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