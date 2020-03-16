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
  # Modules
    source("./Modules/Persberichten_per_beleid.R")
    # source("./Modules/Persreturn_per_beleid.R")
  
  # Functions
    source("./Functions/Persberichten_per_beleid_Table.R")
    source("./Functions/Persberichten_per_beleid_Barplot.R")
    source("./Functions/Persberichten_per_maand_per_beleid.R")
    source("./Functions/Persreturn_per_beleid_Table.R")
    source("./Functions/Persreturn_per_beleid_Barplot.R")
  
  # App
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
                    # selectInput("kwartaal",
                    #         "Selecteer kwartaal:",
                    #         choices = c("Q1", "Q2", "Q3", "Q4", "Jaar"),
                    #         selected = "Q1"),
                    # 
                    tags$hr(),
                    menuItem("Persberichten", tabname = "Persberichten", icon = icon("bar-chart-o"),
                              menuSubItem("Per Beleid", tabName = "Bericht_Beleid"),
                              menuSubItem("Per Type", tabName = "Bericht_Type"),
                              menuSubItem("Per Verzender", tabName = "Bericht_Verzender"),
                              menuItem("Per Tijd", tabname = "Bericht_Tijd", icon = icon("angle-double-right", lib = "font-awesome"),
                                menuSubItem("Per Maand", tabName = "Bericht_Maand"),
                                menuSubItem("Per Jaar", tabName = "Bericht_Jaar")
                              ) 
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
                          title = "File input",
                          width = 5,
                          fileInput("file", 
                                    "Kies Excel document:",
                                    multiple = FALSE,
                                    accept = c(".xls", ".xlsx"),
                                    width = 900,
                                    placeholder = "No file selected"),

                          textInput("sheet", "Te gebruiken Werkblad", value ="Hele organisatie", placeholder = "Hele organisatie" ),
                          checkboxInput("headers", label = "Eerste rij bevat kolomnamen", value = TRUE),
                          selectInput("kwartaal",
                                      "Selecteer kwartaal:",
                                      choices = c("Q1", "Q2", "Q3", "Q4", "Jaar"),
                                      selected = "Q1")
                        ),
                        box(
                          title = "Selecteer kolomen die overenkomen met kolominhoud (indien geen kolomnnamen in Excel)",
                          width = 7,
                          column(
                            width = 3,
                            tags$br(tags$b("Kwartaal")),
                            tags$br(tags$b("Verzender")),
                            tags$br(tags$b("Beleid")),
                            tags$br(tags$b("Detail beleid")),
                            tags$br(tags$b("Type Persbericht"))
                          ),
                          column(
                            width = 3,
                            textInput("col.kwartaal", label = NULL, value = 1, placeholder = 1),
                            textInput("col.verzender", label = NULL, value = 2, placeholder = 2),
                            textInput("col.beleid", label = NULL, value = 9, placeholder = 3),
                            textInput("col.detail", label = NULL, value = 10, placeholder = 4),
                            textInput("col.type", label = NULL, value = 11, placeholder = 5)
                          ),
                          column(
                            width = 3,
                            tags$br(tags$b("Persreturn: Algemeen")),
                            tags$br(tags$b("Persreturn: Web")),
                            tags$br(tags$b("Persreturn: TV")),
                            tags$br(tags$b("Maand")),
                          ),
                          column(
                            width = 3,
                            textInput("col.return.algemeen", label = NULL, value = 6, placeholder = 6),
                            textInput("col.return.web", label = NULL, value = 7, placeholder = 7),
                            textInput("col.return.tv", label = NULL, value = 8, placeholder = 8),
                            textInput("col.maand", label = NULL, value = 13, placeholder = 9)
                          )
                        ),
                        box(
                            tableOutput("table"),
                            width = 12
                        )
                    )
                ),
            
            # Persberichten ----------------------------------------------------
              # Per beleid ----------------------------------------------
                tabItem(
                  tabName = "Bericht_Beleid",
                  fluidRow(
                    tabBox(
                      width = 12,
                      title = "Persberichten per beleid",
                      tabPanel("Barplot", plotOutput("persberichten.beleid.barplot")),
                      tabPanel("Tabel", tableOutput("persberichten.beleid.tabel"))
                    ),
                    tabBox(
                      width = 6,
                      title = "Persberichten: Economie",
                      tabPanel("Barplot", plotOutput("persberichten.beleid.economie.barplot")),
                      tabPanel("Tabel", tableOutput("persberichten.beleid.economie.tabel"))
                    ),
                    tabBox(
                      width = 6,
                      title = "Persberichten: Gouverneur",
                      tabPanel("Barplot", plotOutput("persberichten.beleid.gouverneur.barplot")),
                      tabPanel("Tabel", tableOutput("persberichten.beleid.gouverneur.tabel"))
                    ),
                    tabBox(
                      width = 6,
                      title = "Persberichten: Leefmilieu",
                      tabPanel("Barplot", plotOutput("persberichten.beleid.leefmilieu.barplot")),
                      tabPanel("Tabel", tableOutput("persberichten.beleid.leefmilieu.tabel"))
                    ),
                    tabBox(
                      width = 6,
                      title = "Persberichten: Mobiliteit",
                      tabPanel("Barplot", plotOutput("persberichten.beleid.mobiliteit.barplot")),
                      tabPanel("Tabel", tableOutput("persberichten.beleid.mobiliteit.tabel"))
                    ),
                    tabBox(
                      width = 6,
                      title = "Persberichten: Onderwijs en Educatie",
                      tabPanel("Barplot", plotOutput("persberichten.beleid.onderwijs.barplot")),
                      tabPanel("Tabel", tableOutput("persberichten.beleid.onderwijs.tabel"))
                    ),
                    tabBox(
                      width = 6,
                      title = "Persberichten: Provinciebestuur",
                      tabPanel("Barplot", plotOutput("persberichten.beleid.provinciebestuur.barplot")),
                      tabPanel("Tabel", tableOutput("persberichten.beleid.provinciebestuur.tabel"))
                    ),
                    tabBox(
                      width = 6,
                      title = "Persberichten: Ruimte",
                      tabPanel("Barplot", plotOutput("persberichten.beleid.ruimte.barplot")),
                      tabPanel("Tabel", tableOutput("persberichten.beleid.ruimte.tabel"))
                    ),
                    tabBox(
                      width = 6,
                      title = "Persberichten: Vrije Tijd",
                      tabPanel("Barplot", plotOutput("persberichten.beleid.vrijetijd.barplot")),
                      tabPanel("Tabel", tableOutput("persberichten.beleid.vrijetijd.tabel"))
                    )
                    # Persberichten.beleidOutput("Bericht.Beleid", title = "Persberichten per Beleid", width = "12"),
                    # Persberichten.beleidOutput("Bericht.Economie", title = "Persberichten: Economie"),
                    # Persberichten.beleidOutput("Bericht.Gouverneur", title = "Persberichten: Gouverneur"),
                    # Persberichten.beleidOutput("Bericht.Leefmilieu", title = "Persberichten: Leefmilieu"),
                    # Persberichten.beleidOutput("Bericht.Mobiliteit", title = "Persberichten: Mobiliteit"),
                    # Persberichten.beleidOutput("Bericht.Onderwijs en Educatie", title = "Persberichten: Onderwijs en Educatie"),
                    # Persberichten.beleidOutput("Bericht.Provinciebestuur", title = "Persberichten: Provinciebestuur"),
                    # Persberichten.beleidOutput("Bericht.Ruimte", title = "Persberichten: Ruimte"),
                    # Persberichten.beleidOutput("Bericht.Vrije Tijd", title = "Persberichten: Vrije Tijd")
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
                      tabPanel("Tabel", tableOutput("berichten.type.table"))
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
              # Per Tijd -------------------------------------------------------
                # Per Maand ----------------------------------------------------
                  tabItem(
                    tabName = "Bericht_Maand",
                    fluidRow(
                      tabBox(
                        title = "Persberichten: Totaal per Maand",
                        width = 12,
                        tabPanel("Barplot", plotOutput("berichten.barplot.maand")),
                        tabPanel("Tabel", tableOutput("berichten.tabel.maand"))
                      ),
                      box(
                        title = "Persberichten: Maand (totaal) per Beleid",
                        width = 6,
                        selectInput("beleid", "Selecteer Beleid", c("Economie", "Gouverneur", "Leefmilieu", "Mobiliteit", "Onderwijs en Educatie", "Provinciebestuur", "Ruimte", "Vrije tijd"), selected = "Economie"),
                        plotOutput("barplot.berichten.Maand.totaal.per.Beleid")
                      ),
                      box(
                        title = "Persberichten: Beleid per Maand",
                        width = 6,
                        selectInput("maand", "Selecteer Maand", c("jan", "feb", "mrt", "apr", "mei", "jun", "jul", "aug", "sep", "okt", "nov", "dec"), selected = "jan"),
                        plotOutput("piechart.berichten.Beleid.per.Maand")
                      )
                    )
                  ),
                # Per Jaar -----------------------------------------------------
                  tabItem(
                    tabName = "Bericht_Jaar",
                    fluidRow(
                      tabBox(
                        title = "Persberichten per Kwartaal",
                        width = 6,
                        tabPanel("Barplot", plotOutput("berichten.barplot.kwartaal")),
                        tabPanel("Tabel", tableOutput("berichten.tabel.kwartaal"))
                      ),
                      tabBox(
                        title = "Persberichten per Jaar",
                        width = 6,
                        tabPanel("Scatterplot", plotOutput("berichten.scatterplot.jaar")),
                        tabPanel("Tabel", tableOutput("berichten.tabel.jaar"))
                      )  
                    )
                  ),
            # Persreturn -------------------------------------------------------
              # Per beleid -----------------------------------------------------
                tabItem(
                  tabName = "Return_Beleid",
                  fluidRow(
                    tabBox(
                      width = 12,
                      title = "Persreturn per beleid",
                      tabPanel("Barplot", plotOutput("persreturn.beleid.barplot")),
                      tabPanel("Tabel", tableOutput("persreturn.beleid.tabel"))
                    ),
                    tabBox(
                      width = 6,
                      title = "Persreturn: Economie",
                      tabPanel("Barplot", plotOutput("persreturn.beleid.economie.barplot")),
                      tabPanel("Tabel", tableOutput("persreturn.beleid.economie.tabel"))
                    ),
                    tabBox(
                      width = 6,
                      title = "Persreturn: Gouverneur",
                      tabPanel("Barplot", plotOutput("persreturn.beleid.gouverneur.barplot")),
                      tabPanel("Tabel", tableOutput("persreturn.beleid.gouverneur.tabel"))
                    ),
                    tabBox(
                      width = 6,
                      title = "Persreturn: Leefmilieu",
                      tabPanel("Barplot", plotOutput("persreturn.beleid.leefmilieu.barplot")),
                      tabPanel("Tabel", tableOutput("persreturn.beleid.leefmilieu.tabel"))
                    ),
                    tabBox(
                      width = 6,
                      title = "Persreturn: Mobiliteit",
                      tabPanel("Barplot", plotOutput("persreturn.beleid.mobiliteit.barplot")),
                      tabPanel("Tabel", tableOutput("persreturn.beleid.mobiliteit.tabel"))
                    ),
                    tabBox(
                      width = 6,
                      title = "Persreturn: Onderwijs en Educatie",
                      tabPanel("Barplot", plotOutput("persreturn.beleid.onderwijs.barplot")),
                      tabPanel("Tabel", tableOutput("persreturn.beleid.onderwijs.tabel"))
                    ),
                    tabBox(
                      width = 6,
                      title = "Persreturn: Provinciebestuur",
                      tabPanel("Barplot", plotOutput("persreturn.beleid.provinciebestuur.barplot")),
                      tabPanel("Tabel", tableOutput("persreturn.beleid.provinciebestuur.tabel"))
                    ),
                    tabBox(
                      width = 6,
                      title = "Persreturn: Ruimte",
                      tabPanel("Barplot", plotOutput("persreturn.beleid.ruimte.barplot")),
                      tabPanel("Tabel", tableOutput("persreturn.beleid.ruimte.tabel"))
                    ),
                    tabBox(
                      width = 6,
                      title = "Persreturn: Vrije Tijd",
                      tabPanel("Barplot", plotOutput("persreturn.beleid.vrijetijd.barplot")),
                      tabPanel("Tabel", tableOutput("persreturn.beleid.vrijetijd.tabel"))
                    )
                  )
                ),
              # Per Platform -----------------------------------------------------
                tabItem(
                  tabName = "Return_Platform",
                  fluidRow(
                    tabBox(
                      title = "Persreturn per platform",
                      width = 12,
                      tabPanel("Barplot", plotOutput("return.platform.barplot")),
                      tabPanel("Tabel", tableOutput("return.platform.table"))
                    )
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
                Excel <- read_excel(input$file$datapath, sheet = input$sheet, col_names = input$headers)

            # Fixing colnames --------------------------------------------------
                  if(input$headers == FALSE) {
                    col.selected <- c(input$col.beleid, input$col.detail, input$col.kwartaal, input$col.verzender, input$col.type, input$col.return.algemeen, input$col.return.web, input$col.return.tv, input$col.maand)
                  
                  # Catch errors
                    for(i in col.selected) {
                      if( i == "") {
                        stop("One or more columnsnames doesn't have a column assign to it")
                      } else if (TRUE %in% duplicated(col.selected)) {
                        stop("One or more columns are assigned to the same columnname")
                      }
                    }
                  # prep  
                    for (i in 1:length(col.selected)) {
                      col.selected[i] <- paste0("...", col.selected[i])
                    }
                    
                  # Process columns & names
                    Excel <- data.frame(
                      Kwartaal = Excel[[col.selected[3]]],
                      Verzender = Excel[[col.selected[4]]],
                      Persreturn = Excel[[col.selected[6]]],
                      Web = Excel[[col.selected[7]]],
                      TV = Excel[[col.selected[8]]],
                      Beleid = Excel[[col.selected[1]]],
                      Detail = Excel[[col.selected[2]]],
                      Soort = Excel[[col.selected[5]]],
                      Maand = Excel[[col.selected[9]]]
                    )
                    colnames(Excel)[4] <- "Alleen web"
                    colnames(Excel)[7] <- "Detail beleid"
                  
                  # Errorcatching kwartaal niet gevonden  
                    if(!(input$kwartaal %in% levels(Excel$Kwartaal))) {
                      stop("Verkeerde kolomnr gekoppeld aan kolom 'Kwartaal': kan geselecteerd kwartaal niet terugvinden in kolom. ")
                    }
                }
                
            # check for and remove possible NA values --------------------------
                Excel <- Excel[complete.cases(Excel),]
                
            # Removing non-required columns ------------------------------------
                for(i in colnames(Excel)) {
                  if(!(i %in% c("Beleid", "Detail beleid", "Kwartaal", "Verzender", "Persreturn", "Alleen web", "TV", "Soort", "Maand"))) {
                    Excel[[i]] <- NULL
                  }
                }
                
            # Fixing Mistakes --------------------------------------------------

              # Verzender ------------------------------------------------------
                Excel$Verzender <- gsub("extern", "Extern", Excel$Verzender, ignore.case = FALSE)
                Excel$Verzender <- gsub("gouverneur", "Gouverneur", Excel$Verzender, ignore.case = FALSE)
                Excel$Verzender <- gsub("persdienst", "Persdienst", Excel$Verzender, ignore.case = FALSE)
                Excel$Verzender <- gsub("provincie", "Provincie", Excel$Verzender, ignore.case = FALSE)

              # Pu bij Pb; Persreturn; Alleen web; TV --------------------------
                for (i in c("Persreturn", "Alleen web", "TV")) ({
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
                for (i in c("Verzender", "Beleid")) ({
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
                # return(Persstatistiek())
              Persstatistiek()
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
            
          # Barplot & Tables --------------------------------------------------
            # Barplot ---------------------------------------------------------
            persberichten.beleid.barplot <- Persberichten.beleid.barplot(reactive(df.bericht.beleid()), reactive("Persreturn per beleid"), reactive("Beleid"))
            output$persberichten.beleid.barplot <- renderPlot({
              persberichten.beleid.barplot()
            })
            # Table -----------------------------------------------------------
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
          # Barplots & Tables -------------------------------------------------------------
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
          # Preparation --------------------------------------------------------
            df.berichten.verzender <- reactive({
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
            berichten.verzender.table <- reactive({
              temp <- split(df.berichten.verzender(), df.berichten.verzender()$Verzender)
              temp <- data.frame(
                        Beleid = c("Economie", "Gouverneur", "Leefmilieu", "Mobiliteit", "Onderwijd en Educatie", "Provinciebestuur", "Ruimte", "Vrije Tijd"),
                        Persdienst = temp$Persdienst$Freq,
                        Provincie = temp$Provincie$Freq,
                        Gouverneur = temp$Gouverneur$Freq,
                        Extern = temp$Extern$Freq
              )
            })
            output$berichten.verzender.table <- renderTable({
              berichten.verzender.table()
            })
          # Barplot ------------------------------------------------------------
            berichten.verzender.barplot <- reactive({
              # Specify color pallete
              colors <- brewer.pal(8,"Pastel2")
              # Create plot
              ggplot(data=df.berichten.verzender(), aes(x=Verzender, y=Freq, fill=Beleid)) +
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
            output$berichten.verzender.barplot <- renderPlot({
              berichten.verzender.barplot()
            })
      
        # Per Tijd -------------------------------------------------------------
          # Per Maand ----------------------------------------------------------
            # Totaal per Maand -------------------------------------------------
              # Preparation ------------------------------------------------------
                df.berichten.Maand <- reactive({
                  berichten <- data.frame(table(Persstatistiek()$Maand))
                  colnames(berichten) <- c("Maand", "Freq")
                  berichten$Maand <- factor(berichten$Maand, levels = c("jan", "feb", "mrt", "apr", "mei", "jun", "jul", "aug", "sep", "okt", "nov", "dec"))
                  return(berichten)
                })
              # Table (Per Maand) ----------------------------------------------
                berichten.tabel.maand <- reactive({
                  df.berichten.Maand()
                })
                output$berichten.tabel.maand <- renderTable({
                  berichten.tabel.maand()
                })
              # Barplot (Per Maand) --------------------------------------------
                berichten.barplot.maand <- reactive({
                  # Specify color pallete
                  colors <- c(brewer.pal(8,"Pastel2"), brewer.pal(9, "Pastel1"))
                  # Create plot
                  ggplot(data=df.berichten.Maand(), aes(x=Maand, y=Freq, fill=Maand)) +
                    geom_bar(position = "dodge", stat='identity') +
                    xlab("Maand") +
                    ylab("Aantal") +
                    ggtitle("Persberichten per Maand") +
                    geom_text(aes(label=Freq),
                              position=position_dodge(0.9), vjust=0) +
                    theme_bw() +
                    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
                    scale_fill_manual(values=colors)
                })
                output$berichten.barplot.maand <- renderPlot({
                  berichten.barplot.maand()
                })
            
            # Totaal Maand per Beleid ------------------------------------------
              # Preparation ------------------------------------------------------
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
            
          # Per Jaar -----------------------------------------------------------
            # Per Kwartaal -----------------------------------------------------
              # Preparation ------------------------------------------------------
                df.berichten.Kwartaal <-  reactive({
                  berichten <- data.frame(table(Persstatistiek()$Kwartaal))
                  colnames(berichten) <- c("Kwartaal", "Freq")
                  return(berichten)
                })
              # Tabel ------------------------------------------------------------
                output$berichten.tabel.kwartaal <- renderTable({
                  df.berichten.Kwartaal()
                })
              # Barplot ----------------------------------------------------------
                berichten.barplot.kwartaal <- reactive({
                  # Specify color pallete
                  colors <- c(brewer.pal(8,"Pastel2"), brewer.pal(9, "Pastel1"))
                  # Create plot
                  ggplot(data=df.berichten.Kwartaal(), aes(x=Kwartaal, y=Freq, fill=Kwartaal)) +
                    geom_bar(position = "dodge", stat='identity') +
                    xlab("Kwartaal") +
                    ylab("Aantal") +
                    ggtitle("Persberichten per Kwartaal") +
                    geom_text(aes(label=Freq),
                              position=position_dodge(0.9), vjust=0) +
                    theme_bw() +
                    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
                    scale_fill_manual(values=colors)
                })
                output$berichten.barplot.kwartaal <- renderPlot({
                  berichten.barplot.kwartaal()
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

      # HTML aanmaak ----------------------------------------------------------
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
                                 persberichten.beleid.maand.economie.barplot = Persberichten.beleid.maand(reactive(df.berichten.Maand.totaal.per.Beleid()), reactive("Economie"))(),
                              # Per deelbeleid
                                 persberichten.beleid.economie.barplot = persberichten.beleid.economie.barplot(),
                                 persberichten.beleid.economie.tabel = persberichten.beleid.economie.tabel(),
                            # 1.2.2 Gouverneur ----------------------------------
                              # Per maand
                                 persberichten.beleid.maand.gouverneur.barplot = Persberichten.beleid.maand(reactive(df.berichten.Maand.totaal.per.Beleid()), reactive("Gouverneur"))(),
                              # Per deelbeleid
                                 persberichten.beleid.gouverneur.barplot = persberichten.beleid.gouverneur.barplot(),
                                 persberichten.beleid.gouverneur.tabel = persberichten.beleid.gouverneur.tabel(),
                            # 1.2.3 Leefmilieu ----------------------------------
                              # Per maand
                                persberichten.beleid.maand.leefmilieu.barplot = Persberichten.beleid.maand(reactive(df.berichten.Maand.totaal.per.Beleid()), reactive("Leefmilieu"))(),
                              # Per deelbeleid
                                persberichten.beleid.leefmilieu.barplot = persberichten.beleid.leefmilieu.barplot(),
                                persberichten.beleid.leefmilieu.tabel = persberichten.beleid.leefmilieu.tabel(),
                            # 1.2.4 Mobiliteit ----------------------------------
                              # Per maand
                                persberichten.beleid.maand.mobiliteit.barplot = Persberichten.beleid.maand(reactive(df.berichten.Maand.totaal.per.Beleid()), reactive("Mobiliteit"))(),
                              # Per deelbeleid
                                persberichten.beleid.mobiliteit.barplot = persberichten.beleid.mobiliteit.barplot(),
                                persberichten.beleid.mobiliteit.tabel = persberichten.beleid.mobiliteit.tabel(),
                            # 1.2.5 Onderwijs en Educatie -----------------------
                              # Per maand
                                persberichten.beleid.maand.onderwijs.barplot = Persberichten.beleid.maand(reactive(df.berichten.Maand.totaal.per.Beleid()), reactive("Onderwijs en Educatie"))(),
                              # Per deelbeleid
                                persberichten.beleid.onderwijs.barplot = persberichten.beleid.onderwijs.barplot(),
                                persberichten.beleid.onderwijs.tabel = persberichten.beleid.onderwijs.tabel(),
                            # 1.2.6 Provinciebestuur ----------------------------
                              # Per maand
                                persberichten.beleid.maand.provinciebestuur.barplot = Persberichten.beleid.maand(reactive(df.berichten.Maand.totaal.per.Beleid()), reactive("Provinciebestuur"))(),
                              # Per deelbeleid
                                persberichten.beleid.provinciebestuur.barplot = persberichten.beleid.provinciebestuur.barplot(),
                                persberichten.beleid.provinciebestuur.tabel = persberichten.beleid.provinciebestuur.tabel(),
                            # 1.2.7 Ruimte --------------------------------------
                              # Per maand
                                persberichten.beleid.maand.ruimte.barplot = Persberichten.beleid.maand(reactive(df.berichten.Maand.totaal.per.Beleid()), reactive("Ruimte"))(),
                              # Per deelbeleid
                                persberichten.beleid.ruimte.barplot = persberichten.beleid.ruimte.barplot(),
                                persberichten.beleid.ruimte.tabel = persberichten.beleid.ruimte.tabel(),
                            # 1.2.8 Vrije Tijd ----------------------------------
                              # Per maand
                                persberichten.beleid.maand.vrijetijd.barplot = Persberichten.beleid.maand(reactive(df.berichten.Maand.totaal.per.Beleid()), reactive("Vrije Tijd"))(),
                              # Per deelbeleid
                                persberichten.beleid.vrijetijd.barplot = persberichten.beleid.vrijetijd.barplot(),
                                persberichten.beleid.vrijetijd.tabel = persberichten.beleid.vrijetijd.tabel(),
                           # 1.3 Persberichten per verzender --------------------
                              # 1.3.1 Algemeen
                                berichten.verzender.table = berichten.verzender.table(),
                                berichten.verzender.barplot = berichten.verzender.barplot(),
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
    )
}