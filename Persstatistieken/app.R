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
            output$berichten.type.table <- renderTable({
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
            output$berichten.verzender.table <- renderTable({
              temp <- split(df.berichten.verzender(), df.berichten.verzender()$Verzender)
              temp <- data.frame(
                        Beleid = c("Economie", "Gouverneur", "Leefmilieu", "Mobiliteit", "Onderwijd en Educatie", "Provinciebestuur", "Ruimte", "Vrije Tijd"),
                        Persdienst = temp$Persdienst$Freq,
                        Provincie = temp$Provincie$Freq,
                        Gouverneur = temp$Gouverneur$Freq,
                        Extern = temp$Extern$Freq
              )
            })
          # Barplot ------------------------------------------------------------
            output$berichten.verzender.barplot <- renderPlot({
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
              # Table (Per Maand) --------------------------------------------------
                output$berichten.tabel.maand <- renderTable({
                  df.berichten.Maand()
                })
              # Barplot (Per Maand) ------------------------------------------------
                output$berichten.barplot.maand <- renderPlot({
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
            # Totaal Maand per Beleid ) ----------------------------------------
              # Preparation ------------------------------------------------------
              df.berichten.Maand.totaal.per.Beleid <-  reactive({
                berichten <- data.frame(table(Persstatistiek()$Beleid, Persstatistiek()$Maand))
                colnames(berichten) <- c("Beleid", "Maand", "Freq")
                berichten$Maand <- factor(berichten$Maand, levels = c("jan", "feb", "mrt", "apr", "mei", "jun", "jul", "aug", "sep", "okt", "nov", "dec"))
                split(berichten, berichten$Beleid)
              })
              # Piechart (Per Maand) ---------------------------------------------
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
            # Per tijd (Kwartaal)  ---------------------------------------------
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
                output$berichten.barplot.kwartaal <- renderPlot({
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
              colnames(Web) <- c("Beleid", "Web", "Freq")
              Web$Web <- "Web"

              # Merge dataframes
              persreturn <- data.frame(Beleid = TV$Beleid,
                                       Platform = c(Algemeen$Algemeen, TV$TV, Web$Web),
                                       Persreturn = c(Algemeen$Freq, TV$Freq, Web$Freq))
              return(persreturn)
            })
          # Table --------------------------------------------------------------
            output$return.platform.table <-renderTable({
              df.return.platform()
            })
          # Barplot ------------------------------------------------------------
            output$return.platform.barplot <- renderPlot({
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


        # Pdf aanmaak ----------------------------------------------------------
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
                           kwartaal = input$kwartaal)
            
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