library(shiny)
library(shinydashboard)

# UI =======================================================================
ui <- dashboardPage(
  # Title ------------------------------------------------------------
  dashboardHeader(
    title = "Persstatistiek"
  ),
  # Sidebar ---------------------------------------------------
  dashboardSidebar(
    sidebarMenu(
      tags$br(),
      menuItem("Input", tabName = "Input"),
      menuItem("Persberichten", tabname = "Persberichten", icon = icon("bar-chart-o"),
               menuSubItem("Algemeen", tabName = "Persbericht-algemeen"), 
               menuItem("Per Beleid", tabName = "Persbericht-beleid", icon = icon("angle-double-right", lib = "font-awesome"), 
                        menuSubItem("Per Maand", tabName = "Persbericht-beleid-maand"),
                        menuSubItem("Per Beleid", tabName = "Persbericht-beleid-beleid")
               ),
               menuItem("Per Verzender", tabName = "Persbericht-verzender", icon = icon("angle-double-right", lib = "font-awesome"),
                        menuSubItem("Algemeen", tabName = "Persbericht-verzender-algemeen"),
                        menuSubItem("Per Maand", tabName = "Persbericht-verzender-maand")
               ),
               menuSubItem("Per Type", tabName = "Persbericht-type")
      ),
      menuItem("Persreturn", tabname = "Persreturn", icon = icon("bar-chart-o"),
               menuSubItem("Per Beleid", tabName = "Return_Beleid"),
               menuSubItem("Per Platform", tabName = "Return_Platform")
      ),
      menuItem("Download", tabName = "Download"),
      
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
        # Algemeen -------------------------------------------------------
          tabItem(
            tabName = "Persbericht-algemeen",
            fluidRow(
              tabBox(
                title = "Persberichten per Kwartaal",
                width = 6,
                tabPanel("Barplot", plotOutput("persberichten.alg.kwartaal.plot")),
                tabPanel("Tabel", tableOutput("persberichten.alg.kwartaal.tabel"))
              ),
              tabBox(
                title = "Persberichten: Totaal per Maand",
                width = 6,
                tabPanel("Barplot", plotOutput("persberichten.alg.maand.plot")),
                tabPanel("Tabel", tableOutput("persberichten.alg.maand.tabel"))
              ),
              tabBox(
                width = 12,
                title = "Persberichten per beleid",
                tabPanel("Barplot", plotOutput("persberichten.alg.beleid.plot")),
                tabPanel("Tabel", tableOutput("persberichten.alg.beleid.tabel"))
              ),
            )
          ),
        # Per beleid -----------------------------------------------------
          # Per Maand ----------------------------------------------------
            tabItem(
              tabName = "Persbericht-beleid-maand",
              fluidRow(
                tabBox(
                  width = 6,
                  title = "Economie",
                  tabPanel("Barplot", plotOutput("persberichten.beleid.maand.economie.plot")),
                  tabPanel("Tabel", tableOutput("persberichten.beleid.maand.economie.tabel"))
                ),
                tabBox(
                  width = 6,
                  title = "Gouverneur",
                  tabPanel("Barplot", plotOutput("persberichten.beleid.maand.gouverneur.plot")),
                  tabPanel("Tabel", tableOutput("persberichten.beleid.maand.gouverneur.tabel"))
                ),
                tabBox(
                  width = 6,
                  title = "Leefmilieu",
                  tabPanel("Barplot", plotOutput("persberichten.beleid.maand.leefmilieu.plot")),
                  tabPanel("Tabel", tableOutput("persberichten.beleid.maand.leefmilieu.tabel"))
                ),
                tabBox(
                  width = 6,
                  title = "Mobiliteit",
                  tabPanel("Barplot", plotOutput("persberichten.beleid.maand.mobiliteit.plot")),
                  tabPanel("Tabel", tableOutput("persberichten.beleid.maand.mobiliteit.tabel"))
                ),
                tabBox(
                  width = 6,
                  title = "Onderwijs en Educatie",
                  tabPanel("Barplot", plotOutput("persberichten.beleid.maand.onderwijs.plot")),
                  tabPanel("Tabel", tableOutput("persberichten.beleid.maand.onderwijs.tabel"))
                ),
                tabBox(
                  width = 6,
                  title = "Provinciebestuur",
                  tabPanel("Barplot", plotOutput("persberichten.beleid.maand.provinciebestuur.plot")),
                  tabPanel("Tabel", tableOutput("persberichten.beleid.maand.provinciebestuur.tabel"))
                ),
                tabBox(
                  width = 6,
                  title = "Ruimte",
                  tabPanel("Barplot", plotOutput("persberichten.beleid.maand.ruimte.plot")),
                  tabPanel("Tabel", tableOutput("persberichten.beleid.maand.ruimte.tabel"))
                ),
                tabBox(
                  width = 6,
                  title = "Vrije Tijd",
                  tabPanel("Barplot", plotOutput("persberichten.beleid.maand.vrijetijd.plot")),
                  tabPanel("Tabel", tableOutput("persberichten.beleid.maand.vrijetijd.tabel"))
                )
              )
            ),
          # Per Beleid ---------------------------------------------------
            tabItem(
              tabName = "Persbericht-beleid-beleid",
              fluidRow(
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
              )
            ),
        # Per Verzender --------------------------------------------
          # Algemeen -----------------------------------------------
            tabItem(
              tabName = "Persbericht-verzender-algemeen",
              fluidRow(
                tabBox(
                  title = "Persberichten per Verzender",
                  width = 12,
                  tabPanel("Barplot", plotOutput("berichten.verzender.barplot")),
                  tabPanel("Table", tableOutput("berichten.verzender.table"))
                ),
                tabBox(
                  title = "Beleid per Verzender",
                  width = 12,
                  tabPanel("Barplot", plotOutput("berichten.verzender.beleid.barplot")),
                  tabPanel("Table", tableOutput("berichten.verzender.beleid.table"))
                )
              )
            ),
          # Per maand ----------------------------------------------
            tabItem(
              tabName = "Persbericht-verzender-maand",
              fluidRow(
                tabBox(
                  title = "Januari",
                  width = 6,
                  tabPanel("Barplot", plotOutput("berichten.verzender.januari.barplot")),
                  tabPanel("Table", tableOutput("berichten.verzender.januari.table"))
                ), 
                tabBox(
                  title = "Februari",
                  width = 6,
                  tabPanel("Barplot", plotOutput("berichten.verzender.februari.barplot")),
                  tabPanel("Table", tableOutput("berichten.verzender.februari.table"))
                ), 
                tabBox(
                  title = "Maart",
                  width = 6,
                  tabPanel("Barplot", plotOutput("berichten.verzender.maart.barplot")),
                  tabPanel("Table", tableOutput("berichten.verzender.maart.table"))
                ), 
                tabBox(
                  title = "April",
                  width = 6,
                  tabPanel("Barplot", plotOutput("berichten.verzender.april.barplot")),
                  tabPanel("Table", tableOutput("berichten.verzender.april.table"))
                ), 
                tabBox(
                  title = "Mei",
                  width = 6,
                  tabPanel("Barplot", plotOutput("berichten.verzender.mei.barplot")),
                  tabPanel("Table", tableOutput("berichten.verzender.mei.table"))
                ), 
                tabBox(
                  title = "Juni",
                  width = 6,
                  tabPanel("Barplot", plotOutput("berichten.verzender.juni.barplot")),
                  tabPanel("Table", tableOutput("berichten.verzender.juni.table"))
                ), 
                tabBox(
                  title = "Juli",
                  width = 6,
                  tabPanel("Barplot", plotOutput("berichten.verzender.juli.barplot")),
                  tabPanel("Table", tableOutput("berichten.verzender.juli.table"))
                ), 
                tabBox(
                  title = "Augustus",
                  width = 6,
                  tabPanel("Barplot", plotOutput("berichten.verzender.augustus.barplot")),
                  tabPanel("Table", tableOutput("berichten.verzender.augustus.table"))
                ), 
                tabBox(
                  title = "September",
                  width = 6,
                  tabPanel("Barplot", plotOutput("berichten.verzender.september.barplot")),
                  tabPanel("Table", tableOutput("berichten.verzender.september.table"))
                ), 
                tabBox(
                  title = "Oktober",
                  width = 6,
                  tabPanel("Barplot", plotOutput("berichten.verzender.oktober.barplot")),
                  tabPanel("Table", tableOutput("berichten.verzender.oktober.table"))
                ), 
                tabBox(
                  title = "November",
                  width = 6,
                  tabPanel("Barplot", plotOutput("berichten.verzender.november.barplot")),
                  tabPanel("Table", tableOutput("berichten.verzender.november.table"))
                ), 
                tabBox(
                  title = "December",
                  width = 6,
                  tabPanel("Barplot", plotOutput("berichten.verzender.december.barplot")),
                  tabPanel("Table", tableOutput("berichten.verzender.december.table"))
                )
              )
            ),  
        # Per Type -------------------------------------------------
          tabItem(
            tabName = "Persbericht-type",
            fluidRow(
              tabBox(
                title = "Persberichten per Soort",
                width = 12,
                tabPanel("Barplot", plotOutput("berichten.type.barplot")),
                tabPanel("Tabel", tableOutput("berichten.type.table"))
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
)

