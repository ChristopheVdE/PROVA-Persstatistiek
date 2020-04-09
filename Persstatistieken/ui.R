###############################################################################
# SHINY APP (Persstatistiek): UI
###############################################################################

# PACKAGES ====================================================================
library(shiny)
library(shinydashboard)
# =============================================================================

source("./Modules/Persberichten/Algemeen/Per_maand/maand_plot.R")
source("./Modules/Persberichten/Algemeen/Per_beleid/beleid_plot.R")
source("./Modules/Persberichten/data_visualisation.R")

# UI ==========================================================================
ui <- dashboardPage(
  # TITLE =====================================================================
  dashboardHeader(
    title = "Persstatistiek"
  ),
  # SIDEBAR ===================================================================
  dashboardSidebar(
    sidebarMenu(
      tags$br(),
    # Input -------------------------------------------------------------------
      menuItem("Input", tabName = "Input",
               menuSubItem("Settings", tabName = "Settings", icon = icon("fas fa-cog", lib = "font-awesome")),
               menuSubItem("Data", tabName = "Data", icon = icon("fas fa-table", lib = "font-awesome"))),
    # Persberichten -----------------------------------------------------------
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
    # Persreturn --------------------------------------------------------------
      menuItem("Persreturn", tabname = "Persreturn", icon = icon("bar-chart-o"),
               menuSubItem("Per Beleid", tabName = "Return_Beleid"),
               menuSubItem("Per Platform", tabName = "Return_Platform")
      ),
    # Download ----------------------------------------------------------------
      menuItem("Download", tabName = "Download"),
      downloadButton("report", "Generate report")
    )
  ),
  # BODY ======================================================================
  dashboardBody(
    tabItems(
      # Input scherm ----------------------------------------------------------
      tabItem(
        tabName = "Settings",
        fluidRow(
        # File input & options ------------------------------------------------
          box(
            title = "File input",
            width = 5,
          # File input
            fileInput("file", 
                      "Kies Excel document:",
                      multiple = FALSE,
                      accept = c(".xls", ".xlsx"),
                      width = 900,
                      placeholder = "No file selected"),
          # Werkblad
            textInput("sheet", "Te gebruiken Werkblad", value ="Hele organisatie", placeholder = "Hele organisatie" ),
          # Headers  
            checkboxInput("headers", label = "Eerste rij bevat kolomnamen", value = TRUE),
          # Jaar
            selectInput("jaar", 
                        label = "Jaar", 
                        choices = c(format(Sys.Date(), "%Y"):2010),
                        selected = format(Sys.Date(), "%Y")),
          # Kwartaal
            selectInput("kwartaal",
                        label = "Selecteer kwartaal:",
                        choices = c("Q1", "Q2", "Q3", "Q4", "Jaar"),
                        selected = "Q1")
          ),
        # Kolom selectie ------------------------------------------------------
          box(
            title = "Selecteer kolomen die overenkomen met kolominhoud (indien geen kolomnnamen in Excel)",
            width = 7,
            column(
              width = 3,
              tags$br(tags$b("Kwartaal")),
              tags$br(tags$b("Verzender")),
              tags$br(tags$b("Beleid")),
              tags$br(tags$b("Deelbeleid")),
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
          )
        )
      ),
      # Datatable visualisation ---------------------------------------------
      tabItem(
        tabName = "Data",
        fluidRow(
          box(
            title = "Processed uploaded data",
            width = 12,
            tableOutput("table"),
          )
        )
      ),
      # Persberichten ---------------------------------------------------------
        # Algemeen ------------------------------------------------------------
          tabItem(
            tabName = "Persbericht-algemeen",
            fluidRow(
              tabBox(
                title = "Persberichten per Kwartaal",
                width = 6,
                tabPanel("Barplot", plotOutput("persberichten.alg.kwartaal.plot")),
                tabPanel("Tabel", tableOutput("persberichten.alg.kwartaal.tabel")),
                data.visualOutput("bericht.alg.kwartaal", plottitle = "Persberichten per Kwartaal", Xaxis = "Kwartaal")
              ),
              tabBox(
                title = "Persberichten: Totaal per Maand",
                width = 6,
                tabPanel("Barplot", plotOutput("persberichten.alg.maand.plot")),
                tabPanel("Tabel", tableOutput("persberichten.alg.maand.tabel")),
                data.visualOutput("bericht.alg.maand", plottitle = "Persberichten per maand", Xaxis = "Maand")
              ),
              tabBox(
                width = 12,
                title = "Persberichten per beleid",
                tabPanel("Barplot", plotOutput("persberichten.alg.beleid.plot")),
                tabPanel("Tabel", tableOutput("persberichten.alg.beleid.tabel")),
                data.visualOutput("bericht.alg.beleid", plottitle = "Persberichten per beleid", Xaxis = "Beleid")
                # data.visualOutput("bericht.alg.beleid.plot", plottitle = "Persberichten per beleid")
              ),
            )
          ),
        # Per beleid ----------------------------------------------------------
          # Per Maand ---------------------------------------------------------
            tabItem(
              tabName = "Persbericht-beleid-maand",
              fluidRow(
                tabBox(
                  width = 6,
                  title = "Economie",
                  tabPanel("Barplot", plotOutput("persberichten.beleid.maand.economie.plot")),
                  tabPanel("Tabel", tableOutput("persberichten.beleid.maand.economie.tabel")),
                  data.visualOutput("bericht.beleid.maand.plot.economie", plottitle = "Persberichten: Maand per beleid (Economie)", Xaxis = "Maand")
                ),
                tabBox(
                  width = 6,
                  title = "Gouverneur",
                  tabPanel("Barplot", plotOutput("persberichten.beleid.maand.gouverneur.plot")),
                  tabPanel("Tabel", tableOutput("persberichten.beleid.maand.gouverneur.tabel")),
                  data.visualOutput("bericht.beleid.maand.plot.gouverneur", plottitle = "Persberichten: Maand per beleid (Gouverneur)", Xaxis = "Maand")
                ),
                tabBox(
                  width = 6,
                  title = "Leefmilieu",
                  tabPanel("Barplot", plotOutput("persberichten.beleid.maand.leefmilieu.plot")),
                  tabPanel("Tabel", tableOutput("persberichten.beleid.maand.leefmilieu.tabel")),
                  data.visualOutput("bericht.beleid.maand.plot.leefmilieu", plottitle = "Persberichten: Maand per beleid (Leefmilieu)", Xaxis = "Maand")
                ),
                tabBox(
                  width = 6,
                  title = "Mobiliteit",
                  tabPanel("Barplot", plotOutput("persberichten.beleid.maand.mobiliteit.plot")),
                  tabPanel("Tabel", tableOutput("persberichten.beleid.maand.mobiliteit.tabel")),
                  data.visualOutput("bericht.beleid.maand.plot.mobiliteit", plottitle = "Persberichten: Maand per beleid (Mobiliteit)", Xaxis = "Maand")
                ),
                tabBox(
                  width = 6,
                  title = "Onderwijs en Educatie",
                  tabPanel("Barplot", plotOutput("persberichten.beleid.maand.onderwijs.plot")),
                  tabPanel("Tabel", tableOutput("persberichten.beleid.maand.onderwijs.tabel")),
                  data.visualOutput("bericht.beleid.maand.plot.onderwijs", plottitle = "Persberichten: Maand per beleid (Onderwijs en Educatie)", Xaxis = "Maand")
                ),
                tabBox(
                  width = 6,
                  title = "Provinciebestuur",
                  tabPanel("Barplot", plotOutput("persberichten.beleid.maand.provinciebestuur.plot")),
                  tabPanel("Tabel", tableOutput("persberichten.beleid.maand.provinciebestuur.tabel")),
                  data.visualOutput("bericht.beleid.maand.plot.provinciebestuur", plottitle = "Persberichten: Maand per beleid (Provinciebestuur)", Xaxis = "Maand")
                ),
                tabBox(
                  width = 6,
                  title = "Ruimte",
                  tabPanel("Barplot", plotOutput("persberichten.beleid.maand.ruimte.plot")),
                  tabPanel("Tabel", tableOutput("persberichten.beleid.maand.ruimte.tabel")),
                  data.visualOutput("bericht.beleid.maand.plot.ruimte", plottitle = "Persberichten: Maand per beleid (Ruimte)", Xaxis = "Maand")
                ),
                tabBox(
                  width = 6,
                  title = "Vrije Tijd",
                  tabPanel("Barplot", plotOutput("persberichten.beleid.maand.vrijetijd.plot")),
                  tabPanel("Tabel", tableOutput("persberichten.beleid.maand.vrijetijd.tabel")),
                  data.visualOutput("bericht.beleid.maand.plot.vrijetijd", plottitle = "Persberichten: Maand per beleid (Vrije Tijd)", Xaxis = "Maand")
                )
              )
            ),
          # Per Beleid --------------------------------------------------------
            tabItem(
              tabName = "Persbericht-beleid-beleid",
              fluidRow(
                tabBox(
                  width = 6,
                  title = "Persberichten: Economie",
                  tabPanel("Barplot", plotOutput("persberichten.beleid.beleid.economie.plot")),
                  tabPanel("Tabel", tableOutput("persberichten.beleid.beleid.economie.tabel")),
                  data.visualOutput("bericht.beleid.beleid.plot.economie", plottitle = "Persberichten: Deelbeleid per beleid (Economie)", Xaxis = "Deelbeleid")
                ),
                tabBox(
                  width = 6,
                  title = "Persberichten: Gouverneur",
                  tabPanel("Barplot", plotOutput("persberichten.beleid.beleid.gouverneur.plot")),
                  tabPanel("Tabel", tableOutput("persberichten.beleid.beleid.gouverneur.tabel")),
                  data.visualOutput("bericht.beleid.beleid.plot.gouverneur", plottitle = "Persberichten: Deelbeleid per beleid (Gouverneur)", Xaxis = "Deelbeleid")
                ),
                tabBox(
                  width = 6,
                  title = "Persberichten: Leefmilieu",
                  tabPanel("Barplot", plotOutput("persberichten.beleid.beleid.leefmilieu.plot")),
                  tabPanel("Tabel", tableOutput("persberichten.beleid.beleid.leefmilieu.tabel")),
                  data.visualOutput("bericht.beleid.beleid.plot.leefmilieu", plottitle = "Persberichten: per deelbeleid (Leefmilieu)", Xaxis = "Deelbeleid")
                ),  
                tabBox(
                  width = 6,
                  title = "Persberichten: Mobiliteit",
                  tabPanel("Barplot", plotOutput("persberichten.beleid.beleid.mobiliteit.plot")),
                  tabPanel("Tabel", tableOutput("persberichten.beleid.beleid.mobiliteit.tabel")),
                  data.visualOutput("bericht.beleid.beleid.plot.mobiliteit", plottitle = "Persberichten: per deelbeleid (Mobiliteit)", Xaxis = "Deelbeleid")
                ),
                tabBox(
                  width = 6,
                  title = "Persberichten: Onderwijs en Educatie",
                  tabPanel("Barplot", plotOutput("persberichten.beleid.beleid.onderwijs.plot")),
                  tabPanel("Tabel", tableOutput("persberichten.beleid.beleid.onderwijs.tabel")),
                  data.visualOutput("bericht.beleid.beleid.plot.onderwijs", plottitle = "Persberichten: per deelbeleid (Onderwijs en Educatie)", Xaxis = "Deelbeleid")
                ),
                tabBox(
                  width = 6,
                  title = "Persberichten: Provinciebestuur",
                  tabPanel("Barplot", plotOutput("persberichten.beleid.beleid.provinciebestuur.plot")),
                  tabPanel("Tabel", tableOutput("persberichten.beleid.beleid.provinciebestuur.tabel")),
                  data.visualOutput("bericht.beleid.beleid.plot.provinciebestuur", plottitle = "Persberichten: per deelbeleid (Provinciebestuur)", Xaxis = "Deelbeleid")
                ),
                tabBox(
                  width = 6,
                  title = "Persberichten: Ruimte",
                  tabPanel("Barplot", plotOutput("persberichten.beleid.beleid.ruimte.plot")),
                  tabPanel("Tabel", tableOutput("persberichten.beleid.beleid.ruimte.tabel")),
                  data.visualOutput("bericht.beleid.beleid.plot.ruimte", plottitle = "Persberichten: per deelbeleid (Ruimte)", Xaxis = "Deelbeleid")
                ),
                tabBox(
                  width = 6,
                  title = "Persberichten: Vrije Tijd",
                  tabPanel("Barplot", plotOutput("persberichten.beleid.beleid.vrijetijd.plot")),
                  tabPanel("Tabel", tableOutput("persberichten.beleid.beleid.vrijetijd.tabel")),
                  data.visualOutput("bericht.beleid.beleid.plot.vrijetijd", plottitle = "Persberichten: per deelbeleid (Vrije Tijd)", Xaxis = "Deelbeleid")
                )
              )
            ),
        # Per Verzender -------------------------------------------------------
          # Algemeen ----------------------------------------------------------
            tabItem(
              tabName = "Persbericht-verzender-algemeen",
              fluidRow(
                tabBox(
                  title = "Persberichten per Verzender",
                  width = 12,
                  tabPanel("Barplot", plotOutput("persberichten.verzender.alg.totaal.plot")),
                  tabPanel("Table", tableOutput("persberichten.verzender.alg.totaal.tabel"))
                ),
                tabBox(
                  title = "Beleid per Verzender",
                  width = 12,
                  tabPanel("Barplot", plotOutput("persberichten.verzender.alg.beleid.plot")),
                  tabPanel("Table", tableOutput("persberichten.verzender.alg.beleid.tabel"))
                )
              )
            ),
          # Per maand ---------------------------------------------------------
            tabItem(
              tabName = "Persbericht-verzender-maand",
              fluidRow(
                tabBox(
                  title = "Persdienst",
                  width = 6,
                  tabPanel("Barplot", plotOutput("persberichten.verzender.maand.persdienst.plot")),
                  tabPanel("Tabel", tableOutput("persberichten.verzender.maand.persdienst.tabel"))
                ), 
                tabBox(
                  title = "Provincie",
                  width = 6,
                  tabPanel("Barplot", plotOutput("persberichten.verzender.maand.provincie.plot")),
                  tabPanel("Tabel", tableOutput("persberichten.verzender.maand.provincie.tabel"))
                ), 
                tabBox(
                  title = "Gouverneur",
                  width = 6,
                  tabPanel("Barplot", plotOutput("persberichten.verzender.maand.gouverneur.plot")),
                  tabPanel("Tabel", tableOutput("persberichten.verzender.maand.gouverneur.tabel"))
                ), 
                tabBox(
                  title = "Extern",
                  width = 6,
                  tabPanel("Barplot", plotOutput("persberichten.verzender.maand.extern.plot")),
                  tabPanel("Tabel", tableOutput("persberichten.verzender.maand.extern.tabel"))
                )
              )
            ),  
        # Per Type ------------------------------------------------------------
          tabItem(
            tabName = "Persbericht-type",
            fluidRow(
              tabBox(
                title = "Persberichten per Soort",
                width = 12,
                tabPanel("Barplot", plotOutput("persberichten.type.plot")),
                tabPanel("Tabel", tableOutput("persberichten.type.tabel"))
              )
            )
          ),
      
      # Persreturn ------------------------------------------------------------
        # Per beleid ----------------------------------------------------------
          tabItem(
            tabName = "Return_Beleid",
            fluidRow(
            # Algemeen ------------------------------------------------------
              tabBox(
                width = 12,
                title = "Persreturn per beleid",
                tabPanel("Barplot", plotOutput("persreturn.beleid.alg.plot")),
                tabPanel("Tabel", tableOutput("persreturn.beleid.alg.tabel"))
              ),
            # Detail --------------------------------------------------------
              tabBox(
                width = 6,
                title = "Persreturn: Economie",
                tabPanel("Barplot", plotOutput("persreturn.beleid.economie.plot")),
                tabPanel("Tabel", tableOutput("persreturn.beleid.economie.tabel"))
              ),
              tabBox(
                width = 6,
                title = "Persreturn: Gouverneur",
                tabPanel("Barplot", plotOutput("persreturn.beleid.gouverneur.plot")),
                tabPanel("Tabel", tableOutput("persreturn.beleid.gouverneur.tabel"))
              ),
              tabBox(
                width = 6,
                title = "Persreturn: Leefmilieu",
                tabPanel("Barplot", plotOutput("persreturn.beleid.leefmilieu.plot")),
                tabPanel("Tabel", tableOutput("persreturn.beleid.leefmilieu.tabel"))
              ),
              tabBox(
                width = 6,
                title = "Persreturn: Mobiliteit",
                tabPanel("Barplot", plotOutput("persreturn.beleid.mobiliteit.plot")),
                tabPanel("Tabel", tableOutput("persreturn.beleid.mobiliteit.tabel"))
              ),
              tabBox(
                width = 6,
                title = "Persreturn: Onderwijs en Educatie",
                tabPanel("Barplot", plotOutput("persreturn.beleid.onderwijs.plot")),
                tabPanel("Tabel", tableOutput("persreturn.beleid.onderwijs.tabel"))
              ),
              tabBox(
                width = 6,
                title = "Persreturn: Provinciebestuur",
                tabPanel("Barplot", plotOutput("persreturn.beleid.provinciebestuur.plot")),
                tabPanel("Tabel", tableOutput("persreturn.beleid.provinciebestuur.tabel"))
              ),
              tabBox(
                width = 6,
                title = "Persreturn: Ruimte",
                tabPanel("Barplot", plotOutput("persreturn.beleid.ruimte.plot")),
                tabPanel("Tabel", tableOutput("persreturn.beleid.ruimte.tabel"))
              ),
              tabBox(
                width = 6,
                title = "Persreturn: Vrije Tijd",
                tabPanel("Barplot", plotOutput("persreturn.beleid.vrijetijd.plot")),
                tabPanel("Tabel", tableOutput("persreturn.beleid.vrijetijd.tabel"))
              )
            )
          ),
      # Per Platform ----------------------------------------------------------
        tabItem(
          tabName = "Return_Platform",
          fluidRow(
            tabBox(
              title = "Persreturn per platform",
              width = 12,
              tabPanel("Barplot", plotOutput("persreturn.platform.plot")),
              tabPanel("Tabel", tableOutput("persreturn.platform.tabel"))
            )
          )
        )
    )
  )
)

