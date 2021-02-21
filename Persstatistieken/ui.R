################################################################################
# SHINY APP (Persstatistiek): UI
################################################################################

# PACKAGES =====================================================================
library(shiny)
library(shinydashboard)
library(RColorBrewer)
library(colourpicker)
library(shinyWidgets)
library(DT)
# ==============================================================================

source("./Modules/JaarOverzicht/data_visualisation.R")

# UI ===========================================================================
ui <- dashboardPage(
  # TITLE ======================================================================
  dashboardHeader(
    title = "Persstatistiek"
  ),
  # SIDEBAR ====================================================================
  dashboardSidebar(
    sidebarMenu(
      tags$br(),
    # Input --------------------------------------------------------------------
      menuItem("Input", tabName = "Input", startExpanded = TRUE,
               menuSubItem("Settings", tabName = "Settings", icon = icon("fas fa-cog", lib = "font-awesome")),
               menuSubItem("Kleuren", tabName = "Kleuren", icon = icon("fas fa-palette", lib = 'font-awesome')),
               menuSubItem("Data", tabName = "Data", icon = icon("fas fa-table", lib = "font-awesome"))),
    # Statistieken voor geselcteed jaar -----------------------------------------
      tags$hr(),
      menuSubItem("Statistieken geslecteerd jaar", icon = NULL),
      # Persberichten ------------------------------------------------------------
        menuItem("Persberichten", tabname = "Persberichten", icon = icon("bar-chart-o"),
                 menuItem("Algemeen", tabName = "Persbericht-algemeen", icon = icon("angle-double-right", lib = "font-awesome"), 
                          menuSubItem("Persberichten", tabName = "Persbericht-alg-persbericht"),
                          menuSubItem("Persconferenties", tabName = "Persbericht-alg-persconferentie")
                 ),          
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
      # Persreturn -------------------------------------------------------------
        menuItem("Persreturn", tabname = "Persreturn", icon = icon("bar-chart-o"),
                 menuSubItem("Per Beleid", tabName = "Return_Beleid"),
                 menuSubItem("Per Medium", tabName = "Return_Medium")
        ),
    # Statistieken van alle jaar -----------------------------------------------
      tags$hr(),
      menuSubItem("Statistieken over alle jaren", icon = NULL),
    # Download -----------------------------------------------------------------
      tags$hr(),
      menuItem("Download", tabName = "Download"),
      textOutput("checkrender")
      # downloadButton("report", "Generate report")
    )
  ),
  # BODY =======================================================================
  dashboardBody(
    tabItems(
    # Input scherm -------------------------------------------------------------
      # Settings ---------------------------------------------------------------
      tabItem(
        tabName = "Settings",
        fluidRow(
          column(
            width = 6,
        # File input & options -------------------------------------------------
            box(
              title = "File input",
              width = 12,
              height = "75%",
            # File input
              fileInput("file", 
                        "Kies Excel document:",
                        multiple = FALSE,
                        accept = c(".xls", ".xlsx"),
                        width = 900,
                        placeholder = "No file selected"),
            # Werkblad
              textInput("sheet", "Te gebruiken Werkblad", value ="Hele Organisatie", placeholder = "Hele Organisatie" ),
            # Headers  
              checkboxInput("headers", label = "Eerste rij bevat kolomnamen", value = TRUE),
              hr(),
            # Basisdata
              textInput("basisSheet", "Werkblad met basisdata en relaties", value ="Dependencies", placeholder = "Dependencies" ),
              textInput("rangeDeelbeleid", "Kolom-range tabel Deelbeleiden", value='H:I', placeholder = 'H:I'),
              hr(),
            # Jaar
              selectInput("jaar", 
                          label = "Jaar", 
                          choices = NULL,
                          selected = NULL),
            # Kwartaal
              selectInput("kwartaal",
                          label = "Selecteer kwartaal:",
                          choices = NULL,
                          selected = NULL)
            ),
        # Kolom selectie -------------------------------------------------------
            conditionalPanel(
              condition = "input.headers == false",
              box(
                title = "Selecteer kolomen die overenkomen met kolominhoud",
                width = 12,
                column(
                  width = 6,
                  textInput("col.verzender", label = "Verzender", value = 2, placeholder = 2),
                  textInput("col.beleid", label = "Beleid", value = 10, placeholder = 10),
                  textInput("col.detail", label = "Deelbeleid", value = 11, placeholder = 11),
                  textInput("col.type", label = "Type persbericht", value = 12, placeholder = 12),
                  textInput("col.persconferentie", label = "Datum persconferentie", value = 5, placeholder = 5)
                ),
                column(
                  width = 6,
                  textInput("col.return.algemeen", label = "Persreturn: algemeen", value = 7, placeholder = 7),
                  textInput("col.return.web", label = "Persreturn: web", value = 8, placeholder = 8),
                  textInput("col.return.tv", label = "Persreturn: TV", value = 9, placeholder = 9),
                  textInput("col.datum", label = "Datum", value = 13, placeholder = 13)
                )
              )
            )
          ),
        # Selecteer actieve deelbeleiden per beleid ----------------------------
          column(
            width = 6,
            box(
              title = 'Selecteer actieve deelbeleiden per beleid',
              width = 12,
              pickerInput(
                inputId = 'Economie.ActieveDeelbeleiden',
                label = 'Economie:',
                choices = NULL,
                selected = NULL,
                multiple = TRUE,
                options = list(`actions-box` = TRUE, `live-search`=TRUE)
              ),
              pickerInput(
                inputId = 'Gouverneur.ActieveDeelbeleiden',
                label = 'Gouverneur:',
                choices = NULL,
                selected = NULL,
                multiple = TRUE,
                options = list(`actions-box` = TRUE, `live-search`=TRUE)
              ),
              pickerInput(
                inputId = 'Leefmilieu.ActieveDeelbeleiden',
                label = 'Leefmilieu:',
                choices = NULL,
                selected = NULL,
                multiple = TRUE,
                options = list(`actions-box` = TRUE, `live-search`=TRUE)
              ),
              pickerInput(
                inputId = 'Mobiliteit.ActieveDeelbeleiden',
                label = 'Mobiliteit:',
                choices = NULL,
                selected = NULL,
                multiple = TRUE,
                options = list(`actions-box` = TRUE, `live-search`=TRUE)
              ),
              pickerInput(
                inputId = 'Onderwijs.ActieveDeelbeleiden',
                label = 'Onderwijs en Educatie:',
                choices = NULL,
                selected = NULL,
                multiple = TRUE,
                options = list(`actions-box` = TRUE, `live-search`=TRUE)
              ),
              pickerInput(
                inputId = 'Provinciebestuur.ActieveDeelbeleiden',
                label = 'Provinciebestuur:',
                choices = NULL,
                selected = NULL,
                multiple = TRUE,
                options = list(`actions-box` = TRUE, `live-search`=TRUE)
              ),
              pickerInput(
                inputId = 'Ruimte.ActieveDeelbeleiden',
                label = 'Economie:',
                choices = NULL,
                selected = NULL,
                multiple = TRUE,
                options = list(`actions-box` = TRUE, `live-search`=TRUE)
              ),
              pickerInput(
                inputId = 'VrijeTijd.ActieveDeelbeleiden',
                label = 'Vrije Tijd:',
                choices = NULL,
                selected = NULL,
                multiple = TRUE,
                options = list(`actions-box` = TRUE, `live-search`=TRUE)
              ),
            )
          )
        )
      ),
      # Color chooser ----------------------------------------------------------
      tabItem(
        tabName = "Kleuren",
        fluidRow(
          column(
            width = 6,
            box(
              title = "Plot kleuren: Persberichten",
              width = 12,
              column(
                width = 6,
                colourInput("colour1", "Colour 1", brewer.pal(8,"Pastel2")[1]),
                colourInput("colour2", "Colour 2", brewer.pal(8,"Pastel2")[2]),
                colourInput("colour3", "Colour 3", brewer.pal(8,"Pastel2")[3]),
                colourInput("colour4", "Colour 4", brewer.pal(8,"Pastel2")[4]),
                colourInput("colour5", "Colour 5", brewer.pal(8,"Pastel2")[5]),
                colourInput("colour6", "Colour 6", brewer.pal(8,"Pastel2")[6]),
                colourInput("colour7", "Colour 7", brewer.pal(8,"Pastel2")[7])
              ),
              column(
                width = 6,
                colourInput("colour8", "Colour 8", brewer.pal(8,"Pastel2")[8]),
                colourInput("colour9", "Colour 9", brewer.pal(8,"Pastel1")[1]),
                colourInput("colour10", "Colour 10", brewer.pal(8,"Pastel1")[2]),
                colourInput("colour11", "Colour 11", brewer.pal(8,"Pastel1")[3]),
                colourInput("colour12", "Colour 12", brewer.pal(8,"Pastel1")[4]),
                colourInput("colour13", "Colour 13", brewer.pal(8,"Pastel1")[5]),
                colourInput("colour14", "Colour 14", brewer.pal(8,"Pastel1")[6])
              )
            )
          ),
          column(
            width = 6,
            box(
              title = "Plot kleuren: Persreturn",
              width = 12,
              colourInput("return.colour1", "Persreturn: Yes", brewer.pal(8,"Pastel2")[1]),
              colourInput("return.colour2", "Persreturn: No", brewer.pal(8,"Pastel2")[2])
            )
          )
        )
      ),
      # Datatable visualisation ------------------------------------------------
      tabItem(
        tabName = "Data",
        fluidRow(
          box(
            title = "Processed uploaded data",
            width = 12,
            DT::dataTableOutput("Persstatistiek")
          ),
          box(
            title = 'Full Data',
            width = 12,
            DT::dataTableOutput("PersstatistiekFull")
          ),
          box(
            title = 'Deelbeleiden',
            width = 12,
            DT::dataTableOutput("AlleDeelbeleiden")
          )
        )
      ),
    # Persberichten ------------------------------------------------------------
      # Algemeen ---------------------------------------------------------------
        # Algemeen - Persberichten ---------------------------------------------
          tabItem(
            tabName = "Persbericht-alg-persbericht",
            fluidRow(
              tabBox(
                title = "Persberichten per Kwartaal",
                width = 6,
                tabPanel("Plot - aantal", plotOutput("persberichten.alg.kwartaal.plot.aantal")),
                tabPanel("Plot - procent", plotOutput("persberichten.alg.kwartaal.plot.procent")),
                tabPanel("Tabel", DT::dataTableOutput("persberichten.alg.kwartaal.tabel")),
                data.visualOutput("bericht.alg.kwartaal", plottitle = "Persberichten per Kwartaal", Xaxis = "Kwartaal", Xlabels = FALSE)
              ),
              tabBox(
                title = "Persberichten: Totaal per Maand",
                width = 6,
                tabPanel("Plot - aantal", plotOutput("persberichten.alg.maand.plot.aantal")),
                tabPanel("Plot - procent", plotOutput("persberichten.alg.maand.plot.procent")),
                tabPanel("Tabel", DT::dataTableOutput("persberichten.alg.maand.tabel")),
                data.visualOutput("bericht.alg.maand", plottitle = "Persberichten per maand", Xaxis = "Maand", Xlabels = FALSE)
              ),
              tabBox(
                title = "Persberichten: Totaal per Dag",
                width = 6,
                tabPanel("Plot - aantal", plotOutput("persberichten.alg.dag.plot.aantal")),
                tabPanel("Plot - procent", plotOutput("persberichten.alg.dag.plot.procent")),
                tabPanel("Tabel", DT::dataTableOutput("persberichten.alg.dag.tabel")),
                data.visualOutput("bericht.alg.dag", plottitle = "Persberichten per dag", Xaxis = "Dag", Xlabels = FALSE)
              ),
              tabBox(
                title = "Persberichten: Totaal per Week",
                width = 6,
                tabPanel("Plot - aantal", plotOutput("persberichten.alg.week.plot.aantal")),
                tabPanel("Plot - procent", plotOutput("persberichten.alg.week.plot.procent")),
                tabPanel("Tabel", DT::dataTableOutput("persberichten.alg.week.tabel")),
                data.visualOutput("bericht.alg.week", plottitle = "Persberichten per week", Xaxis = "Week", Xlabels = TRUE, Legende = FALSE)
              ),
              tabBox(
                width = 12,
                title = "Persberichten per beleid",
                tabPanel("Plot - aantal", plotOutput("persberichten.alg.beleid.plot.aantal")),
                tabPanel("Plot - procent", plotOutput("persberichten.alg.beleid.plot.procent")),
                tabPanel("Tabel", DT::dataTableOutput("persberichten.alg.beleid.tabel")),
                data.visualOutput("bericht.alg.beleid", plottitle = "Persberichten per beleid", Xaxis = "Beleid", Xlabels = FALSE)
              )
            )
          ),
        # Algemeen - Persconferenties ------------------------------------------
          tabItem(
            tabName = "Persbericht-alg-persconferentie",
            fluidRow(
              tabBox(
                title = "Persconferenties per Kwartaal",
                width = 6,
                tabPanel("Plot - aantal", plotOutput("persconferenties.alg.kwartaal.plot.aantal")),
                tabPanel("Plot - procent", plotOutput("persconferenties.alg.kwartaal.plot.procent")),
                tabPanel("Tabel", DT::dataTableOutput("persconferenties.alg.kwartaal.tabel")),
                data.visualOutput("conferentie.alg.kwartaal", plottitle = "Persconferenties per Kwartaal", Xaxis = "Kwartaal", Xlabels = FALSE)
              ),
              tabBox(
                title = "Persconferenties: Totaal per Maand",
                width = 6,
                tabPanel("Plot - aantal", plotOutput("persconferenties.alg.maand.plot.aantal")),
                tabPanel("Plot - procent", plotOutput("persconferenties.alg.maand.plot.procent")),
                tabPanel("Tabel", DT::dataTableOutput("persconferenties.alg.maand.tabel")),
                data.visualOutput("conferentie.alg.maand", plottitle = "Persconferenties per maand", Xaxis = "Maand", Xlabels = FALSE)
              ),
              tabBox(
                title = "Persconferenties: Totaal per Dag",
                width = 6,
                tabPanel("Plot - aantal", plotOutput("persconferenties.alg.dag.plot.aantal")),
                tabPanel("Plot - procent", plotOutput("persconferenties.alg.dag.plot.procent")),
                tabPanel("Tabel", DT::dataTableOutput("persconferenties.alg.dag.tabel")),
                data.visualOutput("conferentie.alg.dag", plottitle = "Persconferenties per dag", Xaxis = "Dag", Xlabels = FALSE)
              ),
              tabBox(
                title = "Persconferenties: Totaal per Week",
                width = 6,
                tabPanel("Plot - aantal", plotOutput("persconferenties.alg.week.plot.aantal")),
                tabPanel("Plot - procent", plotOutput("persconferenties.alg.week.plot.procent")),
                tabPanel("Tabel", DT::dataTableOutput("persconferenties.alg.week.tabel")),
                data.visualOutput("conferentie.alg.week", plottitle = "Persconferenties per week", Xaxis = "Week", Xlabels = TRUE, Legende = FALSE)
              ),
              tabBox(
                width = 12,
                title = "Persconferenties per beleid",
                tabPanel("Plot - aantal", plotOutput("persconferenties.alg.beleid.plot.aantal")),
                tabPanel("Plot - procent", plotOutput("persconferenties.alg.beleid.plot.procent")),
                tabPanel("Tabel", DT::dataTableOutput("persconferenties.alg.beleid.tabel")),
                data.visualOutput("conferentie.alg.beleid", plottitle = "Persconferenties per beleid", Xaxis = "Beleid", Xlabels = FALSE)
              )
            )
          ),
      # Per beleid -----------------------------------------------------------
        # Per Maand ----------------------------------------------------------
          tabItem(
            tabName = "Persbericht-beleid-maand",
            fluidRow(
              tabBox(
                width = 6,
                title = "Economie",
                tabPanel("Plot - aantal", plotOutput("persberichten.beleid.maand.economie.plot.aantal")),
                tabPanel("Plot - procent", plotOutput("persberichten.beleid.maand.economie.plot.procent")),
                tabPanel("Tabel", DT::dataTableOutput("persberichten.beleid.maand.economie.tabel")),
                data.visualOutput("bericht.beleid.maand.plot.aantal.economie", plottitle = "Persberichten: Maand per beleid (Economie)", Xaxis = "Maand", Xlabels = FALSE)
              ),
              tabBox(
                width = 6,
                title = "Gouverneur",
                tabPanel("Plot - aantal", plotOutput("persberichten.beleid.maand.gouverneur.plot.aantal")),
                tabPanel("Plot - procent", plotOutput("persberichten.beleid.maand.gouverneur.plot.procent")),
                tabPanel("Tabel", DT::dataTableOutput("persberichten.beleid.maand.gouverneur.tabel")),
                data.visualOutput("bericht.beleid.maand.plot.aantal.gouverneur", plottitle = "Persberichten: Maand per beleid (Gouverneur)", Xaxis = "Maand", Xlabels = FALSE)
              ),
              tabBox(
                width = 6,
                title = "Leefmilieu",
                tabPanel("Plot - aantal", plotOutput("persberichten.beleid.maand.leefmilieu.plot.aantal")),
                tabPanel("Plot - procent", plotOutput("persberichten.beleid.maand.leefmilieu.plot.procent")),
                tabPanel("Tabel", DT::dataTableOutput("persberichten.beleid.maand.leefmilieu.tabel")),
                data.visualOutput("bericht.beleid.maand.plot.aantal.leefmilieu", plottitle = "Persberichten: Maand per beleid (Leefmilieu)", Xaxis = "Maand", Xlabels = FALSE)
              ),
              tabBox(
                width = 6,
                title = "Mobiliteit",
                tabPanel("Plot - aantal", plotOutput("persberichten.beleid.maand.mobiliteit.plot.aantal")),
                tabPanel("Plot - procent", plotOutput("persberichten.beleid.maand.mobiliteit.plot.procent")),
                tabPanel("Tabel", DT::dataTableOutput("persberichten.beleid.maand.mobiliteit.tabel")),
                data.visualOutput("bericht.beleid.maand.plot.aantal.mobiliteit", plottitle = "Persberichten: Maand per beleid (Mobiliteit)", Xaxis = "Maand", Xlabels = FALSE)
              ),
              tabBox(
                width = 6,
                title = "Onderwijs en Educatie",
                tabPanel("Plot - aantal", plotOutput("persberichten.beleid.maand.onderwijs.plot.aantal")),
                tabPanel("Plot - procent", plotOutput("persberichten.beleid.maand.onderwijs.plot.procent")),
                tabPanel("Tabel", DT::dataTableOutput("persberichten.beleid.maand.onderwijs.tabel")),
                data.visualOutput("bericht.beleid.maand.plot.aantal.onderwijs", plottitle = "Persberichten: Maand per beleid (Onderwijs en Educatie)", Xaxis = "Maand", Xlabels = FALSE)
              ),
              tabBox(
                width = 6,
                title = "Provinciebestuur",
                tabPanel("Plot - aantal", plotOutput("persberichten.beleid.maand.provinciebestuur.plot.aantal")),
                tabPanel("Plot - procent", plotOutput("persberichten.beleid.maand.provinciebestuur.plot.procent")),
                tabPanel("Tabel", DT::dataTableOutput("persberichten.beleid.maand.provinciebestuur.tabel")),
                data.visualOutput("bericht.beleid.maand.plot.aantal.provinciebestuur", plottitle = "Persberichten: Maand per beleid (Provinciebestuur)", Xaxis = "Maand", Xlabels = FALSE)
              ),
              tabBox(
                width = 6,
                title = "Ruimte",
                tabPanel("Plot - aantal", plotOutput("persberichten.beleid.maand.ruimte.plot.aantal")),
                tabPanel("Plot - procent", plotOutput("persberichten.beleid.maand.ruimte.plot.procent")),
                tabPanel("Tabel", DT::dataTableOutput("persberichten.beleid.maand.ruimte.tabel")),
                data.visualOutput("bericht.beleid.maand.plot.aantal.ruimte", plottitle = "Persberichten: Maand per beleid (Ruimte)", Xaxis = "Maand", Xlabels = FALSE)
              ),
              tabBox(
                width = 6,
                title = "Vrije Tijd",
                tabPanel("Plot - aantal", plotOutput("persberichten.beleid.maand.vrijetijd.plot.aantal")),
                tabPanel("Plot - procent", plotOutput("persberichten.beleid.maand.vrijetijd.plot.procent")),
                tabPanel("Tabel", DT::dataTableOutput("persberichten.beleid.maand.vrijetijd.tabel")),
                data.visualOutput("bericht.beleid.maand.plot.aantal.vrijetijd", plottitle = "Persberichten: Maand per beleid (Vrije Tijd)", Xaxis = "Maand", Xlabels = FALSE)
              )
            )
          ),
        # Per Beleid ---------------------------------------------------------
            tabItem(
              tabName = "Persbericht-beleid-beleid",
              fluidRow(
                tabBox(
                  width = 6,
                  title = "Persberichten: Economie",
                  tabPanel("Plot - aantal", plotOutput("persberichten.beleid.beleid.economie.plot.aantal")),
                  tabPanel("Plot - procent", plotOutput("persberichten.beleid.beleid.economie.plot.procent")),
                  tabPanel("Tabel", DT::dataTableOutput("persberichten.beleid.beleid.economie.tabel")),
                  data.visualOutput("bericht.beleid.beleid.plot.aantal.economie", plottitle = "Persberichten: Deelbeleid per beleid (Economie)", Xaxis = "Deelbeleid", Xlabels = FALSE)
                ),
                tabBox(
                  width = 6,
                  title = "Persberichten: Gouverneur",
                  tabPanel("Plot - aantal", plotOutput("persberichten.beleid.beleid.gouverneur.plot.aantal")),
                  tabPanel("Plot - procent", plotOutput("persberichten.beleid.beleid.gouverneur.plot.procent")),
                  tabPanel("Tabel", DT::dataTableOutput("persberichten.beleid.beleid.gouverneur.tabel")),
                  data.visualOutput("bericht.beleid.beleid.plot.aantal.gouverneur", plottitle = "Persberichten: Deelbeleid per beleid (Gouverneur)", Xaxis = "Deelbeleid", Xlabels = FALSE)
                ),
                tabBox(
                  width = 6,
                  title = "Persberichten: Leefmilieu",
                  tabPanel("Plot - aantal", plotOutput("persberichten.beleid.beleid.leefmilieu.plot.aantal")),
                  tabPanel("Plot - procent", plotOutput("persberichten.beleid.beleid.leefmilieu.plot.procent")),
                  tabPanel("Tabel", DT::dataTableOutput("persberichten.beleid.beleid.leefmilieu.tabel")),
                  data.visualOutput("bericht.beleid.beleid.plot.aantal.leefmilieu", plottitle = "Persberichten: per deelbeleid (Leefmilieu)", Xaxis = "Deelbeleid", Xlabels = FALSE)
                ),  
                tabBox(
                  width = 6,
                  title = "Persberichten: Mobiliteit",
                  tabPanel("Plot - aantal", plotOutput("persberichten.beleid.beleid.mobiliteit.plot.aantal")),
                  tabPanel("Plot - procent", plotOutput("persberichten.beleid.beleid.mobiliteit.plot.procent")),
                  tabPanel("Tabel", DT::dataTableOutput("persberichten.beleid.beleid.mobiliteit.tabel")),
                  data.visualOutput("bericht.beleid.beleid.plot.aantal.mobiliteit", plottitle = "Persberichten: per deelbeleid (Mobiliteit)", Xaxis = "Deelbeleid", Xlabels = FALSE)
                ),
                tabBox(
                  width = 6,
                  title = "Persberichten: Onderwijs en Educatie",
                  tabPanel("Plot - aantal", plotOutput("persberichten.beleid.beleid.onderwijs.plot.aantal")),
                  tabPanel("Plot - procent", plotOutput("persberichten.beleid.beleid.onderwijs.plot.procent")),
                  tabPanel("Tabel", DT::dataTableOutput("persberichten.beleid.beleid.onderwijs.tabel")),
                  data.visualOutput("bericht.beleid.beleid.plot.aantal.onderwijs", plottitle = "Persberichten: per deelbeleid (Onderwijs en Educatie)", Xaxis = "Deelbeleid", Xlabels = FALSE)
                ),
                tabBox(
                  width = 6,
                  title = "Persberichten: Provinciebestuur",
                  tabPanel("Plot - aantal", plotOutput("persberichten.beleid.beleid.provinciebestuur.plot.aantal")),
                  tabPanel("Plot - procent", plotOutput("persberichten.beleid.beleid.provinciebestuur.plot.procent")),
                  tabPanel("Tabel", DT::dataTableOutput("persberichten.beleid.beleid.provinciebestuur.tabel")),
                  data.visualOutput("bericht.beleid.beleid.plot.aantal.provinciebestuur", plottitle = "Persberichten: per deelbeleid (Provinciebestuur)", Xaxis = "Deelbeleid", Xlabels = FALSE)
                ),
                tabBox(
                  width = 6,
                  title = "Persberichten: Ruimte",
                  tabPanel("Plot - aantal", plotOutput("persberichten.beleid.beleid.ruimte.plot.aantal")),
                  tabPanel("Plot - procent", plotOutput("persberichten.beleid.beleid.ruimte.plot.procent")),
                  tabPanel("Tabel", DT::dataTableOutput("persberichten.beleid.beleid.ruimte.tabel")),
                  data.visualOutput("bericht.beleid.beleid.plot.aantal.ruimte", plottitle = "Persberichten: per deelbeleid (Ruimte)", Xaxis = "Deelbeleid", Xlabels = FALSE)
                ),
                tabBox(
                  width = 6,
                  title = "Persberichten: Vrije Tijd",
                  tabPanel("Plot - aantal", plotOutput("persberichten.beleid.beleid.vrijetijd.plot.aantal")),
                  tabPanel("Plot - procent", plotOutput("persberichten.beleid.beleid.vrijetijd.plot.procent")),
                  tabPanel("Tabel", DT::dataTableOutput("persberichten.beleid.beleid.vrijetijd.tabel")),
                  data.visualOutput("bericht.beleid.beleid.plot.aantal.vrijetijd", plottitle = "Persberichten: per deelbeleid (Vrije Tijd)", Xaxis = "Deelbeleid", Xlabels = FALSE)
                )
              )
            ),
      # Per Verzender---------------------------------------------------------
        # Algemeen -----------------------------------------------------------
          tabItem(
            tabName = "Persbericht-verzender-algemeen",
            fluidRow(
              tabBox(
                title = "Persberichten per Verzender",
                width = 12,
                tabPanel("Plot - aantal", plotOutput("persberichten.verzender.alg.totaal.plot.aantal")),
                tabPanel("Plot - procent", plotOutput("persberichten.verzender.alg.totaal.plot.procent")),
                tabPanel("Table", DT::dataTableOutput("persberichten.verzender.alg.totaal.tabel")),
                data.visualOutput("bericht.verzender.alg.totaal", plottitle = "Persberichten per Verzender", Xaxis = "Verzender", Xlabels = FALSE)
              ),
              tabBox(
                title = "Beleid per Verzender",
                width = 12,
                tabPanel("Plot - aantal", plotOutput("persberichten.verzender.alg.beleid.plot.aantal")),
                tabPanel("Plot - procent", plotOutput("persberichten.verzender.alg.beleid.plot.procent")),
                tabPanel("Table", DT::dataTableOutput("persberichten.verzender.alg.beleid.tabel")),
                data.visualOutput("bericht.verzender.alg.beleid", plottitle = "Persberichten per Beleid per Verzender", Xaxis = "Verzender", Xlabels = TRUE, Piechart = FALSE)
              )
            )
          ),
        # Per maand ----------------------------------------------------------
          tabItem(
            tabName = "Persbericht-verzender-maand",
            fluidRow(
              tabBox(
                title = "Persdienst",
                width = 6,
                tabPanel("Plot - aantal", plotOutput("persberichten.verzender.maand.persdienst.plot.aantal")),
                tabPanel("Plot - procent", plotOutput("persberichten.verzender.maand.persdienst.plot.procent")),
                tabPanel("Tabel", DT::dataTableOutput("persberichten.verzender.maand.persdienst.tabel")),
                data.visualOutput("bericht.verzender.maand.persdienst", plottitle = "Persberichten per Maand: Persdienst", Xaxis = "Maand", Xlabels = FALSE)
              ), 
              tabBox(
                title = "Provincie",
                width = 6,
                tabPanel("Plot - aantal", plotOutput("persberichten.verzender.maand.provincie.plot.aantal")),
                tabPanel("Plot - procent", plotOutput("persberichten.verzender.maand.provincie.plot.procent")),
                tabPanel("Tabel", DT::dataTableOutput("persberichten.verzender.maand.provincie.tabel")),
                data.visualOutput("bericht.verzender.maand.provincie", plottitle = "Persberichten per Maand: Provincie", Xaxis = "Maand", Xlabels = FALSE)
              ), 
              tabBox(
                title = "Gouverneur",
                width = 6,
                tabPanel("Plot - aantal", plotOutput("persberichten.verzender.maand.gouverneur.plot.aantal")),
                tabPanel("Plot - procent", plotOutput("persberichten.verzender.maand.gouverneur.plot.procent")),
                tabPanel("Tabel", DT::dataTableOutput("persberichten.verzender.maand.gouverneur.tabel")),
                data.visualOutput("bericht.verzender.maand.gouverneur", plottitle = "Persberichten per Maand: Gouvereur", Xaxis = "Maand", Xlabels = FALSE)
              ), 
              tabBox(
                title = "Extern",
                width = 6,
                tabPanel("Plot - aantal", plotOutput("persberichten.verzender.maand.extern.plot.aantal")),
                tabPanel("Plot - procent", plotOutput("persberichten.verzender.maand.extern.plot.procent")),
                tabPanel("Tabel", DT::dataTableOutput("persberichten.verzender.maand.extern.tabel")),
                data.visualOutput("bericht.verzender.maand.extern", plottitle = "Persberichten per Maand: Extern", Xaxis = "Maand", Xlabels = FALSE)
              )
            )
          ),  
      # Per Type -------------------------------------------------------------
        tabItem(
          tabName = "Persbericht-type",
          fluidRow(
            tabBox(
              title = "Persberichten per Soort",
              width = 12,
              tabPanel("Plot - aantal", plotOutput("persberichten.type.plot.aantal")),
              tabPanel("Plot - procent", plotOutput("persberichten.type.plot.procent")),
              tabPanel("Tabel", DT::dataTableOutput("persberichten.type.tabel")),
              data.visualOutput("bericht.type", plottitle = "Persberichten per Type", Xaxis = "Beleid", Xlabels = TRUE)
            )
          )
        ),
      
    # Persreturn -------------------------------------------------------------
      # Per beleid -----------------------------------------------------------
        tabItem(
          tabName = "Return_Beleid",
          fluidRow(
        # Algemeen ---------------------------------------------------------
          tabBox(
            width = 12,
            title = "Persreturn per beleid",
            tabPanel("Plot - aantal", plotOutput("persreturn.beleid.alg.plot.aantal")),
            tabPanel("Plot - procent", plotOutput("persreturn.beleid.alg.plot.procent")),
            tabPanel("Tabel", DT::dataTableOutput("persreturn.beleid.alg.tabel")),
            data.visualOutput("return.beleid.alg", plottitle = "Persreturn per Beleid", Xaxis = "Beleid", Xlabels = TRUE, Piechart = FALSE)
          ),
        # Deelbeleid -------------------------------------------------------
          tabBox(
            width = 6,
            title = "Persreturn: Economie",
            tabPanel("Plot - aantal", plotOutput("persreturn.beleid.beleid.economie.plot.aantal")),
            tabPanel("Plot - procent", plotOutput("persreturn.beleid.beleid.economie.plot.procent")),
            tabPanel("Tabel", DT::dataTableOutput("persreturn.beleid.beleid.economie.tabel")),
            data.visualOutput("return.beleid.beleid.economie", plottitle = "Persreturn per Beleid: Economie", Xaxis = "Deelbeleid", Xlabels = TRUE, Piechart = FALSE)
          ),
          tabBox(
            width = 6,
            title = "Persreturn: Gouverneur",
            tabPanel("Plot - aantal", plotOutput("persreturn.beleid.beleid.gouverneur.plot.aantal")),
            tabPanel("Plot - procent", plotOutput("persreturn.beleid.beleid.gouverneur.plot.procent")),
            tabPanel("Tabel", DT::dataTableOutput("persreturn.beleid.beleid.gouverneur.tabel")),
            data.visualOutput("return.beleid.beleid.gouverneur", plottitle = "Persreturn per Beleid: Gouverneur", Xaxis = "Deelbeleid", Xlabels = TRUE, Piechart = FALSE)
          ),
          tabBox(
            width = 6,
            title = "Persreturn: Leefmilieu",
            tabPanel("Plot - aantal", plotOutput("persreturn.beleid.beleid.leefmilieu.plot.aantal")),
            tabPanel("Plot - procent", plotOutput("persreturn.beleid.beleid.leefmilieu.plot.procent")),
            tabPanel("Tabel", DT::dataTableOutput("persreturn.beleid.beleid.leefmilieu.tabel")),
            data.visualOutput("return.beleid.beleid.leefmilieu", plottitle = "Persreturn per Beleid: Leefmilieu", Xaxis = "Deelbeleid", Xlabels = TRUE, Piechart = FALSE)
          ),
          tabBox(
            width = 6,
            title = "Persreturn: Mobiliteit",
            tabPanel("Plot - aantal", plotOutput("persreturn.beleid.beleid.mobiliteit.plot.aantal")),
            tabPanel("Plot - procent", plotOutput("persreturn.beleid.beleid.mobiliteit.plot.procent")),
            tabPanel("Tabel", DT::dataTableOutput("persreturn.beleid.beleid.mobiliteit.tabel")),
            data.visualOutput("return.beleid.beleid.mobiliteit", plottitle = "Persreturn per Beleid: Mobiliteit", Xaxis = "Deelbeleid", Xlabels = TRUE, Piechart = FALSE)
          ),
          tabBox(
            width = 6,
            title = "Persreturn: Onderwijs en Educatie",
            tabPanel("Plot - aantal", plotOutput("persreturn.beleid.beleid.onderwijs.plot.aantal")),
            tabPanel("Plot - procent", plotOutput("persreturn.beleid.beleid.onderwijs.plot.procent")),
            tabPanel("Tabel", DT::dataTableOutput("persreturn.beleid.beleid.onderwijs.tabel")),
            data.visualOutput("return.beleid.beleid.onderwijs", plottitle = "Persreturn per Beleid: Onderwijs en Educatie", Xaxis = "Deelbeleid", Xlabels = TRUE, Piechart = FALSE)
          ),
          tabBox(
            width = 6,
            title = "Persreturn: Provinciebestuur",
            tabPanel("Plot - aantal", plotOutput("persreturn.beleid.beleid.provinciebestuur.plot.aantal")),
            tabPanel("Plot - procent", plotOutput("persreturn.beleid.beleid.provinciebestuur.plot.procent")),
            tabPanel("Tabel", DT::dataTableOutput("persreturn.beleid.beleid.provinciebestuur.tabel")),
            data.visualOutput("return.beleid.beleid.provinciebestuur", plottitle = "Persreturn per Beleid: Provinciebestuur", Xaxis = "Deelbeleid", Xlabels = TRUE, Piechart = FALSE)
          ),
          tabBox(
            width = 6,
            title = "Persreturn: Ruimte",
            tabPanel("Plot - aantal", plotOutput("persreturn.beleid.beleid.ruimte.plot.aantal")),
            tabPanel("Plot - procent", plotOutput("persreturn.beleid.beleid.ruimte.plot.procent")),
            tabPanel("Tabel", DT::dataTableOutput("persreturn.beleid.beleid.ruimte.tabel")),
            data.visualOutput("return.beleid.beleid.ruimte", plottitle = "Persreturn per Beleid: Ruimte", Xaxis = "Deelbeleid", Xlabels = TRUE, Piechart = FALSE)
          ),
          tabBox(
            width = 6,
            title = "Persreturn: Vrije Tijd",
            tabPanel("Plot - aantal", plotOutput("persreturn.beleid.beleid.vrijetijd.plot.aantal")),
            tabPanel("Plot - procent", plotOutput("persreturn.beleid.beleid.vrijetijd.plot.procent")),
            tabPanel("Tabel", DT::dataTableOutput("persreturn.beleid.beleid.vrijetijd.tabel")),
            data.visualOutput("return.beleid.beleid.vrijetijd", plottitle = "Persreturn per Beleid: Vrije Tijd", Xaxis = "Deelbeleid", Xlabels = TRUE, Piechart = FALSE)
          )
        )
      ),
      # Per Platform ---------------------------------------------------------
      tabItem(
        tabName = "Return_Medium",
        fluidRow(
          tabBox(
            title = "Persreturn per medium",
            width = 12,
            tabPanel("Plot - aantal", plotOutput("persreturn.medium.plot.aantal")),
            tabPanel("Plot - procent", plotOutput("persreturn.medium.plot.procent")),
            tabPanel("Tabel", DT::dataTableOutput("persreturn.medium.tabel")),
            data.visualOutput("return.medium", plottitle = "Persreturn (totaal) per Medium", Xaxis = "Beleid", Xlabels = TRUE, Piechart = FALSE)
          )
        )
      ),
    # Download ---------------------------------------------------------------
      tabItem(
        tabName = "Download",
        fluidRow(
        # Download button ------------------------------------------------------
          box(
            width = 12,
            title = "Download",
            downloadButton("report", "Generate report")
            
          )
        )
      )
    )
  )
)


