###############################################################################
# MODULE: Download: manage Rapport volgorde
###############################################################################

# LOAD PACKAGES ===============================================================
library(shiny)
library(sortable)
# =============================================================================

# UI ==========================================================================
RapportOutput <- function(id) {
  ns <- NS(id)
  # Persberichten -------------------------------------------------------------
    tabBox(
      title = "Persberichten",
      width = 12,
    # Persberichten: Algemeen -------------------------------------------------
      tabPanel(
        title = "Algemeen",
        fluidRow(
          column(
            width = 4, 
            bucket_list(
              header = NULL,
              group_name = "Persberichten.algemeen",
              orientation = "vertical",
              add_rank_list(
                input_id = "persberichten.alg",
                text = "Toevoegen aan rapport in onderstaande volgorde",
                labels = list(
                  "Kwartaal",
                  "Maand",
                  "Beleid"
                )
              ),
              add_rank_list(
                input_id = NULL,
                text = "Weglaten uit rapport",
                labels = NULL
              )
            ),
          ),
          column(
            width = 8,
            textInput("Algemeen.title", label = "Sectietitel: Persberichten - algemeen", value = "Algemeen provincie", placeholder = "Algemeen provincie", width = '100%'),
            tags$hr(),
            textInput("Algemeen.kwartaal", label = "Subtitel: Algemeen - Kwartaal", value = "Persberichten per kwartaal", placeholder = "Persberichten per kwartaal", width = '100%'),
            textInput("Algemeen.maand", label = "Subtitel: Algemeen - Maand", value = "Persberichten per maand", placeholder = "Persberichten per maand", width = '100%'),
            textInput("Algemeen.beleid", label = "Subtitel: Algemeen - Beleid", value = "Persberichten per beleid", placeholder = "Persberichten per beleid", width = '100%')
          )
        )
      ),
    # Persberichten: Beleid ---------------------------------------------------
      tabPanel(
        title = "Beleid",
        fluidRow(
          column(
            width = 4,
            bucket_list(
              header = NULL,
              group_name = "Persberichten.beleid",
              orientation = "vertical",
              add_rank_list(
                input_id = "persberichten.beleid",
                text = "Toevoegen aan rapport in onderstaande volgorde",
                labels = list(
                  "Provinciebestuur",
                  "Economie",
                  "Leefmilieu",
                  "Mobiliteit",
                  "Onderwijs en Educatie",
                  "Ruimte",
                  "Vrije Tijd",
                  "Gouverneur"
                )
              ),
              add_rank_list(
                input_id = NULL,
                text = "Weglaten uit rapport",
                labels = NULL
              )
            ),
          ),
          column(
            width = 8,
            tags$br(),
            textInput("Algemeen.title", label = "Sectietitel: Persberichten - beleid", value = "Beleid en deelbeleid", placeholder = "Beleid en deelbeleid", width = '100%'),
            tags$hr(),
            tags$br(),
            tabBox(
              title = "Extra opties",
              width = 12,
      # Provinciebestuur ------------------------------------------------
              tabPanel(
                title = "Provinciebestuur",
                fluidRow(
                  column(
                    width = 5,
                    bucket_list(
                      header = NULL,
                      group_name = "Persberichten.provinciebestuur",
                      orientation = "vertical",
                      add_rank_list(
                        input_id = "persberichten.provincebestuur",
                        text = "Toevoegen aan rapport in onderstaande volgorde",
                        labels = list(
                          "Per maand",
                          "Per deelbeleid"
                        )
                      ),
                      add_rank_list(
                        input_id = NULL,
                        text = "Weglaten uit rapport",
                        labels = NULL
                      )
                    )
                  ),
                  column(
                    width = 7,
                    tags$br(),
                    textInput("Provinciebestuur.titel", label = "Sectietitel: Beleid - Provinciebestuur", value = "Provinciebestuur", placeholder = "Provinciebestuur", width = '100%'),
                    tags$hr(),
                    textInput("Provinciebestuur.maand.titel", label = "Subtitel: Provinciebestuur - Maand", value = "Per maand", placeholder = "Per maand", width = '100%'),
                    textInput("Provinciebestuur.deelbeleid.titel", label = "Subtitel: Provinciebestuur - Deelbeleid", value = "Per thema", placeholder = "Per thema", width = '100%')
                  )
                )
              ),
      # Economie --------------------------------------------------------
              tabPanel(
                title = "Economie",
                fluidRow(
                  column(
                    width = 5,
                    bucket_list(
                      header = NULL,
                      group_name = "Persberichten.economie",
                      orientation = "vertical",
                      add_rank_list(
                        input_id = "persberichten.economie",
                        text = "Toevoegen aan rapport in onderstaande volgorde",
                        labels = list(
                          "Per maand",
                          "Per deelbeleid"
                        )
                      ),
                      add_rank_list(
                        input_id = NULL,
                        text = "Weglaten uit rapport",
                        labels = NULL
                      )
                    )
                  ),
                  column(
                    width = 7,
                    tags$br(),
                    textInput("Economie.titel", label = "Sectietitel: Beleid - Economie", value = "Economie", placeholder = "Economie", width = '100%'),
                    tags$hr(),
                    textInput("Economie.maand.titel", label = "Subtitel: Economie - Maand", value = "Per maand", placeholder = "Per maand", width = '100%'),
                    textInput("Economie.deelbeleid.titel", label = "Subtitel: Economie - Deelbeleid", value = "Per thema", placeholder = "Per thema", width = '100%')
                  )
                )
              ),
      # Leefmilieu ------------------------------------------------------
              tabPanel(
                title = "Leefmilieu",
                fluidRow(
                  column(
                    width = 5,
                    bucket_list(
                      header = NULL,
                      group_name = "Persberichten.leefmilieu",
                      orientation = "vertical",
                      add_rank_list(
                        input_id = "persberichten.leefmilieu",
                        text = "Toevoegen aan rapport in onderstaande volgorde",
                        labels = list(
                          "Per maand",
                          "Per deelbeleid"
                        )
                      ),
                      add_rank_list(
                        input_id = NULL,
                        text = "Weglaten uit rapport",
                        labels = NULL
                      )
                    )
                  ),
                  column(
                    width = 7,
                    tags$br(),
                    textInput("Leefmilieu.titel", label = "Sectietitel: Beleid - Leefmilieu", value = "Leefmilieu", placeholder = "Leefmilieu", width = '100%'),
                    tags$hr(),
                    textInput("Leefmilieu.maand.titel", label = "Subtitel: Leefmilieu - Maand", value = "Per maand", placeholder = "Per maand", width = '100%'),
                    textInput("Leefmilieu.deelbeleid.titel", label = "Subtitel: Leefmilieu - Deelbeleid", value = "Per thema", placeholder = "Per thema", width = '100%')
                  )
                )
              ),
      # Mobiliteit ------------------------------------------------------
              tabPanel(
                title = "Mobiliteit",
                fluidRow(
                  column(
                    width = 5,
                    bucket_list(
                      header = NULL,
                      group_name = "Persberichten.mobiliteit",
                      orientation = "vertical",
                      add_rank_list(
                        input_id = "persberichten.mobiliteit",
                        text = "Toevoegen aan rapport in onderstaande volgorde",
                        labels = list(
                          "Per maand",
                          "Per deelbeleid"
                        )
                      ),
                      add_rank_list(
                        input_id = NULL,
                        text = "Weglaten uit rapport",
                        labels = NULL
                      )
                    )
                  ),
                  column(
                    width = 7,
                    tags$br(),
                    textInput("Mobiliteit.titel", label = "Sectietitel: Beleid - Mobiliteit", value = "Mobilitieit", placeholder = "Mobiliteit", width = '100%'),
                    tags$hr(),
                    textInput("Mobiliteit.maand.titel", label = "Subtitel: Mobiliteit - Maand", value = "Per maand", placeholder = "Per maand", width = '100%'),
                    textInput("Mobiliteit.deelbeleid.titel", label = "Subtitel: Mobiliteit - Deelbeleid", value = "Per thema", placeholder = "Per thema", width = '100%')
                  )
                )
              ),
      # Ruimte ----------------------------------------------------------
              tabPanel(
                title = "Ruimte",
                fluidRow(
                  column(
                    width = 5,
                    bucket_list(
                      header = NULL,
                      group_name = "Persberichten.ruimte",
                      orientation = "vertical",
                      add_rank_list(
                        input_id = "persberichten.ruimte",
                        text = "Toevoegen aan rapport in onderstaande volgorde",
                        labels = list(
                          "Per maand",
                          "Per deelbeleid"
                        )
                      ),
                      add_rank_list(
                        input_id = NULL,
                        text = "Weglaten uit rapport",
                        labels = NULL
                      )
                    )
                  ),
                  column(
                    width = 7,
                    tags$br(),
                    textInput("Ruimte.titel", label = "Sectietitel: Beleid - Ruimte", value = "Ruimte", placeholder = "Ruimte", width = '100%'),
                    tags$hr(),
                    textInput("Ruimte.maand.titel", label = "Subtitel: Ruimte - Maand", value = "Per maand", placeholder = "Per maand", width = '100%'),
                    textInput("Ruimte.deelbeleid.titel", label = "Subtitel: Ruimte - Deelbeleid", value = "Per thema", placeholder = "Per thema", width = '100%')
                  )
                )
              ),
      # Onderwijs en Educatie -------------------------------------------
              tabPanel(
                title = "Onderwijs",
                fluidRow(
                  column(
                    width = 5,
                    bucket_list(
                      header = NULL,
                      group_name = "Persberichten.onderwijs",
                      orientation = "vertical",
                      add_rank_list(
                        input_id = "persberichten.onderwijs",
                        text = "Toevoegen aan rapport in onderstaande volgorde",
                        labels = list(
                          "Per maand",
                          "Per deelbeleid"
                        )
                      ),
                      add_rank_list(
                        input_id = NULL,
                        text = "Weglaten uit rapport",
                        labels = NULL
                      )
                    )
                  ),
                  column(
                    width = 7,
                    tags$br(),
                    textInput("Onderwijs.titel", label = "Sectietitel: Beleid - Onderijs en Educatie", value = "Onderwijs en Educatie", placeholder = "Onderwijs en Educatie", width = '100%'),
                    tags$hr(),
                    textInput("Onderwijs.maand.titel", label = "Subtitel: Onderwijs en Educatie - Maand", value = "Per maand", placeholder = "Per maand", width = '100%'),
                    textInput("Onderwijs.deelbeleid.titel", label = "Subtitel: Onderwijs en Educatie - Deelbeleid", value = "Per thema", placeholder = "Per thema", width = '100%')
                  )
                )
              ),
      # Vrije Tijd ------------------------------------------------------
              tabPanel(
                title = "Vrije tijd",
                fluidRow(
                  column(
                    width = 5,
                    bucket_list(
                      header = NULL,
                      group_name = "Persberichten.vrijetijd",
                      orientation = "vertical",
                      add_rank_list(
                        input_id = "persberichten.vrijetijd",
                        text = "Toevoegen aan rapport in onderstaande volgorde",
                        labels = list(
                          "Per maand",
                          "Per deelbeleid"
                        )
                      ),
                      add_rank_list(
                        input_id = NULL,
                        text = "Weglaten uit rapport",
                        labels = NULL
                      )
                    )
                  ),
                  column(
                    width = 7,
                    tags$br(),
                    textInput("Vrijetijd.titel", label = "Sectietitel: Beleid - Vrije tijd", value = "Vrije tijd", placeholder = "Vrije tijd", width = '100%'),
                    tags$hr(),
                    textInput("Vrijetijd.maand.titel", label = "Subtitel: Vrije tijd - Maand", value = "Per maand", placeholder = "Per maand", width = '100%'),
                    textInput("Vrijetijd.deelbeleid.titel", label = "Subtitel: Vrije tijd - Deelbeleid", value = "Per thema", placeholder = "Per thema", width = '100%')
                  )
                )
              ),
      # Gouverneur ------------------------------------------------------
              tabPanel(
                title = "Gouverneur",
                fluidRow(
                  column(
                    width = 5,
                    bucket_list(
                      header = NULL,
                      group_name = "Persberichten.gouverneur",
                      orientation = "vertical",
                      add_rank_list(
                        input_id = "persberichten.gouverneur",
                        text = "Toevoegen aan rapport in onderstaande volgorde",
                        labels = list(
                          "Per maand",
                          "Per deelbeleid"
                        )
                      ),
                      add_rank_list(
                        input_id = NULL,
                        text = "Weglaten uit rapport",
                        labels = NULL
                      )
                    )
                  ),
                  column(
                    width = 7,
                    tags$br(),
                    textInput("gouverneur.titel", label = "Sectietitel: Beleid - Gouverneur", value = "Gouverneur", placeholder = "Gouverneur", width = '100%'),
                    tags$hr(),
                    textInput("Gouverneur.maand.titel", label = "Subtitel: Gouverneur - Maand", value = "Per maand", placeholder = "Per maand", width = '100%'),
                    textInput("Gouverneur.deelbeleid.titel", label = "Subtitel: Gouverneur - Deelbeleid", value = "Per thema", placeholder = "Per thema", width = '100%')
                  )
                )
              )
            )
          )
        )
      ),
    # Persberichten: Verzender ------------------------------------------------
      tabPanel(
        title = "Verzender",
        fluidRow(
          column(
            width = 4,
            bucket_list(
              header = NULL, 
              group_name = "Persberichten.verzender",
              orientation = "vertical",
              add_rank_list(
                input_id = "persberichten.verzender",
                text = "Toevoegen aan rapport in onderstaande volgorde",
                labels = list(
                  "Algemeen",
                  "Per maand"
                )
              ),
              add_rank_list(
                input_id = NULL,
                text = "Weglaten uit rapport",
                labels = NULL
              )
            )
          ),
          column(
            width = 8,
            tags$br(),
            textInput("Algemeen.title", label = "Sectietitel: Persberichten - verzender", value = "Per verzender", placeholder = "Per verzender", width = '100%'),
            tags$hr(),
            tags$br(),
            tabBox(
              title = "Extra opties",
              width = 12,
      # Algemeen ------------------------------------------------------------
              tabPanel(
                title = "Algemeen",
                textInput("Verzender.alg.titel", label = "Sectietitel: Verzender - Algemeen", value = "Algemeen", placeholder = "Algemeen", width = '100%'),
              ),
      # Per maand -----------------------------------------------------------
              tabPanel(
                title = "Per maand",
                fluidRow(
                  column(
                    width = 5,
                    bucket_list(
                      header = NULL,
                      group_name = "Verzender.maand",
                      orientation = "vertical",
                      add_rank_list(
                        input_id = "verzender.maand",
                        text = "Toevoegen aan rapport in onderstaande volgorde",
                        labels = list(
                          "Persdienst",
                          "Provincie",
                          "Gouverneur",
                          "Extern"
                        )
                      ),
                      add_rank_list(
                        input_id = NULL,
                        text = "Weglaten uit rapport",
                        labels = NULL
                      )
                    )
                  ),
                  column(
                    width = 7,
                    tags$br(),
                    textInput("verzender.maand.titel", label = "Sectietitel: Verzender - Maand", value = "Per Maand", placeholder = "Per Maand", width = '100%'),
                    tags$hr(),
                    textInput("Verzender.Persdienst", label = "Subtitel: Maand - Persdienst", value = "Verzonden door: Persdienst", placeholder = "Verzonden door: Persdienst", width = '100%'),
                    textInput("Verzender.Provincie", label = "Subtitel: Maand - Provincie", value = "Verzonden door: Provincie", placeholder = "Verzonden door: Provincie", width = '100%'),
                    textInput("Verzender.Gouverneur", label = "Subtitel: Maand - Gouverneur", value = "Verzonden door: Gouverneur", placeholder = "Verzonden door: Gouverneur", width = '100%'),
                    textInput("Verzender.Extern", label = "Subtitel: Maand - Extern", value = "Verzonden door: Extern", placeholder = "Verzonden door: Extern", width = '100%')
                  )
                )
              )
            )
          )
        )
      ),
    # Persberichten: Type -----------------------------------------------------
      tabPanel(
        title = "Type",
        fluidRow(
          column(
            width = 6, 
            bucket_list(
              header = NULL,
              group_name = "Persberichten.type",
              orientation = "horizontal",
              add_rank_list(
                input_id = "persberichten.type",
                text = "Toevoegen aan rapport in onderstaande volgorde",
                labels = list(
                  "Type"
                )
              ),
              add_rank_list(
                input_id = NULL,
                text = "Weglaten uit rapport",
                labels = NULL
              )
            ),
          ),
          column(
            width = 6,
            tags$br(),
            textInput("Type.title", label = "Sectietitel: Persberichten - type", value = "Type persberichten", placeholder = "Type persberichten", width = '100%'),
          )
        )
      )
    )
}
# SERVER ======================================================================
Rapport <- function(input, output, session) {
  
}