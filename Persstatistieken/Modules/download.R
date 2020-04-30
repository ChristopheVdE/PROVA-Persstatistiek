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
    tabBox(
      title = "Persberichten",
      width = 12,
    # Persberichten: Algemeen -------------------------------------------------
      tabPanel(
        title = "Algemeen",
        fluidRow(
          column(
            width = 6, 
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
            width = 6,
            textInput("Algemeen.title", label = "Sectietitel: Persberichten - algemeen", value = "Algemeen provincie", placeholder = "Algemeen provincie", width = '100%'),
            tags$hr(),
            textInput("Algemeen.kwartaal", label = "Subtitel: Algemeen - Kwartaal", value = "Persberichten per kwartaal", placeholder = "Persberichten per kwartaal", width = '100%'),
            textInput("Algemeen.maand", label = "Subtitel: Algemeen - Maand", value = "Persberichten per maand", placeholder = "Persberichten per maand", width = '100%'),
            textInput("Algemeen.beleid", label = "Subtitel: Algemeen - Beleid", value = "Persberichten per beleid", placeholder = "Persberichten per beleid", width = '100%')
          )
        )
      ),
    # Persberichten: Beleid ---------------------------------------------------
      tabPanel("Beleid"),
    # Persberichten: Verzender ------------------------------------------------
      tabPanel("Verzender"),
    # Persberichten: Type -----------------------------------------------------
      tabPanel("Type")
    )
}
# SERVER ======================================================================
Rapport <- function(input, output, session) {
  
}