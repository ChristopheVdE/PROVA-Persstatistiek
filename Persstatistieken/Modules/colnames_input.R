library(shiny)

# UI function -------------------------------------------------
choose.colnamesOutput <- function(id, width = 6) {
  # Create namespace function using the provided id
  ns <- NS(id)
  box(
    title = "Selecteer kolomen die overenkomen met kolominhoud (indien geen kolomnnamen in Excel)",
    width = 7,
    column(
      width = 3,
      tags$br(tags$b("Kwartaal")),
      tags$br(tags$b("Beleid")),
      tags$br(tags$b("Detail beleid")),
      tags$br(tags$b("Verzender")),
      tags$br(tags$b("Type Persbericht"))
    ),
    column(
      width = 3,
      selectInput("col.beleid", label = NULL, c("?", "colom 1", "colom 2", "colom 3", "colom 4", "colom 5", "colom 6", "colom 7", "colom 8", "colom 9", "colom 10")),
      selectInput("col.detail", label = NULL, c("?", "colom 1", "colom 2", "colom 3", "colom 4", "colom 5", "colom 6", "colom 7", "colom 8", "colom 9", "colom 10")),
      selectInput("col.kwartaal", label = NULL, c("?", "colom 1", "colom 2", "colom 3", "colom 4", "colom 5", "colom 6", "colom 7", "colom 8", "colom 9", "colom 10")),
      selectInput("col.Verzender", label = NULL, c("?", "colom 1", "colom 2", "colom 3", "colom 4", "colom 5", "colom 6", "colom 7", "colom 8", "colom 9", "colom 10")),
      selectInput("col.type", label = NULL, c("?", "colom 1", "colom 2", "colom 3", "colom 4", "colom 5", "colom 6", "colom 7", "colom 8", "colom 9", "colom 10")),
    ),
    column(
      width = 3,
      tags$br(tags$b("Persreturn: Algemeen")),
      tags$br(tags$b("Persreturn: TV")),
      tags$br(tags$b("Persreturn: Web")),
      tags$br(tags$b("Maand")),
    ),
    column(
      width = 3,
      selectInput("col.return.algemeen", label = NULL, c("?", "colom 1", "colom 2", "colom 3", "colom 4", "colom 5", "colom 6", "colom 7", "colom 8", "colom 9", "colom 10")),
      selectInput("col.return.web", label = NULL, c("?", "colom 1", "colom 2", "colom 3", "colom 4", "colom 5", "colom 6", "colom 7", "colom 8", "colom 9", "colom 10")),
      selectInput("col.return.tv", label = NULL, c("?", "colom 1", "colom 2", "colom 3", "colom 4", "colom 5", "colom 6", "colom 7", "colom 8", "colom 9", "colom 10")),
      selectInput("col.maand", label = NULL, c("?", "colom 1", "colom 2", "colom 3", "colom 4", "colom 5", "colom 6", "colom 7", "colom 8", "colom 9", "colom 10"))
    )
  )
}

# SERVER function ---------------------------------------------
choose.colnames <- function(input, output, session, file) {
  # Number of columns in file
  output$columns <- c("test", "test2", "test3")

    # reactive({
    # columns <- "?"
    # for (i in 1:ncol(file())) {
    #   columns <- c(columns, paste("column: ", i))
    # }
    # c("test", "test2", "test3")
  # })
  
  
}