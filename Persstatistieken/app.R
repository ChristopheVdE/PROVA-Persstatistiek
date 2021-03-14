# Loading packages
library(shiny)

# Persberichten algemeen -------------------------------------------------------
source("./Modules/JaarOverzicht/DataVisual/Persberichten/Algemeen/Persberichten/DataVisual_PbBerichtAlgKwartaal.R")
source("./Modules/JaarOverzicht/DataVisual/Persberichten/Algemeen/Persberichten/DataVisual_PbBerichtAlgMaand.R")
source("./Modules/JaarOverzicht/DataVisual/Persberichten/Algemeen/Persberichten/DataVisual_PbBerichtAlgdag.R")
source("./Modules/JaarOverzicht/DataVisual/Persberichten/Algemeen/Persberichten/DataVisual_PbBerichtAlgWeek.R")
source("./Modules/JaarOverzicht/DataVisual/Persberichten/Algemeen/Persberichten/DataVisual_PbBerichtAlgBeleid.R")
# Persconferenties algemeen ----------------------------------------------------
source("./Modules/JaarOverzicht/DataVisual/Persberichten/Algemeen/Persconferenties/DataVisual_PbConferentieAlgKwartaal.R")
source("./Modules/JaarOverzicht/DataVisual/Persberichten/Algemeen/Persconferenties/DataVisual_PbConferentieAlgMaand.R")
source("./Modules/JaarOverzicht/DataVisual/Persberichten/Algemeen/Persconferenties/DataVisual_PbConferentieAlgdag.R")
source("./Modules/JaarOverzicht/DataVisual/Persberichten/Algemeen/Persconferenties/DataVisual_PbConferentieAlgWeek.R")
source("./Modules/JaarOverzicht/DataVisual/Persberichten/Algemeen/Persconferenties/DataVisual_PbConferentieAlgBeleid.R")
# PErsberichten per beleid------------------------------------------------------
source("./Modules/JaarOverzicht/DataVisual/Persberichten/PerBeleid/DataVisual_PbBeleidMaand.R")
source("./Modules/JaarOverzicht/DataVisual/Persberichten/PerBeleid/DataVisual_PbBeleidDeelbeleid.R")
# Persberichten per verzender --------------------------------------------------
source("./Modules/JaarOverzicht/DataVisual/Persberichten/PerVerzender/DataVisual_PbVerzenderVerzender.R")
source("./Modules/JaarOverzicht/DataVisual/Persberichten/PerVerzender/DataVisual_PbVerzenderBeleid.R")
source("./Modules/JaarOverzicht/DataVisual/Persberichten/PerVerzender/DataVisual_PbVerzenderMaand.R")
source("./Modules/JaarOverzicht/DataVisual/Persberichten/PerVerzender/DataVisual_PbVerzenderDeelbeleid.R")
# Persberichten per type -------------------------------------------------------
source("./Modules/JaarOverzicht/DataVisual/Persberichten/PerType/DataVisual_PbType.R")
# Persreturn per beleid --------------------------------------------------------
source("./Modules/JaarOverzicht/DataVisual/Persreturn/Beleid/DataVisual_PrBeleidAlg.R")
source("./Modules/JaarOverzicht/DataVisual/Persreturn/Beleid/DataVisual_PrBeleidDeelbeleid.R")


## Only run examples in interactive R sessions
if (interactive()) {
  
  # basic example
  shinyApp(
    ui = fluidPage(
      selectInput("variable", "Variable:",
                  c("Cylinders" = "cyl",
                    "Transmission" = "am",
                    "Gears" = "gear")),
      tableOutput("data")
    ),
    server = function(input, output) {
      output$data <- renderTable({
        mtcars[, c("mpg", input$variable), drop = FALSE]
      }, rownames = TRUE)
    }
  )
  
  # demoing group support in the `choices` arg
  shinyApp(
    ui = fluidPage(
      selectInput("state", "Choose a state:",
                  list(`East Coast` = list("NY", "NJ", "CT"),
                       `West Coast` = list("WA", "OR", "CA"),
                       `Midwest` = list("MN", "WI", "IA"))
      ),
      textOutput("result")
    ),
    server = function(input, output) {
      output$result <- renderText({
        paste("You chose", input$state)
      })
    }
  )
}
  )
}