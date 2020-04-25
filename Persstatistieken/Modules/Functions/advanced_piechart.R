###############################################################################
# Advanced piecharts (2 layers)
###############################################################################

library(webr)
library(ggplot2)

advanced.pie <- function(Id, data) {
  plot <- reactive(
    if(Id == "verzender.alg.beleid") {
        PieDonut(data(), 
                 aes(pies = Verzender , donuts=Beleid, count = Persberichten),
                 labelposition = 1,
                 explodeDonut = TRUE,
                 title = "Persberichten per verzender per beleid"
                 )
    }
  )
  return(plot())
}