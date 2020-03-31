###############################################################################
# MODULE: Persberichten - Algemeen: Beleidsplot
###############################################################################

library(shiny)
library(ggplot2)
library(RColorBrewer)

# UI ==========================================================================
bericht.alg.beleid.plotOutput <- function(id, plottitle) {
  ns <- NS(id)
  tabPanel(
    "Opties",
    fluidRow(
      column(
        width = 6,
        textInput(ns("title"), label = "Plot title", value = plottitle, placeholder = "Plot titel"),
        textInput(ns("X-axis"), label = "X-as naam", value = "Beleid", placeholder = "Beleid"),
        textInput(ns("Y-axis"), label = "Y-as naam", value = "Aantal", placeholder = "Aantal")
      ),
      column(
        width = 6,
        selectInput(ns("plottype"), label = "Plot type", choices = c("Barplot", "Taartdiagram"), selected = "Barplot"),
        selectInput(ns("inhoud"), label = "Plot type", choices = c("Nummers", "Procentueel"), selected = "Nummers"),
        checkboxInput(ns("X-labels"), label = "As labels (X-as)", value = TRUE),
        checkboxInput(ns("legend"), label = "Legende", value = TRUE)
      )
    )
  )
}

# SERVER ======================================================================
bericht.alg.beleid.plot <- function(input, output, session, data) {
  
  # Preparation ------------------------------------------------------
  df.bericht.beleid <- reactive({
    # Create dataframe for barplot --------------------------------------------
    bericht.beleid <- data.frame(table(data()$Beleid))
    
    # Rename columns ----------------------------------------------------------
    colnames(bericht.beleid) <- c("Beleid","Persberichten")
    return(bericht.beleid)
  })
  
  # Define color pallete --------------------------------------
  colors <- c(brewer.pal(8,"Pastel2"), brewer.pal(9, "Pastel1"))
  
  # Barplot (Per beleid) --------------------------------------------
  berichten.barplot.maand <- reactive( 
    ggplot(data=df.bericht.beleid(), aes(x=Beleid, y=Persberichten, fill=Beleid)) +
    geom_bar(position = "dodge", stat='identity') +
    xlab("Beleid") +
    ylab("Aantal") +
    ggtitle("Persberichten per beleid") +
    geom_text(aes(label=Persberichten),
              position=position_dodge(0.9), vjust=0) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_fill_manual(values=colors)
  )
    
  return(reactive(berichten.barplot.maand()))
  
}