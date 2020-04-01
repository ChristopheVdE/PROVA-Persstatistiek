###############################################################################
# MODULE: Persberichten - Algemeen: Beleidsplot
###############################################################################

library(shiny)
library(ggplot2)
library(RColorBrewer)
library(scales)

# UI ==========================================================================
bericht.alg.beleid.plotOutput <- function(id, plottitle) {
  ns <- NS(id)
  tabPanel(
    "Opties",
    fluidRow(
      column(
        width = 6,
        textInput(ns("title"), label = "Plot title", value = plottitle, placeholder = "Plot titel"),
        textInput(ns("Xaxis"), label = "X-as naam", value = "Beleid", placeholder = "Beleid"),
        textInput(ns("Yaxis"), label = "Y-as naam", value = "Aantal", placeholder = "Aantal")
      ),
      column(
        width = 6,
        selectInput(ns("type"), label = "Plot type", choices = c("Barplot", "Taartdiagram"), selected = "Barplot"),
        selectInput(ns("inhoud"), label = "Plot type", choices = c("Aantal", "Procentueel"), selected = "Aantal"),
        checkboxInput(ns("Xlabels"), label = "As labels (X-as)", value = TRUE),
        checkboxInput(ns("legend"), label = "Legende", value = TRUE)
      )
    )
  )
}

# SERVER ======================================================================
bericht.alg.beleid.plot <- function(input, output, session, data) {
  
  # Preparation ---------------------------------------------------------------
  df.bericht.beleid <- reactive({
    # Create dataframe for barplot --------------------------------------------
    bericht.beleid <- data.frame(table(data()$Beleid))
    
    # Rename columns ----------------------------------------------------------
    colnames(bericht.beleid) <- c("Beleid","Persberichten")
    
    # Calculate percentages ---------------------------------------------------
    if (input$inhoud == "Procentueel") {
      total <- sum(bericht.beleid$Persberichten)
      for (i in 0:length(bericht.beleid$Persberichten)) {
        if (i == 0) {
          column <- NULL
        } else {
          column <- c(column,
                     (as.numeric(bericht.beleid$Persberichten[[i]]) / total * 100)
          )
        }
      }
      bericht.beleid$Persberichten <- column
    }
    return(bericht.beleid)
  })
  
  # Define color pallete ------------------------------------------------------
  colors <- c(brewer.pal(8,"Pastel2"), brewer.pal(9, "Pastel1"))
  
  # Plot (Per beleid) ---------------------------------------------------------
  berichten.plot.maand <- reactive(
    (if (input$type == "Barplot") {
    # Basic Barplot -----------------------------------------------------------  
      ggplot(data = df.bericht.beleid(), aes(x = Beleid, y = Persberichten, fill = Beleid)) +
             geom_bar(position = "dodge", stat = 'identity') +
             geom_text(aes(label = if(input$inhoud == "Aantal") {Persberichten} else {percent(Persberichten, accuracy = 0.1, scale = 1)}),
                       position = position_dodge(0.9), vjust=0) +
             theme_bw()
    } else {
    # Basic Piechart ----------------------------------------------------------
      ggplot(data = df.bericht.beleid(), aes(x = "", y = Persberichten, fill = Beleid)) +
             geom_bar(width = 1, size = 1, color = "white", stat = 'identity') +
             coord_polar("y", start = 0) +
             geom_text(aes(label = if(input$inhoud == "Aantal") {Persberichten} else {percent(Persberichten, accuracy = 0.1, scale = 1)}),
                       position = position_stack(vjust = 0.5)) +
             theme_minimal()
    }) +
    # Other options -----------------------------------------------------------
       ggtitle(input$title) +
       xlab(input$Xaxis) +
       ylab(input$Yaxis) +
       scale_fill_manual(values=colors) + 
       (if (input$Xlabels) {theme(axis.text.x = element_text(angle = 45, hjust = 1))} else {theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())}) +
       (if (!(input$legend)) {theme(legend.position = "none")})
  )
  return(reactive(berichten.plot.maand()))
}