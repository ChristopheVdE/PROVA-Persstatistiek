###############################################################################
# MODULE: Persberichten - Algemeen: Beleidsplot
###############################################################################

library(shiny)
library(ggplot2)
library(RColorBrewer)
library(scales)

# UI ==========================================================================
bericht.beleid.maand.plotOutput <- function(id, plottitle) {
  ns <- NS(id)
  tabPanel(
    "Opties",
    fluidRow(
      column(
        width = 6,
        textInput(ns("title"), label = "Plot title", value = plottitle, placeholder = "Plot titel"),
        textInput(ns("Xaxis"), label = "X-as naam", value = "Maand", placeholder = "Maand"),
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
bericht.beleid.maand.plot <- function(input, output, session, data, beleid) {

  # Preparation ---------------------------------------------------------------  
  df.berichten.Maand.totaal.per.Beleid <-  reactive({
    # Create basic dataframe --------------------------------------------------
    berichten <- data.frame(table(data()$Beleid, data()$Maand))
    colnames(berichten) <- c("Beleid", "Maand", "Persberichten")
    berichten$Maand <- factor(berichten$Maand, levels = c("jan", "feb", "mrt", "apr", "mei", "jun", "jul", "aug", "sep", "okt", "nov", "dec"))
    berichten <- split(berichten, berichten$Beleid)
    berichten <- berichten[[beleid()]]
    
    # Calculate percentages -----------------------------------------------------
    if (input$inhoud == "Procentueel") {
      total <- sum(berichten$Persberichten)
      for (i in 0:length(berichten$Persberichten)) {
        if (i == 0) {
          column <- NULL
        } else {
          column <- c(column,
                     (as.numeric(berichten$Persberichten[[i]]) / total * 100)
          )
        }
      }
      berichten$Persberichten <- column
    }
    return(berichten)
  })

  
  
  # Define color pallete ------------------------------------------------------
  colors <- c(brewer.pal(8,"Pastel2"), brewer.pal(9, "Pastel1"))
  
  # Plot (beleid per maand) ---------------------------------------------------
  berichten.plot.beleid <- reactive(
    (if (input$type == "Barplot") {
    # Basic Barplot -----------------------------------------------------------
      ggplot(data = df.berichten.Maand.totaal.per.Beleid(), aes(x = Maand, y = Persberichten, fill = Maand)) +
        geom_bar(position = "dodge", stat = 'identity') +
        geom_text(aes(label = if(input$inhoud == "Aantal") {Persberichten} else {percent(Persberichten, accuracy = 0.1, scale = 1)}),
                  position = position_dodge(0.9), vjust=0) +
        theme_bw()
    } else {
    # Basic Piechart ----------------------------------------------------------
      ggplot(data = df.berichten.Maand.totaal.per.Beleid(), aes(x = "", y = Persberichten, fill = Maand)) +
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
      (if (input$inhoud == "Procentueel") {ylim(c(0,100))}) +
      (if (input$Xlabels) {theme(axis.text.x = element_text(angle = 45, hjust = 1))} else {theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())}) +
      (if (!(input$legend)) {theme(legend.position = "none")})
  )
  return(berichten.plot.beleid)
}




