###############################################################################
# MODULE: Data visualisation (plot and table creation)
###############################################################################

# LOAD PACKAGES ===============================================================
library(scales)
# =============================================================================

# LOAD MODULES & FUNCTIONS ====================================================
source("./Modules/JaarOverzicht/percentages.R")
source("./Modules/JaarOverzicht/Plots/simple_barplot.R")
source("./Modules/JaarOverzicht/Plots/simple_piechart.R")
# =============================================================================

# UI ==========================================================================
data.visualOutput <- function(id, plottitle, Xaxis, Xlabels, Legende = TRUE, Piechart = TRUE) {
  ns <- NS(id)
  tabPanel(
    "Opties",
    fluidRow(
      column(
        width = 6,
        textInput(ns("title"), label = "Plot title", value = plottitle, placeholder = "Plot titel"),
        textInput(ns("Xaxis"), label = "X-as naam", value = Xaxis, placeholder = Xaxis),
        textInput(ns("Yaxis"), label = "Y-as naam", value = "Aantal", placeholder = "Aantal")
      ),
      column(
        width = 6,
        selectInput(ns("type.aantal"), label = "Plot type - Aantal", choices = (if(Piechart == TRUE) {c("Barplot", "Taartdiagram")} else {c("Barplot")}), selected = "Barplot"),
        selectInput(ns("type.procent"), label = "Plot type - Procentueel", choices = (if(Piechart == TRUE) {c("Barplot", "Taartdiagram")} else {c("Barplot")}), selected = "Barplot"),
        checkboxInput(ns("Xlabels"), label = "As labels (X-as)", value = Xlabels),
        checkboxInput(ns("legend"), label = "Legende", value = Legende)
      )
    ),
    fluidRow(
      tags$hr(),
      column(width = 12,
             textInput(ns("uitleg"), label ="Typ hier uitleg bij grafiek/ tabel:", value = "", placeholder = "Uitleg bij grafiek", width = "100%")
      )
    )
  )
}

# SERVER ======================================================================
data.visual <- function(input, output, session, Id, data, Xaxis, Fill, colours, beleid = NULL, datadeelbeleid = NULL, verzender = NULL) {

  # Data preparation ---------------------------------------------------------- 
    df.berichten <- reactive(
    # Persreturn --------------------------------------------------------------
      # Per beleid: Algemeen --------------------------------------------------
        if (Id == "return.beleid.alg") {
        # Create table
          berichten <- data.frame(table(data()$Beleid, data()$Persreturn))
          colnames(berichten) <- c("Beleid", "Persreturn", "Aantal")
        # Add percentages
          berichten <- data.frame(berichten[order(berichten$Beleid),], "Procentueel" = calc_percentages(Id, berichten))
        # Return
          berichten
        }
      # Per beleid: Deelbeleid ------------------------------------------------
        else if (Id == "return.beleid.beleid") {
        # Create actual table for chose "beleid"
          temp <- split(data(), data()$Beleid)
          temp <- temp[[beleid]]
          temp <- data.frame("Beleid" = beleid, table(temp$Deelbeleid, temp$Persreturn))
          colnames(temp) <- c("Beleid", "Deelbeleid","Persreturn", "Aantal")
          # Specify "deelbeleid" per "Beleid"
          deelbeleid <- levels(temp[[beleid]]$"Deelbeleid")
          if(beleid == "Economie") {
            for (i in datadeelbeleid()) {
              if (!(i %in% deelbeleid)) {
                deelbeleid <- c(deelbeleid, i)
              }
            }
          } else if(beleid == "Gouverneur") {
            for (i in datadeelbeleid()) {
              if (!(i %in% deelbeleid)) {
                deelbeleid <- c(deelbeleid, i)
              }
            }
          } else if(beleid == "Leefmilieu") {
            for (i in datadeelbeleid()) {
              if (!(i %in% deelbeleid)) {
                deelbeleid <- c(deelbeleid, i)
              }
            }
          } else if(beleid == "Mobiliteit") {
            for (i in datadeelbeleid()) {
              if (!(i %in% deelbeleid)) {
                deelbeleid <- c(deelbeleid, i)
              }
            }
          } else if(beleid == "Onderwijs en Educatie") {
            for (i in datadeelbeleid()) {
              if (!(i %in% deelbeleid)) {
                deelbeleid <- c(deelbeleid, i)
              }
            }
          } else if(beleid == "Provinciebestuur") {
            for (i in datadeelbeleid()) {
              if (!(i %in% deelbeleid)) {
                deelbeleid <- c(deelbeleid, i)
              }
            }
          } else if(beleid == "Ruimte") {
            for (i in datadeelbeleid()) {
              if (!(i %in% deelbeleid)) {
                deelbeleid <- c(deelbeleid, i)
              }
            }
          } else if(beleid == "Vrije Tijd") {
            for (i in datadeelbeleid()) {
              if (!(i %in% deelbeleid)) {
                deelbeleid <- c(deelbeleid, i)
              }
            }
          }
        # Create dummy dataframes containing every "Deelbeleid" of the "Beleid"
          berichten.Ja <- data.frame(
            Beleid = beleid,
            Deelbeleid = deelbeleid,
            Persreturn = "Ja",
            Aantal = 0
          )
          berichten.Nee <- data.frame(
            Beleid = beleid,
            Deelbeleid = deelbeleid,
            Persreturn = "Nee",
            Aantal = 0
          )
        # Update dummy dataframes
          temp <- split(temp, temp$Persreturn)
          suppressWarnings(
            for (i in temp[["Ja"]]$Deelbeleid) {
              berichten.Ja$Aantal[grepl(i, berichten.Ja$Deelbeleid)] <- temp[["Ja"]]$Aantal[grepl(i, temp[["Ja"]]$Deelbeleid)]
            }
          )
          suppressWarnings(
            for (i in temp[["Nee"]]$Deelbeleid) {
              berichten.Nee$Aantal[grepl(i, berichten.Nee$Deelbeleid)] <- temp[["Nee"]]$Aantal[grepl(i, temp[["Nee"]]$Deelbeleid)]
            }
          )
        # Merge dummy dataframes
          berichten <- rbind(berichten.Ja, berichten.Nee)
          berichten <- berichten[order(berichten$Deelbeleid),]
        # Add percentages
          berichten <- data.frame(berichten[order(berichten$Deelbeleid),], "Procentueel" = calc_percentages(Id, berichten))
        # Return
          berichten
        }
      # Per Medium ----------------------------------------------------------
        else if (Id == "return.medium") {
        # Create table: persreturn algemeen
          Algemeen <- split(data(), data()$Persreturn)
          Algemeen <- Algemeen$Ja
          Algemeen <- data.frame(table(Algemeen$Beleid, Algemeen$Persreturn))
          colnames(Algemeen) <- c("Beleid", "Algemeen", "Freq")
          Algemeen$Algemeen <- "Algemeen"
          
        # Create table: TV
          TV <- split(data(), data()$TV)
          TV <- TV$Ja
          TV <- data.frame(table(TV$Beleid, TV$TV))
          colnames(TV) <- c("Beleid", "TV", "Freq")
          TV$TV <- "TV"
          
        # Create Table: Web
          Web <- split(data(), data()$"Alleen web")
          Web <- Web$Ja
          Web$Ja <- "Web"
          Web <- data.frame(table(Web$Beleid, Web$"Alleen web"))
          colnames(Web) <- c("Beleid", "Alleen web", "Freq")
          Web$"Alleen web" <- "Alleen web"
          
        # Merge dataframes
          berichten <- data.frame(Beleid = TV$Beleid,
                                  Medium = c(Algemeen$Algemeen, TV$TV, Web$"Alleen web"),
                                  Aantal = c(Algemeen$Freq, TV$Freq, Web$Freq))
        # Add percentages
          berichten <- data.frame(berichten[order(berichten$Beleid),], "Procentueel" = calc_percentages(Id, berichten))
        # Return
          berichten
        }
  )
  
  # Plot ----------------------------------------------------------------------
    berichten.plot <- reactive({
      plots <- list("Aantal" = NA, "Procent" = NA)
      for (inhoud in c("Aantal", "Procent")) {
    # Barplot -----------------------------------------------------------------
        if ((inhoud == "Aantal" && input$type.aantal == "Barplot") || (inhoud == "Procent" && input$type.procent == "Barplot")) {
          plots[[inhoud]] <- simple_barplot(Id = Id,
                                       data = df.berichten, 
                                       Xaxis = Xaxis,
                                       Fill = Fill,
                                       visual = inhoud, 
                                       title = input$title, 
                                       Xtitle = input$Xaxis, 
                                       Ytitle = input$Yaxis, 
                                       Xlabels = input$Xlabels, 
                                       legend = input$legend, 
                                       colors = colours)
        } 
    # Taartdiagram ------------------------------------------------------------
        else if ((inhoud == "Aantal" && input$type.aantal == "Taartdiagram") || (inhoud == "Procent" && input$type.procent == "Taartdiagram")) {
          plots[[inhoud]] <- simple_piechart(Id = Id,
                                             data = df.berichten,
                                             Fill = Fill,
                                             visual = inhoud,
                                             title = input$title,
                                             Xtitle = input$Xaxis,
                                             Ytitle = input$Yaxis,
                                             Xlabels = input$Xlabels,
                                             legend = input$legend,
                                             colors = colours)
        }
      }
      return(plots)
    })
  # Table ---------------------------------------------------------------------
    tabel <- reactive(
    # Persreturn - beleid - alg -----------------------------------------------
      if (Id == "return.beleid.alg") {
        temp <- split(df.berichten(), df.berichten()$Persreturn)
        temp <- data.frame(
          Beleid = levels(df.berichten()$Beleid),
          Ja = temp$Ja$Aantal,
          Nee = temp$Nee$Aantal
        )
        colnames(temp) <- c("Beleid", "Persreturn", "Geen persreturn")
        temp
      }
    # Persreturn - beleid - deelbeleid  ---------------------------------------
      else if (Id == "return.beleid.beleid") {
        temp <- split(df.berichten(), df.berichten()$Persreturn)
        temp <- data.frame(
          Beleid = beleid,
          Deelbeleid = levels(as.factor(datadeelbeleid())),
          Ja = temp$Ja$Aantal,
          Nee = temp$Nee$Aantal 
        )
        colnames(temp) <- c("Beleid", "Deelbeleid", "Persreturn", "Geen persreturn")
        temp
      }
    # Persreturn - medium -----------------------------------------------------
      else if (Id == "return.medium") {
        temp <- split(df.berichten(), df.berichten()$Medium)
        temp <- data.frame(
          Beleid = levels(df.berichten()$Beleid),
          Algemeen = temp$Algemeen$Aantal,
          Web = temp$"Alleen web"$Aantal,
          TV = temp$TV$Aantal
        )
      }
    # Persberichten (tabellen zonder speciale bewerkingen) --------------------
      else {
        temp <- df.berichten()
        temp$Procentueel <- NULL
        temp
      }
    )
  # Return --------------------------------------------------------------------
  return(list(plot.aantal = reactive(berichten.plot()$Aantal), plot.procent = reactive(berichten.plot()$Procent), tabel = tabel, uitleg = reactive(input$uitleg)))
  }




