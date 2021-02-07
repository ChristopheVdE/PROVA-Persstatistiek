###############################################################################
# MODULE: Data visualisation (plot and table creation)
###############################################################################

# LOAD PACKAGES ===============================================================
library(scales)
# =============================================================================

# LOAD MODULES & FUNCTIONS ====================================================
source("./Modules/Functions/percentages.R")
source("./Modules/Functions/simple_barplot.R")
source("./Modules/Functions/simple_piechart.R")
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
    # Persberichten -----------------------------------------------------------
      # Algemeen - Persberichten
        # Kwartaal ------------------------------------------------------------
          if (Id == "alg.kwartaal") {
          # Create dummy dataframe
            berichten <- data.frame(
              Kwartaal = factor(c("Q1", "Q2", "Q3", "Q4"), c("Q1", "Q2", "Q3", "Q4")),
              Persberichten = 0
            )
          # Create table
            temp <- data.frame(table(data()$Kwartaal))
            colnames(temp) <- c("Kwartaal", "Persberichten")
          # Update values of dummy dataframe
            for (i in temp$Kwartaal) {
              berichten$Persberichten[grepl(i, berichten$Kwartaal)] <- temp$Persberichten[grepl(i, temp$Kwartaal)]
            }
          # Add percentages
            berichten <- data.frame(berichten, "Procentueel" = calc_percentages(Id, berichten))
          # Return df
            berichten
          }
        # Maand ---------------------------------------------------------------
          else if (Id == "alg.maand") {
          # Create table
            berichten <- data.frame(table(data()$Maand))
            colnames(berichten) <- c("Maand", "Persberichten")
          # Add percentages
            berichten <- data.frame(berichten, "Procentueel" = calc_percentages(Id, berichten))
          # Return df
            berichten
          } 
        # Dag -----------------------------------------------------------------
        else if (Id == "alg.dag") {
          # Create table
          berichten <- data.frame(table(data()$Dag))
          colnames(berichten) <- c("Dag", "Persberichten")
          # Add percentages
          berichten <- data.frame(berichten, "Procentueel" = calc_percentages(Id, berichten))
          # Return df
          berichten
        } 
        # Week ----------------------------------------------------------------
        else if (Id == "alg.week") {
          # Create table
          berichten <- data.frame(table(data()$Week))
          colnames(berichten) <- c("Week", "Persberichten")
          # Add percentages
          berichten <- data.frame(berichten, "Procentueel" = calc_percentages(Id, berichten))
          # Return df
          berichten
        } 
        # Beleid --------------------------------------------------------------
          else if (Id == "alg.beleid") {
          # Create table
            berichten <- data.frame(table(data()$Beleid))
            colnames(berichten) <- c("Beleid","Persberichten")
          # Add percentages
            berichten <- data.frame(berichten, "Procentueel" = calc_percentages(Id, berichten))
          # Return df
            berichten
          } 
      # Algemeen Persconferenties ---------------------------------------------
        else if (grepl("conferentie", Id)) {
        # Data preparation ----------------------------------------------------
          temp <- split(data(), data()$Persconferentie)
          temp <- temp$Ja
          for (i in colnames(temp)) {
            if (!(i %in% c("Kwartaal", "Datum PC", "Beleid", "Persconferentie"))) {
              temp[[i]] <- NULL
            }
          }
        # Kwartaal ------------------------------------------------------------
          if (Id == "conferentie.alg.kwartaal") {
            # Create dummy dataframe
            berichten <- data.frame(
              Kwartaal = factor(c("Q1", "Q2", "Q3", "Q4"), c("Q1", "Q2", "Q3", "Q4")),
              Persberichten = 0
            )
            # Create table
            temp <- data.frame(table(temp$Kwartaal))
            colnames(temp) <- c("Kwartaal", "Persberichten")
            # Update values of dummy dataframe
            for (i in temp$Kwartaal) {
              berichten$Persberichten[grepl(i, berichten$Kwartaal)] <- temp$Persberichten[grepl(i, temp$Kwartaal)]
            }
            # Add percentages
            berichten <- data.frame(berichten, "Procentueel" = calc_percentages(Id, berichten))
            # Return df
            colnames(berichten) <- c("Kwartaal", "Persconferenties", "Procentueel")
            berichten
          }
        # Maand ---------------------------------------------------------------
          else if (Id == "conferentie.alg.maand") {
            # Maand PC toevoegen 
            temp$Maand <- factor(format(as.Date(temp$"Datum PC"), format = "%m"), levels = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12"))
            levels(temp$Maand) <- c("jan", "feb", "mrt", "apr", "mei","jun","jul","aug","sep","okt","nov","dec")
            # Create table
            berichten <- data.frame(table(temp$Maand))
            colnames(berichten) <- c("Maand", "Persberichten")
            # Add percentages
            berichten <- data.frame(berichten, "Procentueel" = calc_percentages(Id, berichten))
            # Return df
            colnames(berichten) <- c("Maand", "Persconferenties", "Procentueel")
            berichten
          }
        # Dag -----------------------------------------------------------------
          else if (Id == "conferentie.alg.dag") {
            # Dag Toevoegen
            temp$Dag <- factor(format(as.Date(temp$"Datum PC"), format = "%u"), levels = c(1:7))
            levels(temp$Dag) <- c("ma", "di", "wo", "do", "vr", "za", "zo")
            # Create table
            berichten <- data.frame(table(temp$Dag))
            colnames(berichten) <- c("Dag", "Persberichten")
            # Add percentages
            berichten <- data.frame(berichten, "Procentueel" = calc_percentages(Id, berichten))
            # Return df
            colnames(berichten) <- c("Dag", "Persconferenties", "Procentueel")
            berichten
          }
        # Week ----------------------------------------------------------------
          else if (Id == "conferentie.alg.week") {
            # Week toevoegen
            temp$Week <- ISOweek(as.Date(temp$"Datum PC"))
            # Create table
            berichten <- data.frame(table(temp$Week))
            colnames(berichten) <- c("Week", "Persberichten")
            # Add percentages
            berichten <- data.frame(berichten, "Procentueel" = calc_percentages(Id, berichten))
            # Return df
            colnames(berichten) <- c("Week", "Persconferenties", "Procentueel")
            berichten
          }
        # Beleid --------------------------------------------------------------
          else if (Id == "conferentie.alg.beleid") {
            # Create table
            berichten <- data.frame(table(temp$Beleid))
            colnames(berichten) <- c("Beleid","Persberichten")
            # Add percentages
            berichten <- data.frame(berichten, "Procentueel" = calc_percentages(Id, berichten))
            # Return df
            colnames(berichten) <- c("Beleid","Persconferenties", "Procentueel")
            berichten
          }
          return(berichten)
        }
      # Per beleid: Maand -----------------------------------------------------
        else if (Id == "beleid.maand") {
        # Create table
          berichten <- data.frame(table(data()$Beleid, data()$Maand))
          colnames(berichten) <- c("Beleid", "Maand", "Persberichten")
        # Split dataframe on "Beleid"
          berichten <- split(berichten, berichten$Beleid)
          berichten <- berichten[[beleid]]
        # Add percentages
          berichten <- data.frame(berichten, "Procentueel" = calc_percentages(Id, berichten))
        # Return
          berichten
        }
      # Per beleid: Deelbeleid ------------------------------------------------
        else if (Id == "beleid.beleid") {
        # Create actual table for chosen "beleid"
          temp <- split(data(), data()$Beleid)
          temp <- data.frame("Beleid" = beleid, table(temp[[beleid]]$"Deelbeleid"))
          colnames(temp) <- c("Beleid", "Deelbeleid","Persberichten")
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
        # Create dummy dataframes
          berichten <- data.frame(
            Beleid = beleid,
            Deelbeleid = deelbeleid,
            Persberichten = 0
          )

        # Update values of dummy dataframe
          suppressWarnings(
            for (i in temp$Deelbeleid) {
              berichten$Persberichten[grepl(i, berichten$Deelbeleid)] <- temp$Persberichten[grepl(i, temp$Deelbeleid)]
            }
          )
        # Add percentages
          berichten <- data.frame(berichten, "Procentueel" = calc_percentages(Id, berichten))
        # Return
          berichten
        } 
      # Verzender Algemeen - Verzender ----------------------------------------
        else if (Id == "verzender.alg.verzender") {
        # Create table
          berichten <- data.frame(table(data()$Verzender))
          colnames(berichten) <- c("Verzender", "Persberichten")
        # Add missing "Verzender"
          for(i in c("Persdienst", "Provincie", "Gouverneur", "Extern")) {
            if(!(i %in% levels(berichten$Verzender))) {
              temp <- data.frame(
                Verzender = i,
                Persberichten = 0
              )
              berichten <- rbind(berichten, temp)
            }
          }
        # Add percentages
          berichten <- data.frame(berichten, "Procentueel" = calc_percentages(Id, berichten))
        # Return
          berichten
        } 
      # Verzender Algemeen - Beleid -------------------------------------------
        else if (Id == "verzender.alg.beleid") {
        # Create table
          berichten <- data.frame(table(data()$Beleid, data()$Verzender))
          colnames(berichten) <- c("Beleid", "Verzender", "Persberichten")
        # Add missing "Verzender"
          for(i in c("Persdienst", "Provincie", "Gouverneur", "Extern")) {
            if(!(i %in% levels(berichten$Verzender))) {
              temp <- data.frame(
                Beleid = c("Economie", "Gouverneur", "Leefmilieu", "Mobiliteit", "Onderwijs en Educatie", "Provinciebestuur", "Ruimte", "Vrije Tijd"),
                Verzender = i,
                Persberichten = 0
              )
              berichten <- rbind(berichten, temp)
            }
          }
        # Add percentages
          berichten <- data.frame(berichten, "Procentueel" = calc_percentages(Id, berichten))
        # Return
          berichten
        }
      # Verzender: Maand ------------------------------------------------------
        else if (Id == "verzender.maand") {
        # Create table
          berichten <- data.frame(table(data()$Verzender, data()$Maand))
          colnames(berichten) <- c("Verzender", "Maand", "Persberichten")
        # Add missing "Verzender"
          for(i in c("Persdienst", "Provincie", "Gouverneur", "Extern")) {
            if(!(i %in% levels(berichten$Verzender))) {
              temp <- data.frame(
                Maand = levels(berichten$Maand),
                Verzender = i,
                Persberichten = 0
              )
              berichten <- rbind(berichten, temp)
            }
          }
        # Display only selected verzender
          berichten <- split(berichten, berichten$Verzender)
          berichten <- berichten[[verzender]]
        # Add percentages
          berichten <- data.frame(berichten, "Procentueel" = calc_percentages(Id, berichten))
        # Return
          berichten
        } 
      # Type ------------------------------------------------------------------
        else if (Id == "type") {
        # Create tabel
          berichten <- data.frame(table(data()$Beleid, data()$Soort))
          colnames(berichten) <- c("Beleid", "Type", "Persberichten")
          berichten$Type <- as.factor(berichten$Type)
        # Add missing "Type"
          for(i in c("Activiteitenkalender", "Agendatip", "Evenementenkalender", "Persagenda", "Persbericht", "Persuitnodiging")) {
            if(!(i %in% levels(berichten$Type))) {
              temp <- data.frame(
                Beleid = c("Economie", "Gouverneur", "Leefmilieu", "Mobiliteit", "Onderwijs en Educatie", "Provinciebestuur", "Ruimte", "Vrije Tijd"),
                Type = i,
                Persberichten = 0
              )
              berichten <- rbind(berichten, temp)
            }
          }
        # Add percentages
          berichten <- data.frame(berichten[order(berichten$Beleid),], "Procentueel" = calc_percentages(Id, berichten))
        # Return
          berichten
        }
    # Persreturn --------------------------------------------------------------
      # Per beleid: Algemeen --------------------------------------------------
        else if (Id == "return.beleid.alg") {
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
    # Persberichten: verzender - alg - beleid ---------------------------------
      if (Id == "verzender.alg.beleid") {
        temp <- split(df.berichten(), df.berichten()$Verzender)
        temp <- data.frame(Beleid = levels(df.berichten()$Beleid),
                           Persdienst = temp$Persdienst$Persberichten,
                           Provincie = temp$Provincie$Persberichten,
                           Gouverneur = temp$Gouverneur$Persberichten,
                           Extern = temp$Extern$Persberichten)
      }
    # Persberichten: type -----------------------------------------------------
      else if (Id == "type") {
        temp <- split(df.berichten(), df.berichten()$Type)
        temp <- data.frame(
          Beleid = levels(df.berichten()$Beleid),
          Agendatip = temp$Agendatip$Persberichten, 
          Evenementenkalender = temp$Evenementenkalender$Persberichten,
          Persagenda = temp$Persagenda$Persberichten,
          Persbericht = temp$Persbericht$Persberichten,
          Persuitnodiging = temp$Persuitnodiging$Persberichten
        )
      }
    # Persreturn - beleid - alg -----------------------------------------------
      else if (Id == "return.beleid.alg") {
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
          Deelbeleid = levels(df.berichten()$Deelbeleid),
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




