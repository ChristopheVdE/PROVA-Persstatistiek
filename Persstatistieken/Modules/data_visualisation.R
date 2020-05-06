###############################################################################
# MODULE: Data visualisation (plot and table creation)
###############################################################################

# LOAD PACKAGES ===============================================================
library(shiny)
library(ggplot2)
library(RColorBrewer)
library(scales)
# =============================================================================

# LOAD MODULES & FUNCTIONS ====================================================
source("D:/Documenten/GitHub/Persstatistiek/Persstatistieken/Modules/Functions/percentages.R")
source("D:/Documenten/GitHub/Persstatistiek/Persstatistieken/Modules/Functions/simple_barplot.R")
source("D:/Documenten/GitHub/Persstatistiek/Persstatistieken/Modules/Functions/simple_piechart.R")
source("D:/Documenten/GitHub/Persstatistiek/Persstatistieken/Modules/Functions/advanced_piechart.R")
# =============================================================================

# UI ==========================================================================
data.visualOutput <- function(id, plottitle, Xaxis, Xlabels) {
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
        selectInput(ns("type.aantal"), label = "Plot type - Aantal", choices = c("Barplot", "Taartdiagram"), selected = "Barplot"),
        selectInput(ns("type.procent"), label = "Plot type - Procentueel", choices = c("Barplot", "Taartdiagram"), selected = "Barplot"),
        checkboxInput(ns("Xlabels"), label = "As labels (X-as)", value = Xlabels),
        checkboxInput(ns("legend"), label = "Legende", value = TRUE)
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
data.visual <- function(input, output, session, Id, data, Xaxis, Fill, colours, beleid = NULL, verzender = NULL) {

  # Data preparation ---------------------------------------------------------- 
    df.berichten <- reactive(
    # Persberichten -----------------------------------------------------------
      # Algemeen Kwartaal -----------------------------------------------------
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
      # Algemeen Maand --------------------------------------------------------
        else if (Id == "alg.maand") {
        # Create table
          berichten <- data.frame(table(data()$Maand))
          colnames(berichten) <- c("Maand", "Persberichten")
        # Add percentages
          berichten <- data.frame(berichten, "Procentueel" = calc_percentages(Id, berichten))
        # Return df
          berichten
        } 
      # Algemeen Dag ----------------------------------------------------------
      else if (Id == "alg.dag") {
        # Create table
        berichten <- data.frame(table(data()$Dag))
        colnames(berichten) <- c("Dag", "Persberichten")
        # Add percentages
        berichten <- data.frame(berichten, "Procentueel" = calc_percentages(Id, berichten))
        # Return df
        berichten
      } 
      # Algemeen Week ---------------------------------------------------------
      else if (Id == "alg.week") {
        # Create table
        berichten <- data.frame(table(data()$Week))
        colnames(berichten) <- c("Week", "Persberichten")
        # Add percentages
        berichten <- data.frame(berichten, "Procentueel" = calc_percentages(Id, berichten))
        # Return df
        berichten
      } 
      # Algemeen Beleid -------------------------------------------------------
        else if (Id == "alg.beleid") {
        # Create table
          berichten <- data.frame(table(data()$Beleid))
          colnames(berichten) <- c("Beleid","Persberichten")
        # Add percentages
          berichten <- data.frame(berichten, "Procentueel" = calc_percentages(Id, berichten))
        # Return df
          berichten
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
        # Create actual table for chose "beleid"
          temp <- split(data(), data()$Beleid)
          temp <- data.frame("Beleid" = beleid, table(temp[[beleid]]$"Deelbeleid"))
          colnames(temp) <- c("Beleid", "Deelbeleid","Persberichten")
        # Specify "deelbeleid" per "Beleid"
          deelbeleid <- levels(temp[[beleid]]$"Deelbeleid")
          if(beleid == "Economie") {
            for (i in c("Economie, Innovatie en Samenleving", "Europa", "Financien", "Havencentrum", "Hooibeekhoeve", "Innovant", "Interreg", "Landbouw", "Logistiek", "Mondiaal beleid", "Plattelandsbeleid", "POM Antwerpen", "Sociale economie")) {
              if (!(i %in% deelbeleid)) {
                deelbeleid <- c(deelbeleid, i)
              }
            }
          } else if(beleid == "Gouverneur") {
            for (i in c("Toezicht gemeenten", "Veiligheid")) {
              if (!(i %in% deelbeleid)) {
                deelbeleid <- c(deelbeleid, i)
              }
            }
          } else if(beleid == "Leefmilieu") {
            for (i in c("Bosgroepen", "Duurzaam milieu en natuurgebied", "Kamp C", "Klimaatstrijd", "Landschap", "Milieu en natuur", "MOS", "PIH", "Regionale landschappen", "Waterbeleid")) {
              if (!(i %in% deelbeleid)) {
                deelbeleid <- c(deelbeleid, i)
              }
            }
          } else if(beleid == "Mobiliteit") {
            for (i in c("Fietsbeleid", "Fietseducatie")) {
              if (!(i %in% deelbeleid)) {
                deelbeleid <- c(deelbeleid, i)
              }
            }
          } else if(beleid == "Onderwijs en Educatie") {
            for (i in c("Avant", "Campus Vesta", "CVO Vivant", "Onderwijs", "PITO Starbroek", "PIVA", "PTS Boom", "Suske en Wiske", "Veiligheidsinstituut", "Vormingscentrum")) {
              if (!(i %in% deelbeleid)) {
                deelbeleid <- c(deelbeleid, i)
              }
            }
          } else if(beleid == "Provinciebestuur") {
            for (i in c("Activiteitenkalender", "Pers", "Persagenda", "Provincieraad")) {
              if (!(i %in% deelbeleid)) {
                deelbeleid <- c(deelbeleid, i)
              }
            }
          } else if(beleid == "Ruimte") {
            for (i in c("Erfgoed", "Ruimtelijke planning")) {
              if (!(i %in% deelbeleid)) {
                deelbeleid <- c(deelbeleid, i)
              }
            }
          } else if(beleid == "Vrije Tijd") {
            for (i in c("Arboretum", "De Nekker", "De Schorre", "de Warande", "Kasteel d'Ursel", "Kempens Landschap", "PGRA", "PGRA - M - K", "PGRK", "PGRM", "Terra Nova", "Toerisme Provincie Antwerpen", "Zilvermeer")) {
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
            for (i in c("Economie, Innovatie en Samenleving", "Europa", "Financien", "Havencentrum", "Hooibeekhoeve", "Innovant", "Interreg", "Landbouw", "Logistiek", "Mondiaal beleid", "Plattelandsbeleid", "POM Antwerpen", "Sociale economie")) {
              if (!(i %in% deelbeleid)) {
                deelbeleid <- c(deelbeleid, i)
              }
            }
          } else if(beleid == "Gouverneur") {
            for (i in c("Toezicht gemeenten", "Veiligheid")) {
              if (!(i %in% deelbeleid)) {
                deelbeleid <- c(deelbeleid, i)
              }
            }
          } else if(beleid == "Leefmilieu") {
            for (i in c("Bosgroepen", "Duurzaam milieu en natuurgebied", "Kamp C", "Klimaatstrijd", "Landschap", "Milieu en natuur", "MOS", "PIH", "Regionale landschappen", "Waterbeleid")) {
              if (!(i %in% deelbeleid)) {
                deelbeleid <- c(deelbeleid, i)
              }
            }
          } else if(beleid == "Mobiliteit") {
            for (i in c("Fietsbeleid", "Fietseducatie")) {
              if (!(i %in% deelbeleid)) {
                deelbeleid <- c(deelbeleid, i)
              }
            }
          } else if(beleid == "Onderwijs en Educatie") {
            for (i in c("Avant", "Campus Vesta", "CVO Vivant", "Onderwijs", "PITO Starbroek", "PIVA", "PTS Boom", "Suske en Wiske", "Veiligheidsinstituut", "Vormingscentrum")) {
              if (!(i %in% deelbeleid)) {
                deelbeleid <- c(deelbeleid, i)
              }
            }
          } else if(beleid == "Provinciebestuur") {
            for (i in c("Activiteitenkalender", "Pers", "Persagenda", "Provincieraad")) {
              if (!(i %in% deelbeleid)) {
                deelbeleid <- c(deelbeleid, i)
              }
            }
          } else if(beleid == "Ruimte") {
            for (i in c("Erfgoed", "Ruimtelijke planning")) {
              if (!(i %in% deelbeleid)) {
                deelbeleid <- c(deelbeleid, i)
              }
            }
          } else if(beleid == "Vrije Tijd") {
            for (i in c("Arboretum", "De Nekker", "De Schorre", "de Warande", "Kasteel d'Ursel", "Kempens Landschap", "PGRA", "PGRA - M - K", "PGRK", "PGRM", "Terra Nova", "Toerisme Provincie Antwerpen", "Zilvermeer")) {
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
            if (!(Id == "verzender.alg.beleid")) {
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
            } else {
              plots[[inhoud]] <- advanced.pie(Id = "verzender.alg.beleid", data = df.berichten)
            }
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




