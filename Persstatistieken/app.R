# Installing and loading packages
pkg <- installed.packages()[, "Package"]
to.load <- c("RColorBrewer", "knitr", "ggplot2", "readxl")
for (load in to.load) {
    if(!(load %in% pkg)) {
        install.packages(load, dependencies=TRUE)
    }
    library(load, character.only = TRUE )
}

if (interactive()) {
    shinyApp(
    # UI =======================================================================
        ui = fluidPage(
            # Title ------------------------------------------------------------
            titlePanel(
                "Persstatistiek"
            ),
            
            # Input ------------------------------------------------------------
            sidebarLayout(
                sidebarPanel(
                    # File INPUT -----------------------------------------------
                    fileInput("file", 
                              "Kies Excel document:", 
                              multiple = FALSE, 
                              accept = c(".xls", ".xlsx"), 
                              #width = 900, 
                              placeholder = "No file selected"),
                    
                    # Werkblad selectie ----------------------------------------
                    textInput("sheet", "Te gebruiken Werkblad", value ="Hele organisatie", placeholder = "Hele organisatie" ),
                    
    
                    
                    # Headers --------------------------------------------------
                    checkboxInput("header", "Eerste lijn bevat kolomnamen", TRUE),
                    
                    # Kwartaal selectie ----------------------------------------
                    tags$hr(),
                    selectInput("kwartaal", 
                                "Selecteer kwartaal:", 
                                choices = c("Q1", "Q2", "Q3", "Q4", "Jaar"), 
                                selected = "Q1"),
                    
                    
                    # Ander
                    sliderInput("slider", "Slider", 1, 100, 50),
                    
                    # Download pdf ---------------------------------------------
                    downloadButton("report", "Generate report")
                ),
            
            
            # Main -------------------------------------------------------------
                mainPanel(
                    #verbatimTextOutput("summary"),
                    tableOutput("summary")
                    
                )
            )
        ),
        
    # SERVER ===================================================================
        server <- function(input, output) {
            
        # Prepare dataset ------------------------------------------------------
            output$summary <- renderTable({
                
                req(input$file)
                
            # Reading Excel ----------------------------------------------------
                Excel <- read_excel(input$file$datapath, sheet = input$sheet, col_names = input$header)
                
            # Removing non-required columns ------------------------------------
                Excel$Afzender <- NULL
                Excel$Onderwerp <- NULL
                Excel$Datum <- NULL

            # Fixing Mistakes --------------------------------------------------

              # Verzender ------------------------------------------------------
                Excel$Verzender <- gsub("extern", "Extern", Excel$Verzender, ignore.case = FALSE)
                Excel$Verzender <- gsub("gouverneur", "Gouverneur", Excel$Verzender, ignore.case = FALSE)
                Excel$Verzender <- gsub("persdienst", "Persdienst", Excel$Verzender, ignore.case = FALSE)
                Excel$Verzender <- gsub("provincie", "Provincie", Excel$Verzender, ignore.case = FALSE)

              # Pu bij Pb; Persreturn; Alleen web; TV --------------------------
                for (i in c("Pu bij Pb", "Persreturn", "Alleen web", "TV")) ({
                    Excel[[i]] <- gsub("Ja", "Ja", Excel[[i]], ignore.case = FALSE)
                    Excel[[i]] <- gsub("nee", "Nee", Excel[[i]], ignore.case = FALSE)
                })

              # Beleid ---------------------------------------------------------
                Excel$Beleid <- gsub("economie", "Economie", Excel$Beleid, ignore.case = FALSE)
                Excel$Beleid <- gsub("gouverneur", "Gouverneur", Excel$Beleid, ignore.case = FALSE)
                Excel$Beleid <- gsub("leefmilieu", "Leefmilieu", Excel$Beleid, ignore.case = FALSE)
                Excel$Beleid <- gsub("mobiliteit", "Mobiliteit", Excel$Beleid, ignore.case = FALSE)
                Excel$Beleid <- gsub("Onderwijs en educatie", "Onderwijs en Educatie", Excel$Beleid, ignore.case = FALSE)
                Excel$Beleid <- gsub("provinciebestuur", "Provinciebestuur", Excel$Beleid, ignore.case = FALSE)
                Excel$Beleid <- gsub("ruimte", "Ruimte", Excel$Beleid, ignore.case = FALSE)
                Excel$Beleid <- gsub("Vrije tijd", "Vrije Tijd", Excel$Beleid, ignore.case = FALSE)

              # Detail Beleid --------------------------------------------------
                Excel$"Detail beleid" <- gsub("De Warande", "de Warande", Excel$"Detail beleid", ignore.case = FALSE)
                Excel$"Detail beleid" <- gsub("Economie, innovatie en samenleving", "Economie, Innovatie en Samenleving", Excel$"Detail beleid", ignore.case = FALSE)
                Excel$"Detail beleid" <- gsub("Economie, Innovatie en samenleving", "Economie, Innovatie en Samenleving", Excel$"Detail beleid", ignore.case = FALSE)
                Excel$"Detail beleid" <- gsub("Economie, innovatie en Samenleving", "Economie, Innovatie en Samenleving", Excel$"Detail beleid", ignore.case = FALSE)
                Excel$"Detail beleid" <- gsub("Regionaal Landschappen", "Regionale Landschappen", Excel$"Detail beleid", ignore.case = FALSE)
                Excel$"Detail beleid" <- gsub("Toerisme provincie Antwerpen", "Toerisme Provincie Antwerpen", Excel$"Detail beleid", ignore.case = FALSE)

              # Soort ----------------------------------------------------------
                Excel$Soort <- gsub("persbericht", "Persbericht", Excel$Soort, ignore.case = FALSE)
                Excel$Soort <- gsub("agendatip", "Agendatip", Excel$Soort, ignore.case = FALSE)
                Excel$Soort <- gsub("persagenda", "Persagenda", Excel$Soort, ignore.case = FALSE)
                Excel$Soort <- gsub("activiteitenkalender", "Activiteitenkalender", Excel$Soort, ignore.case = FALSE)
                Excel$Soort <- gsub("evenementenkalender", "Evenementenkalender", Excel$Soort, ignore.case = FALSE)

            # As factor --------------------------------------------------------
                for (i in c("Kwartaal", "Verzender", "Pu bij Pb", "Persreturn", "Alleen web", "TV", "Beleid", "Detail beleid", "Soort", "Maand")) ({
                    Excel[[i]] <- as.factor(Excel[[i]])
                })

            # Split ------------------------------------------------------------
                Persstatistiek <- split.data.frame(Excel, Excel$Kwartaal)
                Persstatistiek$Jaar <- Excel
                
            # Removing Excel ---------------------------------------------------
                Excel <- NULL
            
            # Return dataset ---------------------------------------------------
                return(Persstatistiek[[input$kwartaal]])
            }) 

        # Pdf aanmaak ----------------------------------------------------------
            output$report <- downloadHandler(
                # For PDF output, change this to "report.pdf"
                filename = "report.pdf",
                content = function(file) {
                    # Copy the report file to a temporary directory before processing it, in
                    # case we don't have write permissions to the current working dir (which
                    # can happen when deployed).
                    tempReport <- file.path(tempdir(), "report.Rmd")
                    file.copy("report.Rmd", tempReport, overwrite = TRUE)
                    
                    # Set up parameters to pass to Rmd document
                    params <- list(n = input$slider)
                    
                    # Knit the document, passing in the `params` list, and eval it in a
                    # child of the global environment (this isolates the code in the document
                    # from the code in this app).
                    rmarkdown::render(tempReport, output_file = file,
                                      params = params,
                                      envir = new.env(parent = globalenv())
                    )
                }
            )
        }
    )
}