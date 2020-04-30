# Download --------------------------------------------------------------
tabItem(
  tabName = "Download",
  fluidRow(
    # Download button -----------------------------------------------------
    box(
      width = 12,
      title = "Download",
      downloadButton("report", "Generate report")
    ),
    # Rapport volgorde ----------------------------------------------------
    box(
      width = 12,
      title = "Rapport volgorde",
      collapsible = TRUE,
      bucket_list(
        header = NULL,
        group_name = "Rapport",
        orientation = "vertical",
        add_rank_list(
          input_id = "rapport",
          text = "Toevoegen aan rapport",
          labels = list(
            # Persberichten -----------------------------------------------------
            Persberichten = box(
              width = 12,
              collapsible = TRUE,
              collapsed = TRUE,
              title = "Persberichten",
              rank_list(
                input_id = "Persberichten",
                labels = list(
                  # Algemeen --------------------------------------------------------
                  Algemeen = box(
                    width = 12,
                    collapsible = TRUE,
                    collapsed = TRUE,
                    title = "Algemeen",
                    bucket_list(
                      orientation = "horizontal", 
                      group_name = "Persberichten.Algemeen",
                      header = NULL,
                      add_rank_list(
                        input_id = "Persberichten.alg",
                        text = "Toevoegen aan rapport in onderstaande volgorde",
                        labels = list(
                          Kwartaal = fluidRow(
                            column(width = 3, "Algemeen Kwartaal"),
                            column(width = 9,  textInput("bericht.kwartaal.titel", label = NULL, value = "Persberichten per kwartaal", placeholder = "Sectie titel"))
                          ),
                          Maand = fluidRow(
                            column(width = 3, "Algemeen Maand"),
                            column(width = 9, textInput("bericht.maand.titel", label = NULL, value = "Persberichten per maand", placeholder = "Sectie titel"))
                          ),
                          Beleid = fluidRow(
                            column(width = 3, "Algemeen Beleid"),
                            column(width = 9, textInput("bericht.beleid.titel", label = NULL, value = "Persberichten per beleid", placeholder = "Sectie titel"))
                          )
                        )
                      ),
                      add_rank_list(
                        input_id = "Persberichten.alg.weg",
                        text = "Weglaten uit rapport",
                        labels = NULL
                      )
                    )
                  ),
                  # Beleid ----------------------------------------------------------
                  Beleid = box(
                    width = 12,
                    collapsible = TRUE,
                    collapsed = TRUE,
                    title = "Beleid",
                    # Volgorde van de verschillende beleidsdomeinen -----------------
                    bucket_list(
                      header = NULL,
                      group_name = "Persberichten.beleid",
                      orientation = "horizontal",
                      add_rank_list(
                        text = "Toevoegen aan rapport in onderstaande volgorde",
                        input_id = "Persberichten.beleid",
                        labels = list(
                          # Provinciebestuur --------------------------------------------
                          box(
                            width = 12,
                            collapsible = TRUE,
                            collapsed = TRUE,
                            title = "Provinciebestuur",
                            rank_list(
                              text = "Volgorde grafieken & tabellen per beleid:",
                              input_id = "test3",
                              labels = list(
                                fluidRow(
                                  column(width = 2, "Persberichten per maand"),
                                  column(width = 4, textInput("title", label = NULL, value = "Persberichten per kwartaal", placeholder = "Sectie titel"))
                                ),
                                fluidRow(
                                  column(width = 2, "Persberichten per deelbeleid"),
                                  column(width = 4, textInput("title", label = NULL, value = "Persberichten per maand", placeholder = "Sectie titel"))
                                )
                              )
                            )
                          ),
                          # Economie ------------------------------------------------------
                          box(
                            width = 12,
                            collapsible = TRUE,
                            collapsed = TRUE,
                            title = "Economie",
                            rank_list(
                              text = "Volgorde grafieken & tabellen per beleid:",
                              input_id = "test3",
                              labels = list(
                                fluidRow(
                                  column(width = 2, "Persberichten per maand"),
                                  column(width = 4, textInput("title", label = NULL, value = "Persberichten per kwartaal", placeholder = "Sectie titel"))
                                ),
                                fluidRow(
                                  column(width = 2, "Persberichten per deelbeleid"),
                                  column(width = 4, textInput("title", label = NULL, value = "Persberichten per maand", placeholder = "Sectie titel"))
                                )
                              )
                            )
                          ),
                          # Leefmilieu ----------------------------------------------------
                          box(
                            width = 12,
                            collapsible = TRUE,
                            collapsed = TRUE,
                            title = "Leefmilieu",
                            rank_list(
                              text = "Volgorde grafieken & tabellen per beleid:",
                              input_id = "test3",
                              labels = list(
                                fluidRow(
                                  column(width = 2, "Persberichten per maand"),
                                  column(width = 4, textInput("title", label = NULL, value = "Persberichten per kwartaal", placeholder = "Sectie titel"))
                                ),
                                fluidRow(
                                  column(width = 2, "Persberichten per deelbeleid"),
                                  column(width = 4, textInput("title", label = NULL, value = "Persberichten per maand", placeholder = "Sectie titel"))
                                )
                              )
                            )
                          ),
                          # Mobiliteit ----------------------------------------------------
                          box(
                            width = 12,
                            collapsible = TRUE,
                            collapsed = TRUE,
                            title = "Mobiliteit",
                            rank_list(
                              text = "Volgorde grafieken & tabellen per beleid:",
                              input_id = "test3",
                              labels = list(
                                fluidRow(
                                  column(width = 2, "Persberichten per maand"),
                                  column(width = 4, textInput("title", label = NULL, value = "Persberichten per kwartaal", placeholder = "Sectie titel"))
                                ),
                                fluidRow(
                                  column(width = 2, "Persberichten per deelbeleid"),
                                  column(width = 4, textInput("title", label = NULL, value = "Persberichten per maand", placeholder = "Sectie titel"))
                                )
                              )
                            )
                          ),
                          # Onderwijs en educatie -----------------------------------------
                          box(
                            width = 12,
                            collapsible = TRUE,
                            collapsed = TRUE,
                            title = "Onderwijs en Educatie",
                            rank_list(
                              text = "Volgorde grafieken & tabellen per beleid:",
                              input_id = "test3",
                              labels = list(
                                fluidRow(
                                  column(width = 2, "Persberichten per maand"),
                                  column(width = 4, textInput("title", label = NULL, value = "Persberichten per kwartaal", placeholder = "Sectie titel"))
                                ),
                                fluidRow(
                                  column(width = 2, "Persberichten per deelbeleid"),
                                  column(width = 4, textInput("title", label = NULL, value = "Persberichten per maand", placeholder = "Sectie titel"))
                                )
                              )
                            )
                          ),
                          # Ruimte --------------------------------------------------------
                          box(
                            width = 12,
                            collapsible = TRUE,
                            collapsed = TRUE,
                            title = "Ruimte",
                            rank_list(
                              text = "Volgorde grafieken & tabellen per beleid:",
                              input_id = "test3",
                              labels = list(
                                fluidRow(
                                  column(width = 2, "Persberichten per maand"),
                                  column(width = 4, textInput("title", label = NULL, value = "Persberichten per kwartaal", placeholder = "Sectie titel"))
                                ),
                                fluidRow(
                                  column(width = 2, "Persberichten per deelbeleid"),
                                  column(width = 4, textInput("title", label = NULL, value = "Persberichten per maand", placeholder = "Sectie titel"))
                                )
                              )
                            )
                          ),
                          # Vrije tijd ----------------------------------------------------
                          box(
                            width = 12,
                            collapsible = TRUE,
                            collapsed = TRUE,
                            title = "Vrije Tijd",
                            rank_list(
                              text = "Volgorde grafieken & tabellen per beleid:",
                              input_id = "test3",
                              labels = list(
                                fluidRow(
                                  column(width = 2, "Persberichten per maand"),
                                  column(width = 4, textInput("title", label = NULL, value = "Persberichten per kwartaal", placeholder = "Sectie titel"))
                                ),
                                fluidRow(
                                  column(width = 2, "Persberichten per deelbeleid"),
                                  column(width = 4, textInput("title", label = NULL, value = "Persberichten per maand", placeholder = "Sectie titel"))
                                )
                              )
                            )
                          )
                        )
                      ),
                      add_rank_list(
                        text = "Weglaten uit rapport",
                        labels = NULL
                      )
                    )
                  ),
                  # Verzender -----------------------------------------------------
                  box(
                    width = 12,
                    collapsible = TRUE,
                    collapsed = TRUE,
                    title = "Verzender"
                  ),
                  # Type ----------------------------------------------------------
                  box(
                    width = 12,
                    collapsible = TRUE,
                    collapsed = TRUE,
                    title = "Type"
                  )
                )
              )
            )
          )
        ),
        add_rank_list(
          text = "Weglaten uit rapport",
          input_id = NULL,
          labels = NULL
        )
      )
    ),
    
    # Persreturn --------------------------------------------------------
    box(
      width = 12,
      title = "Persreturn"
    ),
    verbatimTextOutput("results")
  )
)