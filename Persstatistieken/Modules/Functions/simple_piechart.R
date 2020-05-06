###############################################################################
# FUNCTION: Persberichten - Piechart
###############################################################################

# Calculate Piechart ==========================================================
simple_piechart <- function(Id, data, Fill, visual, title, Xtitle, Ytitle, Xlabels, legend, colors) {
  
  Y <- reactive(
        if (visual == "Aantal") {
          # Persreturn
          if(Id == "return.beleid.alg" || Id == "return.beleid.beleid" || Id == "return.medium") {
            data()$Aantal
          } 
          # Persberichten
          else {
            data()$Persberichten
          }
        }
      )
  
  plot <- reactive(
    ggplot(data(), aes(x = "", 
                       y  = (if(visual == "Aantal") {Y()} else {Procentueel}),
                       fill = data()[[Fill]])) +
      geom_bar(width = 1, size = 1, color = "white", stat = 'identity') +
      coord_polar("y", start = 0, direction = -1) +
      geom_text(aes(label = if(visual == "Aantal") {Y()} else {percent(Procentueel, accuracy = 0.1, scale = 1)}),
                position = position_stack(vjust = 0.5)) +
      theme_minimal() +
      ggtitle(title) +
      xlab(Xtitle) +
      ylab(Ytitle) +
      labs(fill = Fill) +
      (if (length(levels(data()[[Fill]])) <= 14) {scale_fill_manual(values=colors())}) +
      (if (visual == "Procent") {ylim(c(0,100))}) +
      (if (Xlabels) {theme(axis.text.x = element_text(angle = 45, hjust = 1))} else {theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())}) +
      (if (!(legend)) {theme(legend.position = "none")})
  )
  return(plot())
}
# =============================================================================