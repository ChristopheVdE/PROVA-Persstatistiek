###############################################################################
# FUNCTION: Persberichten - Piechart
###############################################################################

# Calculate barplot ===========================================================
simple_piechart <- function(data, Fill, visual, title, Xtitle, Ytitle, Xlabels, legend, colors) {
  plot <- reactive(
    ggplot(data(), aes(x = "", 
                       y  = (if (visual == "Aantal") {Persberichten} else {Procentueel}), 
                       fill = data()[[Fill]])) +
      geom_bar(width = 1, size = 1, color = "white", stat = 'identity') +
      coord_polar("y", start = 0, direction = -1) +
      geom_text(aes(label = if(visual == "Aantal") {Persberichten} else {percent(Procentueel, accuracy = 0.1, scale = 1)}),
                position = position_stack(vjust = 0.5)) +
      theme_minimal() +
      ggtitle(title) +
      xlab(Xtitle) +
      ylab(Ytitle) +
      labs(fill = Fill) +
      scale_fill_manual(values=colors) + 
      (if (visual == "Procentueel") {ylim(c(0,100))}) +
      (if (Xlabels) {theme(axis.text.x = element_text(angle = 45, hjust = 1))} else {theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())}) +
      (if (!(legend)) {theme(legend.position = "none")})
  )
  return(plot())
}
# =============================================================================