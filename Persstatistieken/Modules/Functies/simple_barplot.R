###############################################################################
# FUNCTION: Persberichten - Barplot
###############################################################################

# Calculate barplot ===========================================================
simple_barplot <- function(data, visual, title, Xtitle, Ytitle, Xlabels, legend, colors) {
  plot <- reactive(
    ggplot(data(), aes(x = Maand, 
                       y  = (if (visual == "Aantal") {Persberichten} else {Procentueel}), 
                       fill = Maand)) +
      geom_bar(position = "dodge", stat = 'identity') +
      geom_text(aes(label = if(visual == "Aantal") {Persberichten} else {percent(Procentueel, accuracy = 0.1, scale = 1)}),
                position = position_dodge(0.9), 
                vjust=0) +
      theme_bw() +
      ggtitle(title) +
      xlab(Xtitle) +
      ylab(Ytitle) +
      scale_fill_manual(values=colors) + 
      (if (visual == "Procentueel") {ylim(c(0,100))}) +
      (if (Xlabels) {theme(axis.text.x = element_text(angle = 45, hjust = 1))} else {theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())}) +
      (if (!(legend)) {theme(legend.position = "none")})
  )
  return(plot())
}
# =============================================================================