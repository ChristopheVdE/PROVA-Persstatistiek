###############################################################################
# FUNCTION: Persberichten - Barplot
###############################################################################

# Calculate barplot ===========================================================
simple_barplot <- function(Id, data, Xaxis, Fill, visual, title, Xtitle, Ytitle, Xlabels, legend, colors) {
  
  Y <- reactive(
    if (visual == "Aantal") {
      # Persreturn
      if(grepl("return", Id)) {
        data()$Aantal
      } 
      # Persberichten - Persconferenties
      else if (grepl("conferentie", Id)) {
        data()$Persconferenties
      # Persberichten
      } else {
        data()$Persberichten
      }
    }
  )
  #browser()
  
  plot <- reactive(
    ggplot(data(), aes(x = data()[[Xaxis]], 
                       y  = (if(visual == "Aantal") {Y()} else {Procentueel}), 
                       fill = data()[[Fill]])) +
      geom_bar(position = "dodge", stat = 'identity') +
      geom_text(aes(label = if(visual == "Aantal") {Y()} else {percent(Procentueel, accuracy = 0.1, scale = 1)}),
                position = position_dodge(0.9), 
                vjust=0) +
      theme_bw() +
      ggtitle(title) +
      xlab(Xtitle) +
      ylab(Ytitle) +
      labs(fill = Fill) +
      (if (length(levels(as.factor(data()[[Fill]]))) <= 14) {scale_fill_manual(values=colors())}) +
      (if (visual == "Procent") {ylim(c(0,100))}) +
      (if (Xlabels) {theme(axis.text.x = element_text(angle = 45, hjust = 1))} else {theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())}) +
      (if (!(legend)) {theme(legend.position = "none")})
  )
  return(plot())
}
# =============================================================================